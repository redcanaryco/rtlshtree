#include "tlshtree_types.h"

using namespace Rcpp;

unsigned char pearson_hash(unsigned char salt,
                           unsigned char c1,
                           unsigned char c2,
                           unsigned char c3) {
    unsigned char h = 0;
    h = v_table[h ^ salt];
    h = v_table[h ^ c1];
    h = v_table[h ^ c2];
    h = v_table[h ^ c3];
    return h;
}

unsigned char swap_byte(unsigned char in) {
    unsigned char out;
    out = ((in & 0xf0) >> 4) & 0x0f;
    out |= ((in & 0x0f) << 4) & 0xf0;
    return out;
}

void reverse(unsigned char buf[]) {
    for (int i = 0, j = WINDOW_LENGTH - 1; i < j; i++, j--) {
        unsigned char temp = buf[i];
        buf[i] = buf[j];
        buf[j] = temp;
    }
}

unsigned char l_value_calc(int length) {
    unsigned char l;

    if (length <= 656) {
        l = (unsigned char)floor(log((double)length) / log1_5);
    } else if (length <= 3199) {
        l = (unsigned char)(floor(log((double)length) / log1_3 - 8.71777));
    } else {
        l = (unsigned char)(floor(log((double)length) / log1_1 - 62.5472));
    }

    return l;
}

unsigned int partition(unsigned int buf[],
                       unsigned int left,
                       unsigned int right) {
    if (left == right) {
        return left;
    }

    if (left + 1 == right) {
        if (buf[left] > buf[right]) {
            unsigned int temp = buf[right];
            buf[right] = buf[left];
            buf[left] = temp;
        }

        return left;
    }

    unsigned int ret = left;
    unsigned int pivot = (left + right) >> 1;
    unsigned int val = buf[pivot];

    buf[pivot] = buf[right];
    buf[right] = val;

    for (unsigned int i = left; i < right; i++) {
        if (buf[i] < val) {
            unsigned int temp = buf[i];
            buf[i] = buf[ret];
            buf[ret] = temp;
            ret++;
        }
    }

    buf[right] = buf[ret];
    buf[ret] = val;

    return ret;
}

void quartile_points(unsigned int buckets[],
                     unsigned int* q1,
                     unsigned int* q2,
                     unsigned int* q3) {
    unsigned int spl = 0;
    unsigned int spr = 0;

    unsigned int p1 = (EFF_BUCKETS / 4) - 1;
    unsigned int p2 = (EFF_BUCKETS / 2) - 1;
    unsigned int p3 = EFF_BUCKETS - (EFF_BUCKETS / 4) - 1;

    unsigned int end = EFF_BUCKETS - 1;

    unsigned int bucket_copy[EFF_BUCKETS];

    for (int i = 0; i < EFF_BUCKETS; i++) {
        bucket_copy[i] = buckets[i];
    }

    unsigned int short_cut_left[EFF_BUCKETS];
    unsigned int short_cut_right[EFF_BUCKETS];

    unsigned int l = 0;
    unsigned int r = 0;

    for (l = 0, r = end;;) {
        unsigned int ret = partition(bucket_copy, l, r);

        if (ret > p2) {
            r = ret - 1;
            short_cut_right[spr] = ret;
            spr++;
        } else if (ret < p2) {
            l = ret + 1;
            short_cut_left[spl] = ret;
            spl++;
        } else {
            *q2 = bucket_copy[p2];
            break;
        }
    }

    short_cut_left[spl] = p2 - 1;
    short_cut_right[spr] = p2 + 1;

    for (unsigned int i = 0, l = 0; i <= spl; i++) {
        r = short_cut_left[i];
        if (r > p1) {
            while (1) {
                unsigned int ret = partition(bucket_copy, l, r);
                if (ret > p1) {
                    r = ret - 1;
                } else if (ret < p1) {
                    l = ret + 1;
                } else {
                    *q1 = bucket_copy[p1];
                    break;
                }
            }
            break;
        } else if (r < p1) {
            l = r;
        } else {
            *q1 = bucket_copy[p1];
            break;
        }
    }

    for (unsigned int i = 0, r = end; i <= spr; i++) {
        l = short_cut_right[i];
        if (l < p3) {
            while (1) {
                unsigned int ret = partition(bucket_copy, l, r);
                if (ret > p3) {
                    r = ret - 1;
                } else if (ret < p3) {
                    l = ret + 1;
                } else {
                    *q3 = bucket_copy[p3];
                    break;
                }
            }
            break;
        } else if (l > p3) {
            r = l;
        } else {
            *q3 = bucket_copy[p3];
            break;
        }
    }
}

struct ChunkState {
    unsigned int buckets[NUM_BUCKETS];
    unsigned char chunk[WINDOW_LENGTH];
    unsigned int file_size;
    unsigned char checksum;
    unsigned char chunk3[3];
};

int chunk_state_init(struct ChunkState* state, char buf[], int buf_size) {
    memcpy(state->chunk, buf, WINDOW_LENGTH);
    reverse(state->chunk);

    state->file_size = WINDOW_LENGTH - 1;
    state->checksum = 0;

    for (int i = WINDOW_LENGTH; i < buf_size; i++) {
        // start process
        //
        state->chunk3[0] = state->chunk[0];
        state->chunk3[1] = state->chunk[1];
        state->chunk3[2] = state->checksum;

        state->checksum = pearson_hash(0, state->chunk3[0], state->chunk3[1],
                                       state->chunk3[2]);

        state->chunk3[2] = state->chunk[2];
        state->buckets[pearson_hash(SALT[0], state->chunk3[0], state->chunk3[1],
                                    state->chunk3[2])]++;

        state->chunk3[2] = state->chunk[3];
        state->buckets[pearson_hash(SALT[1], state->chunk3[0], state->chunk3[1],
                                    state->chunk3[2])]++;

        state->chunk3[1] = state->chunk[2];
        state->buckets[pearson_hash(SALT[2], state->chunk3[0], state->chunk3[1],
                                    state->chunk3[2])]++;

        state->chunk3[2] = state->chunk[4];
        state->buckets[pearson_hash(SALT[3], state->chunk3[0], state->chunk3[1],
                                    state->chunk3[2])]++;

        state->chunk3[1] = state->chunk[1];
        state->buckets[pearson_hash(SALT[4], state->chunk3[0], state->chunk3[1],
                                    state->chunk3[2])]++;

        state->chunk3[1] = state->chunk[3];
        state->buckets[pearson_hash(SALT[5], state->chunk3[0], state->chunk3[1],
                                    state->chunk3[2])]++;

        unsigned char temp1 = state->chunk[0];
        unsigned char temp2 = state->chunk[1];
        unsigned char temp3 = state->chunk[2];
        unsigned char temp4 = state->chunk[3];
        state->chunk[1] = temp1;
        state->chunk[2] = temp2;
        state->chunk[3] = temp3;
        state->chunk[4] = temp4;
        // end process

        state->chunk[0] = (unsigned char)buf[i];
        state->file_size++;
    }

    return SUCCESS;
}

struct Tlsh* tlsh_init_buf(char* buf, int buf_size) {
    struct Tlsh* tlsh = (struct Tlsh*)calloc(1, sizeof(struct Tlsh));

    if (buf_size < 50 || buf == NULL) {
        tlsh->complete = 0;
        return tlsh;
    }

    // ChunkState is of a fixed size, primarily sized by the NUM_BUCKETS.
    struct ChunkState* state =
        (struct ChunkState*)alloca(sizeof(struct ChunkState));
    memset(state, 0, sizeof(struct ChunkState));

    chunk_state_init(state, buf, buf_size);

    tlsh->checksum = state->checksum;
    tlsh->l_value = l_value_calc(buf_size);

    unsigned int q1 = 0;
    unsigned int q2 = 0;
    unsigned int q3 = 0;

    unsigned int temp_buckets[NUM_BUCKETS] = {0};

    for (int i = 0; i < NUM_BUCKETS; i++) {
        temp_buckets[i] = state->buckets[i];
    }

    quartile_points(temp_buckets, &q1, &q2, &q3);

    if (q3 == 0) {
        tlsh->complete = 0;
        return tlsh;
    }

    tlsh->q1_ratio = (q1 * 100 / q3) % 16;
    tlsh->q2_ratio = (q2 * 100 / q3) % 16;
    tlsh->q_ratio = ((tlsh->q1_ratio & 0xf) << 4) | (tlsh->q2_ratio & 0xf);

    for (int i = 0; i < CODE_SIZE; i++) {
        unsigned char h = 0;
        for (int j = 0; j < 4; j++) {
            unsigned int k = state->buckets[4 * i + j];
            if (q3 < k) {
                h += 3 << (j * 2);
            } else if (q2 < k) {
                h += 2 << (j * 2);
            } else if (q1 < k) {
                h += 1 << (j * 2);
            }
        }

        tlsh->code[(CODE_SIZE - 1) - i] = h;
    }

    tlsh->complete = 1;
    return tlsh;
}

struct Tlsh* tlsh_init_str(char* buf, int buf_len) {
    char* temp_buf = (char*)alloca(HASH_LEN);
    struct Tlsh* tlsh = (struct Tlsh*)calloc(1, sizeof(struct Tlsh));

    if (buf_len < HASH_LEN || buf == NULL) {
        tlsh->complete = 0;
        return tlsh;
    }

    if (buf[0] == 'T' || buf[0] == 't') {
        if (buf_len != 72) {
            tlsh->complete = 0;
            return tlsh;
        }

        memcpy(temp_buf, &buf[2], HASH_LEN);
    } else {
        memcpy(temp_buf, buf, HASH_LEN);
    }

    char temp_str[3] = {0};

    temp_str[0] = temp_buf[0];
    temp_str[1] = temp_buf[1];
    tlsh->checksum = swap_byte(strtol(temp_str, NULL, 16));

    temp_str[0] = temp_buf[2];
    temp_str[1] = temp_buf[3];
    tlsh->l_value = swap_byte(strtol(temp_str, NULL, 16));

    temp_str[0] = temp_buf[4];
    temp_str[1] = temp_buf[5];
    tlsh->q_ratio = strtol(temp_str, NULL, 16);

    tlsh->q1_ratio = (tlsh->q_ratio >> 4) & 0xf;
    tlsh->q2_ratio = tlsh->q_ratio & 0xf;

    for (int i = 6, j = 0; i < buf_len; i += 2, j++) {
        temp_str[0] = temp_buf[i];
        temp_str[1] = temp_buf[i + 1];
        tlsh->code[j] = strtol(temp_str, NULL, 16);
    }

    if (tlsh->q1_ratio == 0 && tlsh->q2_ratio == 0) {
        tlsh->complete = 0;
    }

    tlsh->complete = 1;
    return tlsh;
}

char* tlsh_to_string(struct Tlsh* tlsh, int* buf_len) {
    char* buf = (char*)calloc(1, (3 + CODE_SIZE) * 2 + 1);
    if (tlsh == NULL || tlsh->complete == 0) {
        strncpy(buf, "tnull\0", 6);
        *buf_len = 6;
        return buf;
    }

    *buf_len = (3 + CODE_SIZE) * 2 + 1;

    if (tlsh == NULL) {
        fprintf(stderr, "tlsh_to_string received a null tlsh struct\n");
        return buf;
    }

    snprintf(buf, 3, "%0.2x", swap_byte(tlsh->checksum));
    snprintf(&buf[2], 3, "%0.2x", swap_byte(tlsh->l_value));
    snprintf(&buf[4], 3, "%0.2x", tlsh->q_ratio);

    for (int i = 0; i < CODE_SIZE; i++) {
        snprintf(&buf[6 + (i * 2)], 3, "%0.2x", tlsh->code[i]);
    }

    return buf;
}

unsigned int mod_diff(unsigned char x, unsigned char y, unsigned int R) {
    unsigned int dl = 0;
    unsigned int dr = 0;

    if (y > x) {
        dl = y - x;
        dr = x + R - y;
    } else {
        dl = x - y;
        dr = y + R - x;
    }

    if (dl > dr) {
        return dr;
    }
    return dl;
}

unsigned int digest_distance(unsigned char x[], unsigned char y[]) {
    unsigned int diff = 0;
    for (int i = 0; i < CODE_SIZE; i++) {
        diff += bit_pair_diff_table[x[i]][y[i]];
    }

    return diff;
}

int _tlsh_diff(struct Tlsh* a, struct Tlsh* b) {
    if (a == NULL || b == NULL) {
        return -1;
    }

    unsigned int diff = 0;

    unsigned int q1_diff = mod_diff(a->q1_ratio, b->q1_ratio, 16);
    if (q1_diff <= 1) {
        diff += q1_diff;
    } else {
        diff += (q1_diff - 1) * 12;
    }

    unsigned int q2_diff = mod_diff(a->q2_ratio, b->q2_ratio, 16);
    if (q2_diff <= 1) {
        diff += q2_diff;
    } else {
        diff += (q2_diff - 1) * 12;
    }

    if (a->checksum != b->checksum) {
        diff++;
    }

    diff += digest_distance(a->code, b->code);

    return diff;
}

struct SplitResult {
    struct Tlsh** left;
    int left_len;
    struct Tlsh** right;
    int right_len;
    int split_point;
    struct Tlsh* key;
};

struct SplitResult* tlsh_split(int leaf_size,
                               struct Tlsh** input,
                               int input_len) {
    if (input_len < leaf_size) {
        return NULL;
    }

    int* left_list = (int*)calloc(1, sizeof(int) * input_len);
    int left_index = 0;
    int* right_list = (int*)calloc(1, sizeof(int) * input_len);
    int right_index = 0;

    struct Tlsh* split_point_key;

    // minimum size of a leaf node
    int min_size = (int)(input_len * 0.3);
    int split_point = 5;
    int jump_size = 5;

    for (int i = 0; i < input_len; i++) {
        // clear the lists and reset the index counters
        memset(left_list, 0, input_len);
        left_index = 0;
        memset(right_list, 0, input_len);
        right_index = 0;

        // set the first split point key
        split_point_key = input[i];

        for (int j = 0; j < input_len; j++) {
            // don't do the comparison if we're at the same index. Rather add it
            // to the left side.
            if (j == i) {
                left_list[left_index] = j;
                left_index++;
                continue;
            }

            // get the distance between the two keys
            int diff = _tlsh_diff(split_point_key, input[j]);

            if (diff <= split_point) {
                left_list[left_index] = j;
                left_index++;
            } else {
                right_list[right_index] = j;
                right_index++;
            }
        }

        if (left_index > min_size && right_index > min_size) {
            struct SplitResult* result =
                (struct SplitResult*)calloc(1, sizeof(struct SplitResult));
            result->left =
                (struct Tlsh**)calloc(left_index, sizeof(struct Tlsh*));
            result->left_len = left_index;
            result->right =
                (struct Tlsh**)calloc(right_index, sizeof(struct Tlsh*));
            result->right_len = right_index;
            result->split_point = split_point;
            result->key = split_point_key;

            for (int i = 0; i < left_index; i++) {
                result->left[i] = input[left_list[i]];
            }

            for (int i = 0; i < right_index; i++) {
                result->right[i] = input[right_list[i]];
            }

            return result;
        }

        split_point += jump_size;
    }

    // could not find appropriate split point, use current input as a leaf node
    return NULL;
}

const int IS_LEAF = 1;
const int IS_NOT_LEAF = 0;

void node_free(struct Node* node) {
    if (node == NULL) {
        return;
    }

    if (node->is_leaf == IS_LEAF) {
        for (int i = 0; i < node->size; i++) {
            free(node->items[i]);
        }

        free(node->items);
        free(node);
        return;
    }

    // we don't need to free node->key as it will be freed in the for loop
    // above.

    node_free(node->left);
    node_free(node->right);

    free(node);
}

struct Node* tlsh_tree_insert(int leaf_size,
                              struct Tlsh** input,
                              int input_len) {
    if (input_len < leaf_size) {
        struct Node* node = (struct Node*)calloc(1, sizeof(struct Node));
        node->is_leaf = IS_LEAF;
        node->items = input;
        node->size = input_len;
        node->max_size = input_len;
        return node;
    }

    struct SplitResult* result = tlsh_split(leaf_size, input, input_len);

    if (result == NULL) {
        struct Node* node = (struct Node*)calloc(1, sizeof(struct Node));
        node->is_leaf = IS_LEAF;
        node->items = input;
        node->size = input_len;
        node->max_size = input_len;

        return node;
    }

    struct Node* left_node =
        tlsh_tree_insert(leaf_size, result->left, result->left_len);
    struct Node* right_node =
        tlsh_tree_insert(leaf_size, result->right, result->right_len);

    struct Node* node = (struct Node*)calloc(1, sizeof(struct Node));
    node->is_leaf = IS_NOT_LEAF;
    node->left = left_node;
    node->right = right_node;
    node->split_point = result->split_point;
    node->key = result->key;

    return node;
}

int _tlsh_tree_delete(struct Node* node, struct Tlsh* key) {
    if (node == NULL) {
        return FAILURE;
    }

    if (node->is_leaf == IS_LEAF) {
        // first loop removes the element and sets the start point for shifting
        // the remaining elements
        int start_point = -1;

        // check if there are any keys with a distance of 0
        for (int i = 0; i < node->size; i++) {
            if (_tlsh_diff(node->items[i], key) == 0) {
                start_point = i;
                free(node->items[i]);
                node->items[i] = NULL;
                break;
            }
        }

        // make sure we have a start point
        if (start_point < 0) {
            return FAILURE;
        }

        // start at the next element after the start point
        for (int i = start_point + 1; i < node->size; i++) {
            node->items[i - 1] = node->items[i];
        }

        // set the last item to NULL because it was shifted back one
        node->items[node->size - 1] = NULL;
        node->size--;

        return SUCCESS;
    }

    if (_tlsh_diff(node->key, key) <= node->split_point) {
        return _tlsh_tree_delete(node->left, key);
    } else {
        return _tlsh_tree_delete(node->right, key);
    }
}

int _tlsh_tree_add(struct Node* node, struct Tlsh* key) {
    if (node == NULL) {
        return FAILURE;
    }

    if (node->is_leaf == IS_LEAF) {
        if (node->size < node->max_size - 1) {
            node->size++;
            node->items[node->size] = key;
            return SUCCESS;
        }

        // check if there are any keys with a distance of 0
        for (int i = 0; i < node->size; i++) {
            if (_tlsh_diff(node->items[i], key) == 0) {
                return SUCCESS;
            }
        }

        struct Tlsh** new_arr =
            (struct Tlsh**)calloc(node->max_size * 2, sizeof(struct Tlsh*));
        for (int i = 0; i < node->size; i++) {
            new_arr[i] = node->items[i];
        }
        node->items = new_arr;

        node->max_size = node->max_size * 2;
        node->size++;
        node->items[node->size] = key;
        return SUCCESS;
    }

    if (_tlsh_diff(node->key, key) <= node->split_point) {
        return _tlsh_tree_add(node->left, key);
    } else {
        return _tlsh_tree_add(node->right, key);
    }
}

struct TlshMatch* _tlsh_lookup(struct Tlsh* key,
                               struct Node* node,
                               int threshold,
                               int* matches) {
    if (node == NULL) {
        fprintf(stderr, "_tlsh_lookup failed, passed in null node\n");
        *matches = 0;
        return NULL;
    }

    if (key == NULL) {
        fprintf(stderr, "_tlsh_lookup failed, passed in null key\n");
        *matches = 0;
        return NULL;
    }

    if (threshold < 0) {
        threshold = 0;
    }

    if (node->is_leaf == IS_LEAF) {
        int match_count = 0;
        struct TlshMatch* cur = NULL;

        for (int i = 0; i < node->size; i++) {
            int distance = _tlsh_diff(key, node->items[i]);
            if (distance <= threshold) {
                match_count++;
                struct TlshMatch* temp =
                    (struct TlshMatch*)calloc(1, sizeof(struct TlshMatch));
                int len;
                char* temp_str = tlsh_to_string(node->items[i], &len);
                temp->str = temp_str;
                temp->size = len;
                temp->distance = distance;
                temp->next = NULL;

                if (cur == NULL) {
                    cur = temp;
                } else {
                    temp->next = cur;
                    cur = temp;
                }
            }
        }

        *matches = match_count;
        return cur;
    }

    if (_tlsh_diff(key, node->key) <= node->split_point) {
        return _tlsh_lookup(key, node->left, threshold, matches);
    } else {
        return _tlsh_lookup(key, node->right, threshold, matches);
    }
}

//' Compare two sets of TLSH hashes, return the count of how many
//' similar hashes there are.
//'
//' @param xs list of TLSH hashes
//' @param xs2 list of TLSH hashes
//' @return integer showing the union count between the two sets.
//' @description calculates the union of two TLSH lists given some
//'   threshold.
//' @usage
//' out <- tlsh_n_union(tlsh_set_a, tlsh_set_b)
// [[Rcpp::export]]
int tlsh_n_union(int threshold, std::vector<std::string> xs, std::vector<std::string> xs2) {

    std::vector<Tlsh*> list1;
    std::vector<Tlsh*> list2;

    if (threshold < 0) {
        threshold = 0;
    }

    for (int i = 0; i < xs.size(); i++) {
        std::string s(xs.at(i));
        struct Tlsh* temp = tlsh_init_str(s.data(), s.size());

        if (temp->complete == 0) {
            free(temp);
            continue;
        }

        list1.push_back(temp);
    }

    for (int i = 0; i < xs2.size(); i++) {
        std::string s(xs.at(i));
        struct Tlsh* temp = tlsh_init_str(s.data(), s.size());

        if (temp->complete == 0) {
            free(temp);
            continue;
        }

        list2.push_back(temp);
    }

    int output = 0;

    for (int i = 0; i < list1.size(); i++) {
        struct Tlsh* a = list1.at(i);
        
        for (int j = 0; j < list2.size(); j++) {
            struct Tlsh* b = list2.at(j);

            if (_tlsh_diff(a, b) <= threshold) {
                output++;
            }
        }
    }

    for (int i = 0; i < list1.size(); i++) {
        free(list1.at(i));
    }

    for (int i = 0; i < list2.size(); i++) {
        free(list2.at(i));
    }

    return output;
}

//' Calculate the distance between two TLSH hashes
//'
//' @param a TLSH hash string
//' @param b TLSH hash string
//' @return integer representing the distance between the two hashes
//' @description negative values indicate one of the two hashes is invalid
//' @usage
//' out <- tlsh_diff(tlsh_a, tlsh_b)
// [[Rcpp::export]]
int tlsh_diff(std::string a, std::string b) {
    Tlsh* tlsh_a = tlsh_init_str(a.data(), a.size());
    if (tlsh_a->complete == 0) {
        Rcpp::Rcerr << "invalid TLSH value " << a << "\n";
        free(tlsh_a);
        return -1;
    }

    Tlsh* tlsh_b = tlsh_init_str(b.data(), b.size());
    if (tlsh_b->complete == 0) {
        Rcpp::Rcerr << "invalid TLSH value " << b << "\n";
        free(tlsh_b);
        return -1;
    }

    int output = _tlsh_diff(tlsh_a, tlsh_b);

    free(tlsh_a);
    free(tlsh_b);

    return output;
}

TlshTree::TlshTree(struct Node* n, int s) {
    size = s;
    node = n;
}

TlshTree::~TlshTree() {
    node_free(this->node);
}

//' Returns simple information about the tree
//'
//' @param tlshtree value from creating with tlsh_tree_new
//' @return List object with information about the tree
//' @usage
//' out <- tlsh_tree_info(tree)
// [[Rcpp::export]]
Rcpp::List tlsh_tree_info(Rcpp::XPtr<TlshTree> tree) {
    Rcpp::List list;
    Rcpp::Rcerr << tree->size << "\n";
    list["size"] = tree->size;
    return list;
}

//' Creates a new TLSH tree given the vector of TLSH hashes
//'
//' @param leaf_size is the set size of each leaf, minimum size is 20
//' @param xs is a vector of TLSH hashes
//' @return tlshtree pointer
//' @usage
//' out <- tlsh_tree_new(hashes, 20)
// [[Rcpp::export]]
Rcpp::XPtr<TlshTree> tlsh_tree_new(int leaf_size, Rcpp::StringVector xs) {
    struct Tlsh** hashes =
        (struct Tlsh**)calloc(xs.size(), sizeof(struct Tlsh*));

    int index = 0;

    if (leaf_size <= 0) {
        leaf_size = 20;
    }

    std::set<std::string> dups;

    for (int i = 0; i < xs.size(); i++) {
        std::string s(xs(i));

        // check if the key has already been added, skip if so
        if (dups.count(s) > 0) {
            continue;
        } else {
            dups.insert(s);
        }

        struct Tlsh* temp = tlsh_init_str(s.data(), s.size());

        if (temp->complete == 0) {
            free(temp);
            continue;
        }

        hashes[index] = temp;
        index++;
    }

    struct Node* node = tlsh_tree_insert(leaf_size, hashes, index);

    Rcpp::XPtr<TlshTree> ptr(new TlshTree(node, index), true);
    return ptr;
}

//' For each TLSH hash, find all matches it has within the tree, given
//' some threshold
//'
//' @param tlshtree value from creating with tlsh_tree_new
//' @param threshold is the maximum distance to be considered a pair
//' @param xs is a vector of TLSH hashes
//' @return a list with all the TLSH hash matches for each key in
//' the input vector
//' @usage
//' out <- tlsh_tree_matches(10, hashes)
// [[Rcpp::export]]
Rcpp::List tlsh_tree_matches(Rcpp::XPtr<TlshTree> tree,
                             int threshold,
                             std::vector<std::string> keys) {
    Rcpp::List list = Rcpp::List::create();

    if (tree->node == NULL) {
        Rcpp::Rcerr << "null tree provided to tlsh_tree_n_matches\n";
        return list;
    }

    for (int i = 0; i < keys.size(); i++) {
        Rcpp::StringVector sv;
        struct Tlsh* tlsh = tlsh_init_str(keys.at(i).data(), HASH_LEN);

        if (tlsh->complete == 0) {
            free(tlsh);
            continue;
        }

        int matches;
        struct TlshMatch* head =
            _tlsh_lookup(tlsh, tree->node, threshold, &matches);
        if (head == NULL) {
            continue;
        }

        for (struct TlshMatch *cur = head, *temp = NULL; cur != NULL;) {
            sv.push_back(std::string(cur->str));
            free(cur->str);
            temp = cur;
            cur = cur->next;
            free(temp);
        }

        int tlsh_len;
        std::string key(tlsh_to_string(tlsh, &tlsh_len));
        list[key] = sv;
    }

    return list;
}

//' For each TLSH hash, count how many matches it has within the tree, given
//' some threshold
//'
//' @param tlshtree value from creating with tlsh_tree_new
//' @param threshold is the maximum distance to be considered a pair
//' @param xs is a vector of TLSH hashes
//' @return a count of how many pairs are in the vector
//' @usage
//' out <- tlsh_tree_n_matches(10, hashes)
// [[Rcpp::export]]
Rcpp::IntegerVector tlsh_tree_n_matches(Rcpp::XPtr<TlshTree> tree,
                                        int threshold,
                                        std::vector<std::string> keys) {
    Rcpp::IntegerVector iv;

    if (tree->node == NULL) {
        Rcpp::Rcerr << "null tree provided to tlsh_tree_n_matches\n";
        return iv;
    }

    for (int i = 0; i < keys.size(); i++) {
        struct Tlsh* tlsh = tlsh_init_str(keys.at(i).data(), keys.at(i).size());

        if (tlsh->complete == 0) {
            free(tlsh);
            iv.push_back(0);
            continue;
        }

        int matches;
        struct TlshMatch* head =
            _tlsh_lookup(tlsh, tree->node, threshold, &matches);
        iv.push_back(matches);

        if (head == NULL) {
            continue;
        }

        for (struct TlshMatch *cur = head, *temp = NULL; cur != NULL;) {
            if (cur->str != NULL) {
                free(cur->str);
            }
            temp = cur;
            cur = cur->next;
            free(temp);
        }

        free(tlsh);
    }

    return iv;
}

//' Given a vector of tlsh hashes, return a count of all the pairwise matches
//' given some threshold
//'
//' @param threshold is the maximum distance to be considered a pair
//' @param xs is a vector of TLSH hashes
//' @return a count of how many pairs are in the vector
//' @usage
//' out <- tlsh_n_pairs(10, hashes)
// [[Rcpp::export]]
unsigned int tlsh_n_pairs(unsigned int threshold, Rcpp::StringVector xs) {
    int matches = 0;

    std::vector<Tlsh*> hashes;

    for (int i = 0; i < xs.size(); i++) {
        std::string s(xs(i));
        struct Tlsh* temp = tlsh_init_str(s.data(), s.size());

        if (temp->complete == 0) {
            free(temp);
            continue;
        }

        hashes.push_back(temp);
    }

    for (int i = 0; i < hashes.size(); i++) {
        for (int j = i + 1; j < hashes.size(); j++) {
            Tlsh* a = hashes.at(i);
            Tlsh* b = hashes.at(j);
            if (_tlsh_diff(a, b) <= threshold) {
                matches++;
            }
        }
    }

    for (int i = 0; i < hashes.size(); i++) {
        free(hashes.at(i));
    }

    return matches;
}

//' Deletes a TLSH hash from an existing tree.
//'
//' @param tlshtree value from creating with tlsh_tree_new
//' @param key_str is the TLSH hash to be deleted
//' @return a pointer to the tlshtree
//' @usage
//' tree <- tlsh_tree_delete(tree, tlsh_hash)
// [[Rcpp::export]]
Rcpp::XPtr<TlshTree> tlsh_tree_delete(Rcpp::XPtr<TlshTree> tree,
                                      std::string key_str) {
    struct Tlsh* key = tlsh_init_str(key_str.data(), key_str.size());

    if (key->complete == 0) {
        free(key);
        return tree;
    }

    if (_tlsh_tree_delete(tree->node, key) == FAILURE) {
        Rcpp::Rcerr << "error deleting key from the tree\n";
        return tree;
    }

    tree->size--;
    return tree;
}

//' Adds a TLSH hash to an existing tree.
//'
//' @param tlshtree value from creating with tlsh_tree_new
//' @param key_str is the TLSH hash to be added
//' @return a pointer to the tlshtree
//' @description Adds a TLSH hash to an existing tree. This will not
//' rebuild the structure of the existing tree, only the hash will be added to
//' the appropriate leaf. The
//' underlying point value of the tree will not change, but this function
//' returns that pointer value regardless, so the return value can be
//' ignored for most use cases.
//' @usage
//' tree <- tlsh_tree_add(tree, tlsh_hash)
// [[Rcpp::export]]
Rcpp::XPtr<TlshTree> tlsh_tree_add(Rcpp::XPtr<TlshTree> tree,
                                   std::string key_str) {
    struct Tlsh* key = tlsh_init_str(key_str.data(), key_str.size());

    if (key->complete == 0) {
        free(key);
        return tree;
    }

    if (_tlsh_tree_add(tree->node, key) == FAILURE) {
        Rcpp::Rcerr << "error adding key to tree\n";
        return tree;
    }

    tree->size++;
    return tree;
}

//' Transform a string into a vector of TLSH hashes
//'
//' @param s a string
//' @return a TLSH hash
//' @usage
//' out <- tlsh_from_str(string_val_gt_50)
// [[Rcpp::export]]
std::string tlsh_from_str(std::string s) {
    struct Tlsh* key = tlsh_init_buf(s.data(), s.size());
    int key_size;
    char* str = tlsh_to_string(key, &key_size);
    std::string out(str);
    free(str);

    return out;
}

//' Transform a vector of strings into a vector of TLSH hashes
//'
//' @param xs vector of strings
//' @return vector of TLSH hashes
//' @usage
//' df <- read.csv('test.csv')
//' names(df) <- c('raw_string_value')
//' df <- df %>%
//'   mutate(tlsh_hash = tlsh_from_strs(raw_string_value))
// [[Rcpp::export]]
StringVector tlsh_from_strs(std::vector<std::string> xs) {
    StringVector output;

    for (int i = 0; i < xs.size(); i++) {
        struct Tlsh* key = tlsh_init_buf(xs.at(i).data(), xs.at(i).size());
        int key_size;
        char* str = tlsh_to_string(key, &key_size);
        output.push_back(std::string(str));
        free(str);
    }
    return output;
}
