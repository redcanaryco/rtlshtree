# TLSH and TLSH Tree implementation in R

This repo contains an implementation of TLSH and a TLSH tree structure for
efficiently searching for similar TLSH hashes.

## References
- Jonathan Oliver, Muqeet Ali, Josiah Hagen, "[HAC-T and Fast Search for Similarity in Security](https://tlsh.org/papersDir/COINS_2020_camera_ready.pdf)
- Jonathan Oliver, Chun Cheng and Yanggui Chen, “[TLSH - A Locality
    Sensitive
    Hash](https://github.com/trendmicro/tlsh/blob/master/TLSH_CTC_final.pdf)”
    4th Cybercrime and Trustworthy Computing Workshop, Sydney, November
    2013
- Lucas Rist, [TLSH lib in Golang](https://github.com/glaslos/tlsh)

## Installation

```
devtools::install_github("github.com/redcanaryco/rtlshtree")
```

## Usage

`tlsh_diff(tlsh_a, tlsh_b)`

Calculates the distance between two TLSH hashes.

`tlsh_from_str(str_val)`

Calculates the TLSH hash for a raw string input.

`tlsh_from_strs(str_vector)`

Calculate the TLSH hash for each raw string value in the vector. Returns a vector of TLSH hashes.

`tlsh_n_pairs(50, tlsh_vector)`

Returns the number of pairwise matches that are no more than a distance of 50 apart.

`tlsh_tree_add(tree, tlsh)`

Adds a TLSH hash to an existing TLSH tree. This will not change the structure of
the tree, only add the new TLSH to the appropriate leaf node.

`tlsh_tree_delete(tree, tlsh)`

Removes a TLSH hash from an existing TLSH tree.

`tlsh_tree_matches(tree, 50, tlsh_vector)`

Returns a R List where each key points to a vector of TLSH matches.

`tlsh_tree_n_matches(tree, 50 tlsh_vector)`

Returns a vector of counts corresponding to the input tlsh_vector. The count is
how many matches were found in the tree for the give hash.

`tlsh_tree_new(20, tlsh_vector)`

Creates a new TLSH tree given a set of TLSH hashes, with the given leaf size.

`tlsh_tree_info(tree)`

Reports basic information about the TLSH tree

## Example

```
library(tidyverse)
library(jsonlite)
library(tlshtree)

# test.jsonl
#   Structure: {"raw": "..."}
df <- readLines("test.jsonl") %>%
    lapply(fromJSON) %>%
    lapply(unlist) %>%
    bind_rows() %>%
    mutate(tlsh = tlsh_from_strs(raw))

tlsh.tree <- tlsh_tree_new(df$tlsh, 20)

df <- df %>%
    mutate(num_matches = tlsh_tree_n_matches(tlsh.tree, tlsh, 50))

# df <- df[order(-num_matches), ]
# knitr::kable(df, "simple")
```

## Development

Obviously, you'll need an installation of R. And you'll need to have the [devtools](https://github.com/r-lib/devtools) package installed.

To get started running the tests, git clone this repo and navigate to the project root and run the following:

```
Rscript -e "library(devtools); devtools::test()"
```

For debugging, you can start R with the `--debugger=<lldb/gdb>` flag and any crashes will automatically start a debugger with symbols included.
