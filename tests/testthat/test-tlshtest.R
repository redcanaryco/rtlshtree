library(testthat)

test_that("tlsh strings parse correctly", {
  hashes <- c(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"
  )

  tlsh_v <- c(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2"
  )


  tree <- tlsh_tree_new(20, hashes)

  out <- tlsh_tree_matches(tree, 10, tlsh_v)
  df <- data.frame(out)
  expect_equal(nrow(df), 6)
  expect_equal(ncol(df), 2)

  rm(tree)
})

test_that("tlsh pairwise count", {
  hashes <- c(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"
  )

  out <- tlsh_n_pairs(10, hashes)
  expect_equal(out, 14)
})


test_that("tlsh_diff works", {
  out <- tlsh_diff(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2"
  )

  expect_equal(out, 1)
})

test_that("tlsh tree builds and can lookup", {
  hashes <- c(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"
  )

  tree <- tlsh_tree_new(20, hashes)

  out1 <- tlsh_tree_n_matches(tree, 0, "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa")
  expect_equal(out1[1], 1)

  out2 <- tlsh_tree_n_matches(tree, 2, "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa")
  expect_equal(out2[1], 2)

  out6 <- tlsh_tree_n_matches(tree, 100000, "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa")
  expect_equal(out6[1], 6)
})

test_that("tlsh tree can add new and non duplicate items", {
  hashes <- c(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"
  )

  tree <- tlsh_tree_new(20, hashes)

  tree <- tlsh_tree_add(tree, "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa")
  out <- tlsh_tree_n_matches(tree, 0, "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa")
  expect_equal(out[1], 1)

  tree <- tlsh_tree_add(tree, "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaac")

  out <- tlsh_tree_n_matches(tree, 0, "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaac")

  expect_equal(out[1], 1)
})

test_that("tlsh_n_pairs produces the correct amount of pairs from a vector", {
  hashes <- c(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"
  )

  out <- tlsh_n_pairs(50, hashes)
  expect_equal(out, 15)
})

test_that("tlsh can produce a correct hash", {
  hash <- "MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!"
  out <- tlsh_from_str(hash)
  expect_equal(out, "8ad02202fc30c02303a002b02b33b00ec30a82f80008e2fa000a008030b20e03cca0c2")
})

test_that("tlsh can produce a correct hash", {
  hash <- c("MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!")
  out <- tlsh_from_strs(hash)
  expect_equal(out[1], "8ad02202fc30c02303a002b02b33b00ec30a82f80008e2fa000a008030b20e03cca0c2")
})

test_that("tlsh tree builds and can lookup tree with splits", {
  hashes <- c(
    "54456c07b6a214fcc5d6AAAAA26b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570BBBBB6b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826bCCCCCc70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6c570826b96b2DDDDD4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70EEEEE1327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a5FFFFFd7b349ce6302f52f642baaaaa",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327AAAAA9ce6302f52f641b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b3BBBBB302f52f632b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ceCCCCC52f643b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302DDDDD44b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52fEEEEEaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f646bFFFFF",
    "54456c07AAAAA4fcc5d61570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a2BBBBB5d62570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcCCCCC570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6DDDDD26b96b26c70b4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d65570EEEEE6b26c70b4a581327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d66570826bFFFFFc70b4a581327d7b349ce6302f52f642baaaaa",
    "54456c07b6a214fcc5d6c570826b96b26AAAAAa581327d7b349ce6302f52f641b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70bBBBBB327d7b349ce6302f52f632b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a58CCCCC7b349ce6302f52f643b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327DDDDD9ce6302f52f644b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b3EEEEE302f52f645baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ceFFFFF52f646baaaaa",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"
  )

  tree <- tlsh_tree_new(10, hashes)

  out1 <- tlsh_tree_n_matches(tree, 0, c("54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"))
  expect_equal(out1[1], 1)
})

test_that("tlsh that we can remove a hash", {
  hash <- c("MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!!MIT License is so cool license that I can't imagine a better one!")
  out <- tlsh_from_strs(hash)
  expect_equal(out[1], "8ad02202fc30c02303a002b02b33b00ec30a82f80008e2fa000a008030b20e03cca0c2")
})

test_that("tlsh tree builds and can lookup tree with splits", {
  hashes <- c(
    "54456c07b6a214fcc5d6AAAAA26b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570BBBBB6b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826bCCCCCc70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6c570826b96b2DDDDD4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70EEEEE1327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a5FFFFFd7b349ce6302f52f642baaaaa",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327AAAAA9ce6302f52f641b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b3BBBBB302f52f632b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ceCCCCC52f643b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302DDDDD44b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52fEEEEEaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f646bFFFFF",
    "54456c07AAAAA4fcc5d61570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a2BBBBB5d62570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcCCCCC570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6DDDDD26b96b26c70b4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d65570EEEEE6b26c70b4a581327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d66570826bFFFFFc70b4a581327d7b349ce6302f52f642baaaaa",
    "54456c07b6a214fcc5d6c570826b96b26AAAAAa581327d7b349ce6302f52f641b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70bBBBBB327d7b349ce6302f52f632b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a58CCCCC7b349ce6302f52f643b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327DDDDD9ce6302f52f644b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b3EEEEE302f52f645baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ceFFFFF52f646baaaaa",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"
  )

  tree <- tlsh_tree_new(10, hashes)

  out1 <- tlsh_tree_n_matches(tree, 0, c("54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"))
  expect_equal(out1[1], 1)

  info <- tlsh_tree_info(tree)
  expect_equal(info$size, 25)

  tlsh_tree_delete(tree, "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa")


  info <- tlsh_tree_info(tree)
  expect_equal(info$size, 24)

  out1 <- tlsh_tree_n_matches(tree, 0, c("54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"))
  expect_equal(out1[1], 0)
})


test_that("tlsh union count is correct", {
  list1 <- c(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe4",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaab",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642baaaaa"
  )

  list2 <- c(
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe1",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe2",
    "54456c07b6a214fcc5d6c570826b96b26c70b4a581327d7b349ce6302f52f642b6efe3"
  )

  out <- tlsh_n_union(0, list1, list2)
  expect_equal(out[1], 3)

})
