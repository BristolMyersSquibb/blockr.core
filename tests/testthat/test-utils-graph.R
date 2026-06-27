test_that("topo sort", {

  # linear graph
  expect_identical(
    topo_sort(as_adjacency_matrix(letters[1:3], letters[2:4])),
    letters[1:4]
  )

  # diverging paths
  expect_identical(
    topo_sort(as_adjacency_matrix(from = c("a", "a", "b", "c"),
                                  to = c("b", "c", "d", "d"))),
    c("a", "c", "b", "d") # or c("a", "b", "c", "d")
  )

  # multiple roots
  expect_identical(
    topo_sort(as_adjacency_matrix(from = c("a", "c"), to = c("b", "d"))),
    c("c", "d", "a", "b") # or c("a", "b", "c", "d")
  )

  # cycle
  expect_error(
    topo_sort(as_adjacency_matrix(from = c("a", "b", "c"),
                                  to = c("b", "c", "a"))
    ),
    "The graph contains a cycle and is not a DAG."
  )

  # single node
  expect_identical(
    topo_sort(as_adjacency_matrix(character(), character(), "a")),
    "a"
  )

  # empty graph
  expect_identical(
    topo_sort(as_adjacency_matrix(from = character(), to = character())),
    character()
  )
})

test_that("upstream blocks", {

  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      c = new_scatter_block(), d = new_subset_block()),
    list(from = c("a", "d"), to = c("d", "c"))
  )

  expect_setequal(upstream_blocks("c", board), c("a", "c", "d"))
  expect_setequal(upstream_blocks("d", board), c("a", "d"))
  expect_setequal(upstream_blocks("a", board), "a")
  expect_setequal(upstream_blocks("b", board), "b")
  expect_setequal(upstream_blocks(c("b", "c"), board), c("a", "b", "c", "d"))

  empty <- new_board(c(a = new_dataset_block("iris")))

  expect_setequal(upstream_blocks("a", empty), "a")
  expect_identical(upstream_blocks(character(), empty), character())
})
