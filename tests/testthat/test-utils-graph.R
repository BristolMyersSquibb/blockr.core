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
