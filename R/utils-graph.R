#' Graph utils
#'
#' Block dependencies are represented by DAGs and graph utility functions
#' `topo_sort()` and `is_acyclic()` are used to create a topological ordering
#' (implemented as DFS) of blocks and to check for cycles. An adjacency matrix
#' corresponding to a board is available as `as.matrix()`.
#'
#' @param x Adjacency matrix/board object
#'
#' @examples
#' brd <- new_board(
#'   c(
#'      a = new_dataset_block(),
#'      b = new_dataset_block(),
#'      c = new_scatter_block(),
#'      d = new_subset_block()
#'   ),
#'   list(from = c("a", "d"), to = c("d", "c"))
#' )
#'
#' as.matrix(brd)
#' topo_sort(brd)
#' is_acyclic(brd)
#'
#' @return Topological ordering via `topo_sort()` returns a character vector
#' with sorted node IDs and the generic function `is_acyclic()` is expected to
#' return a scalar logical value.
#'
#' @export
topo_sort <- function(x) {

  if (is_board(x)) {
    x <- as.matrix(x)
  }

  stopifnot(
    is.matrix(x), nrow(x) == ncol(x),
    identical(rownames(x), colnames(x)),
    is.integer(x), all(x %in% c(0L, 1L))
  )

  num_nodes <- nrow(x)

  if (!num_nodes) {
    return(character())
  }

  dfs <- local({

    visited <- rep(FALSE, num_nodes)
    on_stack <- rep(FALSE, num_nodes)

    function(node, adj, stack) {

      if (on_stack[node]) {
        abort(
          "The graph contains a cycle and is not a DAG.",
          class = "graph_has_cycle"
        )
      }

      if (visited[node]) {
        return(stack)
      }

      visited[node] <<- TRUE
      on_stack[node] <<- TRUE

      for (neighbor in which(adj[node, ] == 1)) {
        stack <- Recall(neighbor, adj, stack)
      }

      on_stack[node] <<- FALSE

      c(node, stack)
    }
  })

  stack <- integer()

  for (node in seq_len(num_nodes)) {
    stack <- dfs(node, x, stack)
  }

  rownames(x)[stack]
}

as_adjacency_matrix <- function(from, to, nodes = union(from, to)) {

  stopifnot(
    all(from %in% nodes), all(to %in% nodes), anyDuplicated(nodes) == 0L
  )

  n <- length(nodes)

  res <- matrix(0L, n, n, dimnames = list(nodes, nodes))
  res[cbind(from, to)] <- 1L

  res
}

#' @param x Object
#' @rdname topo_sort
#' @export
is_acyclic <- function(x) {
  UseMethod("is_acyclic")
}

#' @rdname topo_sort
#' @export
is_acyclic.matrix <- function(x) {
  tryCatch(
    {
      topo_sort(x)
      TRUE
    },
    graph_has_cycle = function(e) FALSE
  )
}
