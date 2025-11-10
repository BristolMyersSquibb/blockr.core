# Graph utils

Block dependencies are represented by DAGs and graph utility functions
`topo_sort()` and `is_acyclic()` are used to create a topological
ordering (implemented as DFS) of blocks and to check for cycles. An
adjacency matrix corresponding to a board is available as
[`as.matrix()`](https://rdrr.io/r/base/matrix.html).

## Usage

``` r
# S3 method for class 'board'
is_acyclic(x)

# S3 method for class 'links'
is_acyclic(x)

topo_sort(x)

is_acyclic(x)

# S3 method for class 'matrix'
is_acyclic(x)
```

## Arguments

- x:

  Object

## Value

Topological ordering via `topo_sort()` returns a character vector with
sorted node IDs and the generic function `is_acyclic()` is expected to
return a scalar logical value.

## Examples

``` r
brd <- new_board(
  c(
     a = new_dataset_block(),
     b = new_dataset_block(),
     c = new_scatter_block(),
     d = new_subset_block()
  ),
  list(from = c("a", "d"), to = c("d", "c"))
)

as.matrix(brd)
#>   a b c d
#> a 0 0 0 1
#> b 0 0 0 0
#> c 0 0 0 0
#> d 0 0 1 0
topo_sort(brd)
#> [1] "b" "a" "d" "c"
is_acyclic(brd)
#> [1] TRUE
```
