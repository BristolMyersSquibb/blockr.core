# Board utils

A set of utility functions is available for querying and manipulating
board components (i.e. blocks, links and stacks). Functions for
retrieving and modifying board options are documented in
[`new_board_options()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.md).

## Usage

``` r
board_blocks(x)

board_blocks(x) <- value

board_block_ids(x)

rm_blocks(x, rm, ..., session = get_session())

board_links(x)

board_links(x) <- value

board_link_ids(x)

modify_board_links(
  x,
  add = NULL,
  rm = NULL,
  mod = NULL,
  ...,
  session = get_session()
)

board_stacks(x)

board_stacks(x) <- value

board_stack_ids(x)

modify_board_stacks(
  x,
  add = NULL,
  rm = NULL,
  mod = NULL,
  ...,
  session = get_session()
)

board_options(x)

board_options(x) <- value

board_option_ids(x)

available_stack_blocks(
  x,
  stacks = board_stacks(x),
  blocks = board_stack_ids(x)
)
```

## Arguments

- x:

  Board

- value:

  Replacement value

- rm:

  Block/link/stack IDs to remove

- ...:

  Further arguments they may be passed from the board server context

- session:

  Shiny session object

- add:

  Links/stacks to add

- mod:

  Link/stacks to modify

- blocks, stacks:

  Sets of blocks/stacks

## Value

Functions for retrieving, as well as updating components
(`board_blocks()`/`board_links()`/`board_stacks()`/`board_options()` and
`board_blocks<-()`/`board_links<-()`/`board_stacks<-()`/`board_options<-()`)
return corresponding objects (i.e. `blocks`, `links`, `stacks` and
`board_options`), while ID getters (`board_block_ids()`,
`board_link_ids()`, `board_stack_ids()` and `board_option_ids()`) return
character vectors, as does `available_stack_blocks()`. Convenience
functions `rm_blocks()`, `modify_board_links()` and
`modify_board_stacks()` return an updated `board` object.

## Blocks

Board blocks can be retrieved using `board_blocks()` and updated with
the corresponding replacement function `board_blocks<-()`. If just the
current board IDs are of interest, `board_block_ids()` is available as
short for `names(board_blocks(x))`. In order to remove block(s) from a
board, the (generic) convenience function `rm_blocks()` is exported,
which takes care (in the default implementation for `board`) of also
updating links and stacks accordingly. The more basic replacement
function `board_blocks<-()` might fail at validation of the updated
board object if an inconsistent state results from an update (e.g. a
block referenced by a stack is no longer available).

## Links

Board links can be retrieved using `board_links()` and updated with the
corresponding replacement function `board_links<-()`. If only links IDs
are of interest, this is available as `board_link_ids()`, which is short
for `names(board_links(x))`. A (generic) convenience function for all
kinds of updates to board links in one is available as
`modify_board_links()`. With arguments `add`, `rm` and `mod`, links can
be added, removed or modified in one go.

## Stacks

Board stacks can be retrieved using `board_stacks()` and updated with
the corresponding replacement function `board_stacks<-()`. If only the
stack IDs are of interest, this is available as `board_stack_ids()`,
which is short for `names(board_stacks(x))`. A (generic) convenience
function to update stacks is available as `modify_board_stacks()`, which
can add, remove and modify stacks depending on arguments passed as
`add`, `rm` and `mod`. If block IDs that are not already associated with
a stack (i.e. "free" blocks) are of interest, this is available as
`available_stack_blocks()`.

## Options

Board options can be retrieved using `board_options()` and updated with
the corresponding replacement function `board_options<-()`. If only the
option IDs are of interest, this is available as `board_option_ids()`,
which calls
[`board_option_id()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.md)
on each board option.

## Examples

``` r
brd <- new_board(
  c(
     a = new_dataset_block(),
     b = new_subset_block()
  ),
  list(from = "a", to = "b")
)

board_blocks(brd)
#> <blocks[2]>
#> 
#> a
#>  <dataset_block<data_block<block>>>
#>  Name: "Dataset"
#>  No data inputs
#>  Initial block state:
#>   $ dataset: chr(0)
#>   $ package: chr "datasets"
#>  Constructor: blockr.core::new_dataset_block()
#> 
#> b
#>  <subset_block<transform_block<block>>>
#>  Name: "Subset"
#>  Data inputs: "data"
#>  Initial block state:
#>   $ subset: chr ""
#>   $ select: chr ""
#>  Constructor: blockr.core::new_subset_block()
board_block_ids(brd)
#> [1] "a" "b"

board_links(brd)
#> <links[1]>
#> tabarded_lice 
#> a -> b (data) 
board_link_ids(brd)
#> [1] "tabarded_lice"

board_stacks(brd)
#> <stacks[0]>
board_stack_ids(brd)
#> character(0)

board_options(brd)
```
