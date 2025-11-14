# Board

A set of blocks, optionally connected via links and grouped into stacks
are organized as a `board` object. Boards are constructed using
`new_board()` and inheritance can be tested with `is_board()`, while
validation is available as (generic function) `validate_board()`. This
central data structure can be extended by adding further attributes and
sub-classes. S3 dispatch is used in many places to control how the UI
looks and feels and using this extension mechanism, UI aspects can be
customized to user requirements. Several utilities are available for
retrieving and modifying block attributes (see
[`board_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md)).

## Usage

``` r
new_board(
  blocks = list(),
  links = list(),
  stacks = list(),
  options = default_board_options(),
  ...,
  class = character()
)

validate_board(x)

is_board(x)
```

## Arguments

- blocks:

  Set of blocks

- links:

  Set of links

- stacks:

  Set of stacks

- options:

  Board-level user settings

- ...:

  Further (metadata) attributes

- class:

  Board sub-class

- x:

  Board object

## Value

The board constructor `new_board()` returns a `board` object, as does
the validator `validate_board()`, which typically is called for side
effects in the form of errors. Inheritance checking as `is_board()`
returns a scalar logical.

## Examples

``` r
brd <- new_board(
  c(
     a = new_dataset_block(),
     b = new_subset_block()
  ),
  list(from = "a", to = "b")
)

is_board(brd)
#> [1] TRUE
```
