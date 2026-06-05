# Stack UI

Several generics are exported in order to integrate stack UI into board
UI. We have `stack_ui()` which is dispatched on the `board` (and in the
default implementation) on individual `stack` objects. This renders
stacks as bootstrap accordion items (using
[`bslib::accordion()`](https://rstudio.github.io/bslib/reference/accordion.html)).
If a different way of displaying stacks and integrating them with a
board is desired, this can be implemented by introducing a board
subclass and providing a `stack_ui()` method for that subclass.
Inserting stacks into (and removing stacks from) a board is available as
`insert_stack_ui()`/`remove_stack_ui()` and blocks into/from stacks via
`add_block_to_stack()`/`remove_block_from_stack()`. All are S3 generics
with implementations for `board` and alternative implementation may be
provided for board sub-classes.

## Usage

``` r
stack_ui(id, x, ...)

# S3 method for class 'board'
stack_ui(id, x, stacks = NULL, edit_ui = NULL, ...)

# S3 method for class 'stack'
stack_ui(id, x, edit_ui = NULL, ...)

insert_stack_ui(id, x, board, edit_ui = NULL, session = get_session(), ...)

# S3 method for class 'board'
insert_stack_ui(id, x, board, edit_ui = NULL, session = get_session(), ...)

remove_stack_ui(id, board, session = get_session(), ...)

# S3 method for class 'board'
remove_stack_ui(id, board, session = get_session(), ...)

add_block_to_stack(board, block_id, stack_id, session = get_session(), ...)

# S3 method for class 'board'
add_block_to_stack(board, block_id, stack_id, session = get_session(), ...)

remove_block_from_stack(
  board,
  block_id,
  board_id,
  session = get_session(),
  ...
)

# S3 method for class 'board'
remove_block_from_stack(
  board,
  block_id,
  board_id,
  session = get_session(),
  ...
)
```

## Arguments

- id:

  Parent namespace

- x:

  Object

- ...:

  Generic consistency

- stacks:

  (Additional) stacks (or IDs) for which to generate the UI

- edit_ui:

  Stack edit plugin

- board:

  Board object

- session:

  Shiny session

- block_id, stack_id, board_id:

  Block/stack/board IDs

## Value

UI set up via `stack_ui()` is expected to return
[`shiny::tag()`](https://rdrr.io/pkg/shiny/man/reexports.html) or
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)
objects while stack/block insertion/removal functions (into/from
board/stack objects) are called for their side-effects. Both
`insert_stack_ui()`/`remove_stack_ui` and
`add_block_to_stack()`/`remove_block_from_stack()` return `NULL`
invisibly and where the former call
[`shiny::insertUI()`](https://rdrr.io/pkg/shiny/man/insertUI.html)/[`shiny::removeUI()`](https://rdrr.io/pkg/shiny/man/insertUI.html)
and the latter modify the DOM via
[shiny::session](https://rdrr.io/pkg/shiny/man/session.html) custom
messages.
