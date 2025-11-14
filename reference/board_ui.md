# Board UI

As counterpart to
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md),
`board_ui()` is responsible for rendering UI for a board module. This
top-level entry point for customizing board appearance and functionality
can be overridden by sub-classing the boar object and providing an
implementation for this sub-class. Such an implementation is expected to
handle UI for plugins and all board components, including blocks, links
and stacks, but may rely on functionality that generates UI for these
components, such as
[`block_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md)
or
[`stack_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/stack_ui.md),
as well as already available UI provided by plugins themselves.
Additionally, `toolbar_ui()` is responsible for creating a toolbar UI
component from several plugin UI components.

## Usage

``` r
# S3 method for class 'board_options'
board_ui(id, x, ...)

board_ui(id, x, ...)

# S3 method for class 'board'
board_ui(id, x, plugins = board_plugins(x), options = NULL, ...)

# S3 method for class '`NULL`'
board_ui(id, x, ...)

insert_block_ui(id, x, blocks = NULL, ..., session = get_session())

# S3 method for class 'board'
insert_block_ui(id, x, blocks = NULL, ..., session = get_session())

remove_block_ui(id, x, blocks = NULL, ..., session = get_session())

# S3 method for class 'board'
remove_block_ui(id, x, blocks = NULL, ..., session = get_session())

toolbar_ui(id, x, plugins = list(), ...)

# S3 method for class 'board'
toolbar_ui(id, x, plugins = list(), options = NULL, ...)
```

## Arguments

- id:

  Namespace ID

- x:

  Board

- ...:

  Generic consistency

- plugins:

  UI for board plugins

- options:

  Board options (`NULL` defaults to the union of board, block and
  registry sourced options)

- blocks:

  (Additional) blocks (or IDs) for which to generate the UI

- session:

  Shiny session

## Value

A `board_ui()` implementation is expected to return
[shiny::tag](https://rdrr.io/pkg/shiny/man/reexports.html) or
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)
objects, as does `toolbar_ui()`, while updater functions
(`insert_block_ui()` and `remove_block_ui()`) are called for their side
effects (which includes UI updates such as
[`shiny::insertUI()`](https://rdrr.io/pkg/shiny/man/insertUI.html),
[`shiny::removeUI()`](https://rdrr.io/pkg/shiny/man/insertUI.html)) and
return the board object passed as `x` invisibly.

## Details

Dynamic UI updates are handled by functions `insert_block_ui()` and
`remove_block_ui()` for adding and removing block-level UI elements to
and from `board` UI, whenever blocks are added or removed. These update
functions are provided as S3 generics with implementations for `board`
and can be extended if so desired.
