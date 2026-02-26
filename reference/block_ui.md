# Block UI

The UI associated with a block is created via the generics `expr_ui()`
and `block_ui()`. The former is mainly responsible for user inputs that
are specific to every block type (i.e. a `subset_block` requires
different user inputs compared to a `head_block`, see
[`new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.md))
and essentially calls the UI function passed as `ui` to
[`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md).
UI that represents block outputs typically is shared among similar block
types (i.e. blocks with shared inheritance structure, such as
`subset_block` and `head_block`, which both inherit from
`transform_block`). This type of UI us created by `block_ui()` and block
inheritance is used to deduplicate shared functionality (i.e. by
implementing a method for the `transform_block` class only instead of
separate methods for `subset_block` and `head_block`. Working in tandem
with `block_ui()`, the generic `block_output()` generates the output to
be displayed by the UI portion defined via `block_ui()`.

## Usage

``` r
block_ui(id, x, ...)

expr_ui(id, x, ...)

block_output(x, result, session)

# S3 method for class 'board'
block_ui(id, x, blocks = NULL, edit_ui = NULL, ctrl_ui = NULL, ...)
```

## Arguments

- id:

  Namespace ID

- x:

  Object for which to generate a UI container

- ...:

  Generic consistency

- result:

  Block result

- session:

  Shiny session object

- blocks:

  (Additional) blocks (or IDs) for which to generate the UI

- edit_ui, ctrl_ui:

  Block plugin UI

## Value

Both `expr_ui()` and `block_ui()` are expected to return shiny UI (e.g.
objects wrapped in a
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)). For
rendering the UI, `block_output()` is required to return the result of a
shiny render function. For example, a transform block might show the
resulting `data.frame` as an HTML table using the DT package. The
corresponding `block_ui()` function would then contain UI created by
[`DT::dataTableOutput()`](https://rdrr.io/pkg/DT/man/dataTableOutput.html)
and rendering in `block_output()` would then be handled by
[`DT::renderDT()`](https://rdrr.io/pkg/DT/man/dataTableOutput.html).

## Details

The result of `block_output()`, which is evaluated in the
[`block_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_server.md)
context is assigned to `output$result`. Consequently, when referencing
the block result in `block_ui()`, this naming convention has to be
followed by referring to this as something like
`shiny::NS(id, "result")`.

## Board-level block UI

While the contents of block-level UI are created by dispatching
`block_ui()` on blocks another dispatch on `board` (see
[`new_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board.md))
occurs as well. This can be used to control how blocks are integrated
into the board UI. For the default board, this uses
[`bslib::card()`](https://rstudio.github.io/bslib/reference/card.html)
to represent blocks. For boards that extend the default `board` class,
control is available for how blocks are displayed by providing a
board-specific `block_ui()` method.
