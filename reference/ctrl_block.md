# Plugin module for external control of block inputs

This plugin enables setting block reactive state values from outside the
block expression server context. Blocks opt in to external control via
the `external_ctrl` argument to
[`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md),
which can be set to `TRUE` (all constructor inputs) or a character
vector of specific input names. The default UI renders a
[`shiny::textInput()`](https://rdrr.io/pkg/shiny/man/textInput.html) for
each externally controllable input along with a submit
[`shiny::actionButton()`](https://rdrr.io/pkg/shiny/man/actionButton.html).
Both the server and UI can be replaced with custom implementations by
passing alternate functions to `ctrl_block()`.

## Usage

``` r
ctrl_block(server = ctrl_block_server, ui = ctrl_block_ui)

ctrl_block_server(id, x, vars, data, eval)

ctrl_block_ui(id, x)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- id:

  Namespace ID

- x:

  Block

- vars:

  Reactive state values (list of `reactiveVal` objects keyed by input
  name)

- data:

  Input data paseed as list of reactive values

- eval:

  Reactive that evaluates the block expression against input data. May
  be used to validate that the new values produce a successful
  evaluation.

## Value

A plugin container inheriting from `ctrl_block` is returned by
`ctrl_block()`, while the UI component (i.e. `ctrl_block_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)) and
the server component (i.e. `ctrl_block_server()`) is expected to return
a value that passes validation (i.e. `TRUE` or a reactive gate).

## Details

The default server validates submitted values by evaluating the block
expression (via the `eval` reactive) after updating state. On success, a
reactive gate is returned as `TRUE`, allowing downstream evaluation to
proceed. On failure, state values are reverted to their previous values,
the user is notified, and the gate is set to `FALSE`, which blocks
downstream evaluation until a subsequent successful submit.
