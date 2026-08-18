# Code generation plugin module

All code necessary for reproducing a data analysis as set up in blockr
can be made available to the user. Several ways of providing such a
script or code snippet are conceivable and currently implemented, we
have a modal with copy-to-clipboard functionality. This is readily
extensible, for example by offering a download button, by providing this
functionality as a `generate_code` module.

## Usage

``` r
generate_code(server = generate_code_server, ui = generate_code_ui)

generate_code_server(id, board, update, ...)

generate_code_ui(id, board)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- id:

  Namespace ID

- board:

  Reactive values object

- update:

  Reactive value object to initiate board updates

- ...:

  Extra arguments passed from parent scope

## Value

A plugin container inheriting from `generate_code` is returned by
`generate_code()`, while the UI component (e.g. `generate_code_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html))
and the server component (i.e. `generate_code_server()`) is expected to
return `NULL`.

## Details

Opening the modal asks for every block on the board to be built, through
the `construct` board update component (see the "Evaluation requests"
section of
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)),
because the script is assembled from block expressions and an unbuilt
block carries none. Nothing is evaluated: a board that defers its
off-screen blocks stays deferred, and the front-end's gating is
untouched.

Export is held back while a block is not fully configured, or while one
reports an error from its last run — either would put code into the
script that does not reproduce the board. A block that has never run
reports neither, so the modal offers to evaluate the board, which is a
one-off that leaves the blocks dormant again but has them report what
they found.
