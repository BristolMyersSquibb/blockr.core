# User notification plugin module

During the evaluation cycle of each block, conditions (errors, warnings
and messages) may be raised. The default `notify_user` plugin surfaces
these to the user as
[`shiny::showNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html)
toasts, displaying newly active conditions and clearing ones that are no
longer active via
[`shiny::removeNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html).
Each block's conditions (see
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md))
are tracked individually, so that a single block's change touches only
its own notifications rather than re-processing the whole board.

## Usage

``` r
notify_user(server = notify_user_server, ui = notify_user_ui)

notify_user_server(id, board, ...)

notify_user_ui(id, board)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- id:

  Namespace ID

- board:

  Reactive values object

- ...:

  Extra arguments passed from parent scope

## Value

A plugin container inheriting from `notify_user` is returned by
`notify_user()` and the UI component (e.g. `notify_user_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)). The
server component (i.e. `notify_user_server()`) is called for the side
effect of managing notifications and returns `NULL`.
