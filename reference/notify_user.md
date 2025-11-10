# User notification plugin module

During the evaluation cycle of each block, user notifications may be
generated to inform in case of issues such as errors or warnings. These
notifications are provided in a way that display can be controlled and
adapted to specific needs. The default `notify_user` plugin simply
displays notifications via
[`shiny::showNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html),
with some ID management in order to be able to clear no longer relevant
notifications via
[`shiny::removeNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html).

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
`notify_user()`, while the UI component (e.g. `notify_user_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html); if
available) and the server component (i.e. `notify_user_server()`) is
expected to return a
[`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
or [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
which evaluates to a list containing notifications per block and
notification type (i.e. "message", "warning" or "error").
