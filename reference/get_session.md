# Shiny utilities

Utility functions for shiny:

- `get_session`: See
  [`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html).

- `generate_plugin_args`: Meant for unit testing plugins.

- `notify`: Glue-capable wrapper for
  [`shiny::showNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html).

## Usage

``` r
get_session()

notify(
  ...,
  envir = parent.frame(),
  action = NULL,
  duration = 5,
  close_button = TRUE,
  id = NULL,
  type = c("message", "warning", "error"),
  session = get_session()
)
```

## Arguments

- ...:

  Concatenated as `paste0(..., "\n")`

- envir:

  Environment where the logging call originated from

- action:

  Message content that represents an action. For example, this could be
  a link that the user can click on. This is separate from `ui` so
  customized layouts can handle the main notification content separately
  from action content.

- duration:

  Number of seconds to display the message before it disappears. Use
  `NULL` to make the message not automatically disappear.

- close_button:

  Passed as `closeButton` to
  [`shiny::showNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html)

- id:

  A unique identifier for the notification.

  `id` is optional for `showNotification()`: Shiny will automatically
  create one if needed. If you do supply it, Shiny will update an
  existing notification if it exists, otherwise it will create a new
  one.

  `id` is required for `removeNotification()`.

- type:

  A string which controls the color of the notification. One of
  "default" (gray), "message" (blue), "warning" (yellow), or "error"
  (red).

- session:

  Session object to send notification to.

## Value

Either `NULL` or a shiny session object for `get_session()`, a list of
arguments for plugin server functions in the case of
[`generate_plugin_args()`](https://bristolmyerssquibb.github.io/blockr.core/reference/testing.md)
and `notify()` is called for the side-effect of displaying a browser
notification (and returns `NULL` invisibly).
