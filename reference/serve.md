# Serve object

Intended as entry point to start up a shiny app, the generic function
`serve()` can be dispatched either on a single block (mainly for
previewing purposes during block development) or an entire board

## Usage

``` r
serve(x, ...)

# S3 method for class 'block'
serve(x, id = "block", ..., data = list())

# S3 method for class 'board'
serve(
  x,
  id = rand_names(),
  plugins = blockr_app_plugins,
  options = blockr_app_options,
  loader = local_loader(),
  ...
)

blockr_app_plugins(x)

custom_plugins(x)

blockr_app_options(x)

custom_options(x)

blockr_app_ui(id, x, ..., query = list())

blockr_app_server(id, x, ..., query = list())

blockr_test_exports(x, rv, ...)
```

## Arguments

- x:

  Object

- ...:

  Generic consistency

- id:

  Board namespace ID

- data:

  Data inputs

- plugins:

  Board plugins

- options:

  Board options

- loader:

  A
  [`board_loader()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_loader.md)
  resolving the board to build for each request; defaults to
  [`local_loader()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_loader.md),
  the in-process save/restore handoff

- query:

  Parsed URL query parameters (a named list) for the incoming request,
  passed to `blockr_app_ui()` and `blockr_app_server()` at both the GET
  and the websocket phase. A board subclass method reads it to make the
  rendered UI and the server URL-aware – for example picking an initial
  view from `query[["view"]]`; the default methods ignore it.

- rv:

  Board
  [`shiny::reactiveValues()`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)

## Value

The generic `serve()` is expected to return the result of a call to
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwzEGcIbgAEs1EXYKAvAqEALUqVTtEAeiMEAnhElQMAcxaltAVzoYWRIyLGSMtOliNCAJQCENwsjFAMZnyeElIycMEccAwAbnCCEAoKEHDUAPrwDDZw+bHigmAAKizwQbgh2QAmUFwaCmGcmdnZAB7tLVzscKSGiABCAPIAIg1ZPWYDrVDDo8wAwtosEgDqcCw2uo0KSYFgAL4AukA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwzEGcIbgAEs1EXYKAvAqEALUqVTtEAeiMEAnhElQMAcxaltAVzoYWRIyLGSMtOliNCAJQCENwsjFAMZnyeElIycMEccAwAbnCCEAoKEHDUAPp0RJEAJpnZ2bHiapoE5RXZUBo5efklUFzscKSFohKCYABCAPIAIkG4IQ2VzbkF7Z3dvV4DAMLaLBIA6nAsNroTU9MEs63wDDZwy-1CACos8EFH2YGTWRVhljUKdc8VUCdNHN8p9xAMoPItIRIUIAB6Hd4NOiAloFUEDOgw6F4KFmJ6IhSvP6cAHVZphTh8AHNX4oLEEIJJF4hQJgAC+AF0gA)
