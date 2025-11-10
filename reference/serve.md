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
serve(x, id = rand_names(), plugins = board_plugins(x), ...)

get_serve_obj()
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
