# Destroy a Shiny module

Cleans up inputs, outputs and/or observers associated with a Shiny
module.

## Usage

``` r
destroy_module(
  id,
  what = c("inputs", "outputs", "observers"),
  session = get_session()
)
```

## Arguments

- id:

  Module namespace id.

- what:

  Character vector indicating which components to destroy. Defaults to
  all of `"inputs"`, `"outputs"` and `"observers"`.

- session:

  Shiny session object.

## Value

The namespaced id (invisibly).

## Note

This function relies on non-documented, internal Shiny APIs that are not
officially supported or recommended to use. These internals may break if
Shiny's internal source code changes. Use with caution and only whenever
necessary.
