# Remove entries from a `reactiveValues` object

shiny offers no public way to delete a key from a
[`shiny::reactiveValues()`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
object – assigning `NULL` stores a `NULL` value but leaves the key in
[`names()`](https://rdrr.io/r/base/names.html). `trim_rv()` removes the
named entries outright and invalidates the affected reactive
dependencies, so a key can be truly dropped (and later re-added) – for
instance when a variadic block argument is unlinked.

## Usage

``` r
trim_rv(x, rm)
```

## Arguments

- x:

  A `reactiveValues` object.

- rm:

  Character vector of keys to remove; all must be present in `x`.

## Value

`x`, invisibly.
