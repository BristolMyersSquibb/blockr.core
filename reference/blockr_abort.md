# Blockr conditions

Wrappers for
[`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html),
[`rlang::warn()`](https://rlang.r-lib.org/reference/abort.html) and
[`rlang::inform()`](https://rlang.r-lib.org/reference/abort.html). In
addition to `class`, conditions inherit from "blockr_error".

## Usage

``` r
blockr_abort(..., class = character(), envir = parent.frame())

blockr_warn(
  ...,
  class = character(),
  envir = parent.frame(),
  frequency = "always",
  frequency_id = NULL
)

blockr_inform(
  ...,
  class = character(),
  envir = parent.frame(),
  frequency = "always",
  frequency_id = NULL
)
```

## Arguments

- ...:

  Forwarded to
  [`cli::pluralize()`](https://cli.r-lib.org/reference/pluralize.html)

- class:

  Condition class

- envir:

  Forwarded to
  [`cli::pluralize()`](https://cli.r-lib.org/reference/pluralize.html)

- frequency, frequency_id:

  Forwarded to
  [`rlang::warn()`](https://rlang.r-lib.org/reference/abort.html)

## Value

Called for side-effect of signaling conditions.
