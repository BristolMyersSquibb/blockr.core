# Assertions

Utility functions, mainly intended for asserting common preconditions
are exported for convenience in dependent packages.

## Usage

``` r
is_scalar(x)

is_string(x)

is_bool(x)

is_intish(x)

is_count(x, allow_zero = TRUE)

is_number(x)

not_null(...)

has_length(x)
```

## Arguments

- x:

  Object to check

- allow_zero:

  Determines whether the value 0 is considered a valid count

- ...:

  Silently ignored

## Value

Scalar logical value.
