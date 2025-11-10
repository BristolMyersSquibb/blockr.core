# Utilities for code export

To facilitate other means of code export than implemented by the default
[`generate_code()`](https://bristolmyerssquibb.github.io/blockr.core/reference/generate_code.md)
plugin, this utility performs much of the heavy lifting to properly
arrange and scope block-level expressions.

## Usage

``` r
export_code(expressions, board)
```

## Arguments

- expressions:

  Block expressions

- board:

  Board object

## Value

String containing properly arranged block expressions.
