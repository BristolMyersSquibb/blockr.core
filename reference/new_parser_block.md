# Parser block constructors

Operating on results from blocks created via
[`new_file_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_file_block.md),
parser blocks read (i.e. "parse") a file and make the contents available
to subsequent blocks for further analysis and visualization.

## Usage

``` r
new_parser_block(
  server,
  ui,
  class,
  ctor = sys.parent(),
  dat_valid = is_file,
  ...
)

new_csv_block(sep = ",", quote = "\"", ...)
```

## Arguments

- server:

  A function returning
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)

- ui:

  A function with a single argument (`ns`) returning a `shiny.tag`

- class:

  Block subclass

- ctor:

  String-valued constructor name or function/frame number (mostly for
  internal use or when defining constructors for virtual classes)

- dat_valid:

  (Optional) input data validator

- ...:

  Forwarded to `new_parser_block()` and
  [`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)

- sep, quote:

  Forwarded to
  [`utils::read.table()`](https://rdrr.io/r/utils/read.table.html)

## Value

All blocks constructed via `new_parser_block()` inherit from
`parser_block`.

## Details

If using the default validator for a parser block sub-class (i.e. not
overriding the `dat_valid` argument in the call to
`new_parser_block()`), the data argument corresponding to the input file
name must be `file` in order to match naming conventions in the
validator function.

## CSV block

Files in CSV format provided for example by a block created via
[`new_file_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_file_block.md)
may be parsed into `data.frame` by CSV blocks.
