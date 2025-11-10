# File block constructors

Similarly to
[`new_data_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_data_block.md),
blocks created via `new_file_block()` serve as starting points in
analysis pipelines by providing data to down-stream blocks. They
typically will not have data inputs and represent root nodes in analysis
graphs.

## Usage

``` r
new_file_block(server, ui, class, ctor = sys.parent(), ...)

new_filebrowser_block(
  file_path = character(),
  volumes = filebrowser_volumes(),
  ...
)

filebrowser_volumes(default = c(home = path.expand("~")))

new_upload_block(...)
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

- ...:

  Forwarded to `new_file_block()` and
  [`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)

- file_path:

  File path

- volumes:

  Parent namespace

- default:

  Default volumes specification (use the blockr option "volumes" to
  override)

## Value

All blocks constructed via `new_file_block()` inherit from `file_block`.

## File browser block

In order to make user data available to blockr, this block provides
file- upload functionality via
[`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html).
Given that data provided in this way are only available for the
life-time of the shiny session, exported code is not self-contained and
a script containing code from an upload block is cannot be run in a new
session. Also, serialization of upload blocks is currently not allowed
as the full data would have to be included during serialization.

## Upload block

In order to make user data available to blockr, this block provides
file- upload functionality via
[`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html).
Given that data provided in this way are only available for the
life-time of the shiny session, exported code is not self-contained and
a script containing code from an upload block is cannot be run in a new
session. Also, serialization of upload blocks is currently not allowed
as the full data would have to be included during serialization.
