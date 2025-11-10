# Data block constructors

Data blocks typically do not have data inputs and represent root nodes
in analysis graphs. Intended as initial steps in a pipeline, such blocks
are responsible for providing down-stream blocks with data.

## Usage

``` r
new_data_block(server, ui, class, ctor = sys.parent(), ...)

new_dataset_block(dataset = character(), package = "datasets", ...)

new_static_block(data, ...)
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

  Forwarded to `new_data_block()` and
  [`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)

- dataset:

  Selected dataset

- package:

  Name of an R package containing datasets

- data:

  Data (used directly as block result)

## Value

All blocks constructed via `new_data_block()` inherit from `data_block`.

## Dataset block

This data block allows to select a dataset from a package, such as the
datasets package available in most R installations as one of the
packages with "recommended" priority. The source package can be chosen
at time of block instantiation and can be set to any R package, for
which then a set of candidate datasets is computed. This includes
exported objects that inherit from `data.frame`.

## Static block

Mainly useful for testing and examples, this block simply returns the
data with which it was initialized. Serialization of static blocks is
not allowed and exported code will not be self-contained in the sense
that it will not be possible to reproduce results in a script that
contains code from a static block.
