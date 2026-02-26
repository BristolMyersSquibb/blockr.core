# Blocks

Steps in a data analysis pipeline are represented by blocks. Each block
combines data input with user inputs to produce an output. In order to
create a block, which is implemented as a shiny module, we require a
server function, a function that produces some UI and a class vector.

## Usage

``` r
new_block(
  server,
  ui,
  class,
  ctor = sys.parent(),
  ctor_pkg = NULL,
  dat_valid = NULL,
  allow_empty_state = FALSE,
  block_name = default_block_name,
  expr_type = c("quoted", "bquoted"),
  external_ctrl = FALSE,
  block_metadata = NULL,
  ...
)

default_block_name(class)

is_block(x)

as_block(x, ...)

blocks(...)

is_blocks(x)

as_blocks(x, ...)
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

- ctor_pkg:

  String-valued package name when passing a string-valued constructor
  name or `NULL`

- dat_valid:

  (Optional) input data validator

- allow_empty_state:

  Either `TRUE`, `FALSE` or a character vector of `state` values that
  may be empty while still moving forward with block eval

- block_name:

  Block name

- expr_type:

  Expression type (experimental)

- external_ctrl:

  Set up external control (experimental)

- block_metadata:

  Block metadata

- ...:

  Further (metadata) attributes

- x:

  An object inheriting from `"block"`

## Value

Both `new_block()` and `as_block()` return an object inheriting from
`block`, while `is_block()` returns a boolean indicating whether an
object inherits from `block` or not. Block vectors, created using
`blocks()`, `as_blocks()`, or by combining multiple blocks using
[`base::c()`](https://rdrr.io/r/base/c.html) all inherit frm `blocks`
and `iss_block()` returns a boolean indicating whether an object
inherits from `blocks` or not.

## Details

A block constructor may have arguments, which taken together define the
block state. It is good practice to expose all user-selectable arguments
of a block (i.e. everything excluding the "data" input) as block
arguments such that block can be fully initialized via the constructor.
Some default values are required such that blocks can be constructed via
constructor calls without arguments. Where it is sensible to do so,
specific default values are acceptable, but if in any way data
dependent, defaults should map to an "empty" input. For example, a block
that provides [`utils::head()`](https://rdrr.io/r/utils/head.html)
functionality, one such argument could be `n` and a reasonable default
value could be `6L` (in line with corresponding default S3 method
implementation). On the other hand, a block that performs a
[`base::merge()`](https://rdrr.io/r/base/merge.html) operation might
expose a `by` argument, but a general purpose default value (that does
not depend on the data) is not possible. Therefore,
[`new_merge_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.md)
has `by = character()`.

The return value of a block constructor should be the result of a call
to `new_block()` and `...` should be contained in the constructor
signature such that general block arguments (e.g. `name`) are available
from the constructor.

## Server

The server function (passed as `server`) is expected to be a function
that returns a
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
This function is expected to have at least an argument `id`
(string-valued), which will be used as the module ID. Further arguments
may be used in the function signature, one for each "data" input. A
block implementing [`utils::head()`](https://rdrr.io/r/utils/head.html)
for example could have a single extra argument `data`, while a block
that performs [`base::merge()`](https://rdrr.io/r/base/merge.html)
requires two extra arguments, e.g. `x` and `y`. Finally, a variadic
block, e.g. a block implementing something like
[`base::rbind()`](https://rdrr.io/r/base/cbind.html), needs to
accommodate for an arbitrary number of inputs. This is achieved by
passing a
[`shiny::reactiveValues()`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
object as `...args` and thus such a variadic block needs `...args` as
part of the server function signature. All per-data input arguments are
passed as
[`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html) or
[`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
objects.

The server function may implement arbitrary shiny logic and is expected
to return a list with components `expr` and `state`. The expression
corresponds to the R code necessary to perform the block task and is
expected to be a reactive quoted expression. It should contain
user-chosen values for all user inputs and placeholders for all data
inputs (using the same names for data inputs as in the server function
signature). Such an expression for a
[`base::merge()`](https://rdrr.io/r/base/merge.html) block could be
created using [`base::bquote()`](https://rdrr.io/r/base/bquote.html) as

    bquote(
      merge(x, y, by = .(cols)),
      list(cols = current_val())
    }

where `current_val()` is a reactive that evaluates to the current user
selection of the `by` columns. This should then be wrapped in a
[`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html) call
such that `current_val()` can be evaluated whenever the current
expression is required.

The `state` component is expected to be a named list with either
reactive or "static" values. In most cases, components of `state` will
be reactives, but it might make sense in some scenarios to have
constructor arguments that are not exposed via UI components but are
fixed at construction time. An example for this could be the
`dataset_block` implementation where we have constructor arguments
`dataset` and `package`, but only expose `dataset` as UI element. This
means that `package` is fixed at construction time. Nevertheless,
`package` is required as state component, as this is used for
re-creating blocks from saved state.

State component names are required to match block constructor arguments
and re-creating saved objects basically calls the block constructor with
values obtained from block state.

## UI

Block UI is generated using the function passed as `ui` to the
`new_block` constructor. This function is required to take a single
argument `id` and shiny UI components have to be namespaced such that
they are nested within this ID (i.e. by creating IDs as
`shiny::NS(id, "some_value")`). Some care has to be taken to properly
initialize inputs with constructor values. As a rule of thumb, input
elements exposed to the UI should have corresponding block constructor
arguments such that blocks can be created with a given initial state.

Block UI should be limited to displaying and arranging user inputs to
set block arguments. For outputs, use generics
[`block_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md)
and
[`block_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md).

## Sub-classing

In addition to the specific class of a block, the core package uses
virtual classes to group together blocks with similar behavior (e.g.
`transform_block`) and makes use of this inheritance structure in S3
dispatch for methods like
[`block_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md)
and
[`block_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md).
This pattern is not required but encouraged.

## Initialization/evaluation

Some control over when a block is considered "ready for evaluation" is
available via arguments `dat_valid` and `allow_empty_state`. Data input
validation can optionally be performed by passing a predicate function
with the same arguments as in the server function (not including `id`)
and the block expression will not be evaluated as long as this function
throws an error.

Other conditions (messages and warnings) may be thrown as will be caught
and displayed to the user but they will not interrupt evaluation. Errors
are safe in that they will be caught as well but the will interrupt
evaluation as long as block data input does not satisfy validation.

## Block vectors

Multiple blocks can be combined into a `blocks` object, a container for
an (ordered) set of blocks. Block IDs are handled at the `blocks` level
which will ensure uniqueness.

## Examples

``` r
new_identity_block <- function() {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(quote(identity(data))),
            state = list()
          )
        }
      )
    },
    function(id) {
      htmltools::tagList()
    },
    class = "identity_block"
  )
}

blk <- new_identity_block()
#> Warning: No block metadata available for block identity_block.
is_block(blk)
#> [1] TRUE

blks <- c(a = new_dataset_block(), b = new_subset_block())

is_block(blks)
#> [1] FALSE
is_blocks(blks)
#> [1] TRUE

names(blks)
#> [1] "a" "b"

tryCatch(
  names(blks["a"]) <- "b",
  error = function(e) conditionMessage(e)
)
#> [1] "Replacing IDs `a` with `b` is not allowed."
```
