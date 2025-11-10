# Block server

A block is represented by several (nested) shiny modules and the top
level module is created using the `block_server()` generic. S3 dispatch
is offered as a way to add flexibility, but in most cases the default
method for the `block` class should suffice at top level. Further entry
points for customization are offered by the generics `expr_server()` and
`block_eval()`, which are responsible for initializing the block
"expression" module (i.e. the block server function passed in
[`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md))
and block evaluation (evaluating the interpolated expression in the
context of input data), respectively.

## Usage

``` r
block_server(id, x, data = list(), ...)

# S3 method for class 'block'
block_server(
  id,
  x,
  data = list(),
  block_id = id,
  edit_block = NULL,
  board = reactiveValues(),
  update = reactiveVal(),
  ...
)

expr_server(x, data, ...)

block_eval(x, expr, data, ...)

block_eval_trigger(x, session = get_session())

block_render_trigger(x, session = get_session())
```

## Arguments

- id:

  Namespace ID

- x:

  Object for which to generate a
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)

- data:

  Input data (list of reactives)

- ...:

  Generic consistency

- block_id:

  Block ID

- edit_block:

  Block edit plugin

- board:

  Reactive values object containing board information

- update:

  Reactive value object to initiate board updates

- expr:

  Quoted expression to evaluate in the context of `data`

- session:

  Shiny session object

## Value

Both `block_server()` and `expr_server()` return shiny server module
(i.e. a call to
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)),
while `block_eval()` evaluates an interpolated (w.r.t. block "user"
inputs) block expression in the context of block data inputs.

## Details

The module returned from `block_server()`, at least in the default
implementation, provides much of the essential but block-type agnostic
functionality, including data input validation (if available),
instantiation of the block expression server (handling the
block-specific functionality, i.e. block user inputs and expression),
and instantiation of the `edit_block` module (if passed from the parent
scope).

A block is considered ready for evaluation whenever input data is
available that satisfies validation
([`validate_data_inputs()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.md))
and nonempty state values are available (unless otherwise instructed via
`allow_empty_state` in
[`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)).
Conditions raised during validation and evaluation are caught and
returned in order to be surfaced to the app user.

Block-level user inputs (provided by the expression module) are
separated from output, the behavior of which can be customized via the
[`block_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md)
generic. The
[`block_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md)
generic can then be used to control rendering of outputs.
