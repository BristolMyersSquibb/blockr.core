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
block_eval(x, expr, env, ...)

eval_env(data)

block_eval_trigger(x, session = get_session())

block_server(id, x, data = list(), ...)

# S3 method for class 'block'
block_server(
  id,
  x,
  data = list(),
  block_id = id,
  edit_block = NULL,
  ctrl_block = NULL,
  board = reactiveValues(),
  update = reactiveVal(),
  ...
)

expr_server(x, data, ...)

block_render_trigger(x, session = get_session())
```

## Arguments

- x:

  Object for which to generate a
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)

- expr:

  Quoted expression to evaluate in the context of `data`

- env:

  Environment in which to evaluate `expr`

- ...:

  Generic consistency

- data:

  Input data (list of reactives)

- session:

  Shiny session object

- id:

  Namespace ID

- block_id:

  Block ID

- edit_block, ctrl_block:

  Block plugins

- board:

  Reactive values object containing board information

- update:

  Reactive value object to initiate board updates

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

When a front-end (such as blockr.dock) drives the `visible`
write-channel that
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
hands to the board callback, naming the block IDs currently on screen,
evaluation and rendering are gated on visibility. Rendering is gated on
plain visibility: the render observer is suspended while a block is off
screen and resumed once it is on screen, starting suspended so nothing
renders before the front-end first reports. Evaluation is gated on the
*needed* set, the on-screen blocks together with their upstream closure
over
[`board_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md)
(derived from `visible`, recomputed only when it or the links change). A
block's input data reactives stay unfulfilled (they
[`shiny::req()`](https://rdrr.io/pkg/shiny/man/req.html) out) unless the
block is needed, so a block that is neither visible nor feeding a
visible block pulls no input and stays fully quiescent: its result
reactive, and any observer its expression server registers on the
incoming data, all short-circuit and do nothing. A needed but off-screen
block (one feeding a visible block) evaluates but does not render. With
nothing driving `visible` every block is needed and behaviour is
unchanged; the `gate_visibility`
[`blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.md)
(default `TRUE`) turns gating off entirely.
