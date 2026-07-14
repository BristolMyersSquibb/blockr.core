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
  inputs_ready = reactive(TRUE),
  visibility = NULL,
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

- inputs_ready:

  Reactive flag signaling whether the block's required inputs are all
  connected to ready upstream blocks (supplied by
  [`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md);
  defaults to always-ready when a block server is run standalone)

- visibility:

  Visibility channel bundle – a list with two channels, `required` and
  `visible`, each an environment of per-block `reactiveVal`s, supplied
  by
  [`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
  to gate rendering; `NULL` (the standalone default) leaves the block
  ungated

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

Each block carries an *eval status* – one of `dormant`, `waiting`,
`unset`, `failed` or `ready` – which, together with its orthogonal
front-end visibility, determines its behaviour. The status separates the
two input kinds (data inputs from links, user inputs from `state`) and a
genuine failure:

- `dormant` – not *needed* (neither on screen nor feeding, transitively
  over
  [`board_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md),
  an on-screen block); inputs stay unfulfilled
  ([`shiny::req()`](https://rdrr.io/pkg/shiny/man/req.html) out) and
  nothing evaluates.

- `waiting` – needed, but a required *data* input is missing:
  unconnected, below the required number of variadic `...args` inputs
  (one by default), or fed by an upstream block that is not itself
  `ready` (see `allow_empty_state`).

- `unset` – data inputs are ready, but a required *user* input (`state`
  value) has not been provided (unless permitted by
  `allow_empty_state`).

- `failed` – all inputs are present, but the block cannot produce a
  result: the data validator
  ([`validate_data_inputs()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.md))
  or the block expression raised. The offending condition is surfaced
  through the block conditions.

- `ready` – evaluation succeeded and a result (possibly a legitimate
  `NULL`) is available for downstream blocks to consume.

A block reaches `ready` only once its upstreams have, so an unconnected
or pending block holds its whole downstream chain `waiting` without any
of them evaluating against missing data. Output rendering follows the
status: the block output is shown only while `ready` and cleared
otherwise, so a block leaving `ready` never displays a stale result.
While not `ready` the block surfaces a condition explaining why – a
`status`-phase note for `waiting` and `unset`, or the raised error for
`failed`. Conditions raised during validation and evaluation are caught
and returned to be surfaced to the app user.

Block-level user inputs (provided by the expression module) are
separated from output, the behavior of which can be customized via the
[`block_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md)
generic. The
[`block_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md)
generic can then be used to control rendering of outputs.

A front-end (such as blockr.dock) drives two per-block channels that
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
hands to the board callback as `visibility`: `required` (which blocks it
needs built and evaluated) and `visible` (which blocks it has arranged
on screen). Requirements are a cause the front-end – and core-side
features such as code export – declare; visibility is the effect the
front-end reports back once it has painted a block. Rendering is gated
on `visible`: the render observer is suspended while a block carries no
visible slot and resumed once the front-end writes a non-empty string
for it, starting suspended so nothing renders before the first report.
Evaluation is gated on the *needed* set, the `required` blocks together
with their upstream closure over
[`board_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md)
(recomputed only when requirements or links change). A block's input
data reactives stay unfulfilled (they
[`shiny::req()`](https://rdrr.io/pkg/shiny/man/req.html) out) unless the
block is needed, so a block that is neither required nor feeding a
required block pulls no input and stays fully quiescent: its result
reactive, and any observer its expression server registers on the
incoming data, all short-circuit and do nothing. A needed but off-screen
block (one feeding a required block) evaluates but does not render.
Block-server *construction* is prioritized the same way: the needed set
is instantiated first so that first paint waits only for the required
blocks and their upstreams, and the remaining block servers are built
progressively in the background. That background pass holds until the
front-end reports every required block as visible, so it never competes
with first paint. A `required` slot of `FALSE` keeps a block built but
dormant (ever required, not needed now); an absent slot leaves it
unbuilt. Until a block is built it is absent from the `board$blocks`
handed to plugins and callbacks, which simply see it appear once
constructed. The background cadence is set by the
`background_construction_delay`
[`blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.md)
(milliseconds between successive blocks, default 50); a value of 0
disables the staggering and builds every block up front. With nothing
writing `required` every block is needed and behaviour is unchanged; the
`gate_visibility`
[`blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.md)
(default `TRUE`) turns gating off entirely.
