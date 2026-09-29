# Board server

A call to `board_server()`, dispatched on objects inheriting from
`board`, returns a
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html),
containing all necessary logic to manipulate board components via UI.
Extensibility over currently available functionality is provided in the
form of S3, where a `board_server()` implementation of `board`
sub-classes may be provided, as well as via a plugin architecture and
callback functions which can be used to register additional observers.

## Usage

``` r
board_server(id, x, ...)

# S3 method for class 'board'
board_server(
  id,
  x,
  plugins = board_plugins(x),
  options = board_options(x),
  callbacks = gate_stacks(),
  callback_location = c("end", "start"),
  ...
)

eager(owner, blocks = character())

gate_stacks()
```

## Arguments

- id:

  Parent namespace

- x:

  Board

- ...:

  Generic consistency

- plugins:

  Board plugins as modules

- options:

  Board options (`NULL` defaults to the union of board, block and
  registry sourced options)

- callbacks:

  Single (or list of) callback function(s) registering additional
  observers. A board is eager by default: it evaluates every block. A
  callback makes it lazy by returning `eager(owner, blocks)`, on its own
  or as one element of a list whose other elements are passed on to
  plugins as usual. Core then evaluates only the blocks some owner holds
  eager, and what feeds them; it seeds `blocks` as that owner's eager
  set before the first flush, and at most one callback may return one.
  Changes from then on travel as an `eager` component under the same
  owner label through the `update` channel each callback receives (see
  [board_update](https://bristolmyerssquibb.github.io/blockr.core/reference/board_update.md)
  and the Evaluation requests section). Each callback also receives a
  `visibility` list of the per-block channels `visible` and `frozen`,
  environments of `reactiveVal`s (core keeps one per board block as
  blocks are added and removed). Report whether a block is currently
  painted with `visibility$visible[[id]](TRUE)` (or `FALSE` once built
  but off screen, leaving `NA` until it is first built); the board
  renders a block only once it is reported painted, and holds background
  construction until every block the front-end holds eager is. Set
  `visibility$frozen[[id]](TRUE)` to freeze a block's inputs (for
  example when its controls are hidden), so a forged input can no longer
  steer it.

  Core's own front-end drives these channels through a callback like any
  other: `gate_stacks()` reads the stack accordion (see
  [`stack_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/stack_ui.md))
  and is the default, so a stacked board that renders core's UI is lazy,
  and a board that does not render it is left alone – it passes its own
  callbacks. A consumer that wants both keeps it in the list rather than
  replacing it – `callbacks = list(gate_stacks(), my_callback)`, which
  is for a front-end that does render the accordion: on a stacked board
  it returns the blocks of the initially open stacks as its eager set,
  and reads that input only to refine it.

- callback_location:

  Location of callback invocation (before or after plugins)

- owner:

  Label under which the front-end holds its blocks eager, as it would
  name itself in an `eager` component

- blocks:

  Block IDs the front-end needs evaluated from the start

## Value

A `board_server()` implementation (such as the default for the `board`
base class) is expected to return a
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).

## Active conditions

Conditions raised while blocks evaluate (errors, warnings and messages)
are exposed as a reactive data frame `board$conditions` on the read-only
board handed to plugins and callbacks, with one row per active condition
and columns `block`, `phase`, `severity`, `message` and `id`. It
combines the per-block `server$conditions` reactives (see
[`block_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_server.md)),
so a consumer reads a single reactive — the whole board, or one block's
frame for fine-grained updates — rather than walking nested condition
state. The default
[`notify_user()`](https://bristolmyerssquibb.github.io/blockr.core/reference/notify_user.md)
plugin renders its toasts from this source.

## Evaluation requests

Deferred evaluation leaves a block that nothing currently needs holding
its last check — the status it reached, its result and the conditions it
reports (see
[`block_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_server.md)).
Anything that can reach the
[`board_update()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_update.md)
channel can ask for such a block to be brought up to date, without
putting it on screen, through the `evaluate` and `eager` payload
components. Both name blocks, and core joins them, together with their
upstream closure over
[`board_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md)
(without which they cannot produce a result), to the eval set. They
differ only in who lets go: an `evaluate` request is a one-off that core
drops once the block has run, while a block held `eager` stays evaluated
until its owner releases it.

Eager blocks are keyed by owner, the `eager` component mapping each
owner to a delta over the blocks it holds, so several consumers may hold
the same block and none of them overwrites another's set:

    update(
      list(
        eager = set_names(
          list(list(set = board_block_ids(board$board))),
          session$ns("preview")
        )
      )
    )

A delta is `set`, `add` and `rm`, of which `set` states that owner's
entire set at once and cannot be combined with the other two. Releasing
everything is `set = character()`; releasing some blocks is `rm`, which
— unlike `set` and `add` — may name a block the board no longer has, so
a release cannot be rejected by a removal that raced it. Restating a set
repairs a release that never arrived, rather than letting it accumulate.

Core cannot infer the owner — the write and its effect are separated by
a flush — so the label travels in the payload. Nothing keys off shiny's
namespacing, but taking the label from `session$ns()` as above is what
keeps owners unique without a registry, and lets one module hold two
independent sets under two labels. An owner's set outlives the module
that made it: core drops a block from every set once it leaves the
board, but an owner that goes away without releasing holds what it held
for the rest of the session.

Holding a block eager asks for evaluation and nothing else: nothing
about what is on screen changes. The front-end is an owner like any
other – it holds its blocks eager under the label it declared with
`eager()` (see
[`block_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_server.md))
– so core never distinguishes its demand from any other owner's, and a
consumer holding a block eager cannot park what the front-end is
showing. Because these requests carry no state change, they are also the
one part of a payload a locked board still accepts.

Core drops a one-off request once the block has run — or has reported
why it cannot, such as an unconnected data input or a user input that
was never set. Either way the block has been checked against everything
it depends on, so once it leaves the eval set it goes on reporting what
that check found, until one of those changes (see
[`block_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_server.md)).
A request for a parked block that is current is spent at once, since its
status already reports what a run would find, and requesting a block
that is already in the eval set does nothing.

## Construction requests

Evaluation implies construction, but not the reverse: a consumer that
needs a block merely *present* — the code export reads each block's
expression and none of their results — had to make it run as well. The
`construct` payload component asks for construction on its own. Like
`evaluate` it is a bare character vector of block IDs, and the blocks it
names are built in dependency order and left `unevaluated`:

    update(list(construct = board_block_ids(board$board)))

Nothing is retained. Once a block is built it stays built, so unlike the
two evaluation components there is no owner to name and nothing to hand
back, and asking for a block that is already built does nothing. The
request joins neither the eval set nor any owner's eager set, so it
cannot turn a lazily evaluating board into an eagerly evaluating one.

A block that the same payload adds, or that an `evaluate` or `eager`
names, is already constructed — the add builds it directly, and
evaluation demand joins the needed set, which the background constructor
builds. Pairing `construct` with either is redundant rather than wrong.
The component covers what neither does: a block that must exist while
nothing needs it evaluated.

The named blocks are built in the flush that applies the payload, which
is the work `background_construction_delay` otherwise paces out. A
caller that wants that pacing sends several smaller payloads rather than
one.
