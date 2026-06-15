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
  callbacks = list(),
  callback_location = c("end", "start"),
  ...
)
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

  Single (or list of) callback function(s), called only for their
  side-effects)

- callback_location:

  Location of callback invocation (before or after plugins)

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
