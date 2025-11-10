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

- callbacks:

  Single (or list of) callback function(s), called only for their
  side-effects)

- callback_location:

  Location of callback invocation (before or after plugins)

## Value

A `board_server()` implementation (such as the default for the `board`
base class) is expected to return a
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
