# Board loader

Which `board` to build for an incoming request – and how a board is
carried across the `session$reload()` that a
[preserve_board](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.md)
restore triggers – is the job of an app-level **board loader**, passed
to
[`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
as its `loader` argument. A `board_loader()` pairs a
`resolve(request, session, default)` – returning the `board` to build
for an incoming request, or `NULL` for the
[`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
default – with an optional `stage(board, session)`, which persists a
board and returns the URL query parameters that reference it (a
resolve-only loader, e.g. one not backing a restore, leaves `stage`
`NULL`).
[`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
uses that one loader for both the request-phase resolution (at the GET,
where `session` is `NULL`, and at the WS connect) and the in-session
staging when a restore fires; **core** writes those parameters into the
URL and drives the reload, so the reload stays a guaranteed core
mechanism that no loader can opt out of.

## Usage

``` r
board_loader(resolve, stage = NULL)

is_board_loader(x)

local_loader()
```

## Arguments

- resolve, stage:

  Paired functions backing a `board_loader`: `resolve` is
  `function(request, session, default)` returning the `board` to build
  or `NULL`; `stage` is `function(board, session)` (or `NULL` for a
  loader that does not stage, e.g. resolve-only) which persists `board`
  and returns the URL query parameters referencing it (core writes them
  and reloads). They share private state, so a `board_loader` is built
  as a unit, not supplied as two loose functions.

- x:

  Object to test for `board_loader`-ness

## Value

`board_loader()` and `local_loader()` return a `board_loader` object and
`is_board_loader()` a scalar logical.

## Details

`resolve` receives the raw HTTP `request` at both phases (the UI request
at the GET, `session$request` at the WS), the `session` (`NULL` at the
GET), and `default` – the `board` passed to
[`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
(the
[`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
default, built when `resolve` returns `NULL`), which a loader can also
derive its own result from (e.g. `clear_board(default)`). A loader that
keys off URL query parameters reads them itself, minding the phase
split: the query is on `request$QUERY_STRING` at the GET but
`session$clientData$url_search` at the WS (the websocket request carries
neither).

The default `local_loader()` keeps its handoff in a per-loader store (no
process global) and is therefore single-process; multi-user deployments
pass a loader resolving from a shared backend (as blockr.session does).
