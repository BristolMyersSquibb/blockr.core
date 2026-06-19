# Locked boards

A board can be deployed read-only by setting the `blockr.locked` option
(see
[`blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.md)).
Locking is enforced server-side, not by UI hiding: a forged
`Shiny.setInputValue` bypasses any `display: none` and fires the
underlying observer, so the refusal has to happen on the server. While
locked, the two channels every mutation funnels through – the
`board_update` lifecycle (see
[`board_update()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_update.md))
and
[`set_board_option_value()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.md)
– refuse to apply changes.

## Usage

``` r
is_board_locked(board, ...)

# S3 method for class 'board'
is_board_locked(board, ...)
```

## Arguments

- board:

  A `board` object to dispatch on.

- ...:

  Generic consistency.

## Value

A logical flag.

## Details

`is_board_locked()` reports whether a board is locked and dispatches on
the board, so a subclass can source the state however it likes (for
example gating an unlock behind an authenticated request). The default
`board` method reads the `blockr.locked` option, and `blockr.dock`'s
`is_dock_locked()` delegates here.

## Scope

Locking guards against forged *client input* – a browser that sets Shiny
inputs but cannot run server-side R. It is **not** a defense against
code executing in the session: blockr evaluates arbitrary R, which can
flip the option, rebind this generic or mutate board state directly, and
no in-process flag can prevent that. A subclass method may raise the bar
(e.g. an authenticated unlock) but cannot close the gap on its own – a
hard boundary has to live where the user's code does not run (a
sandboxed evaluator, or an ephemeral read-only deployment). Treat
locking as a guard for non-malicious users.

## Examples

``` r
is_board_locked(new_board())
#> [1] FALSE
```
