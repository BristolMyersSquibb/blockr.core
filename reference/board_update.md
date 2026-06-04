# Board update

Inside
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
every state change flows through one `board_update` reactive. Core
registers two observers framing the change: an initial one that
validates the payload and runs `augment_board_update()` for auto-fixups,
and a final one that runs `apply_board_update()` and resets the
reactive. Plugins or callbacks may register their own observers in
between, provided they use a *finite* priority — the highest and lowest
reactive priorities are reserved for core.

## Usage

``` r
validate_board_update(payload, board, ..., session = get_session())

augment_board_update(upd, board, ..., session = get_session())

apply_board_update(board, upd, ..., session = get_session())
```

## Arguments

- payload, upd:

  A board update payload — see Validation above for the accepted shape.

- board:

  A `board` object.

- ...:

  Forwarded between methods. For `apply_board_update()`, the final
  observer also splices
  [`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)'s
  `...` in here.

- session:

  A shiny session, default
  [`get_session()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.md).

## Value

`validate_board_update()` returns `invisible(payload)` (or throws a
[`blockr_abort()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_abort.md)
error). `augment_board_update()` returns the (possibly extended)
payload. `apply_board_update()` returns a `board`.

## Details

All three functions dispatch on the `board` class. Subclasses override
to validate, augment, or react to their own payload slots, typically
composing with [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html).
`validate_board_update()` is also a caller-facing entry point: it
mirrors the initial observer's checks against a caller-supplied payload,
useful for staging layers (e.g. accumulating LLM-proposed updates) that
need to fail loudly before publishing.

## Validation

The default `.board` method runs a structural check on the payload
(block / link / stack per-slot rules) and a cross-reference check that
link endpoints and stack members resolve in the post-update merged view.
Unknown top-level keys are passed through, so subclass payload slots
reach subclass augment / apply methods.

## Augment

The default `.board` method inserts implied link removals and stack
updates that follow from block removals, plus link-input completion.
Subclass methods may extend the payload with their own fixups; an error
thrown here aborts the update before apply runs.

## Apply

The default `.board` method returns the supplied board unchanged — the
core apply path (block / link / stack mutation, block UI insertion /
removal) is not routed through this generic. Subclass methods receive a
plain `board` snapshot (no reactive surface) and return a `board`, which
the final observer assigns back to `rv$board`. For piecemeal
customization of the core apply path itself, override the relevant
sub-generic
([`modify_board_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md),
[`insert_block_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_ui.md),
etc.) instead.

Errors thrown from either augment or apply are caught by the observer,
reported via
[`notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.md),
and the reactive is reset so the app keeps running.

## Outcome

Alongside the human-facing
[`notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.md)
toast, every update cycle records a machine-readable result into
`board$last_update` (the read-only board handed to plugins and
callbacks). It is a list with a monotonically increasing `seq`, a
logical `ok`, the `phase` it ended in (`"validate"` or `"apply"`), and a
`message`
([`conditionMessage()`](https://rdrr.io/r/base/conditions.html) on
failure, `NA` on success); it is `NULL` before the first update. The
`seq` advances on every write so that two consecutive identical outcomes
still invalidate a downstream observer. A programmatic caller can watch
this field to learn whether a dispatched update was rejected, failed to
apply, or landed.

## Examples

``` r
brd <- new_board(
  blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
  links = links(ab = new_link(from = "a", to = "b"))
)

validate_board_update(
  list(links = list(rm = "ab")),
  brd
)

try(
  validate_board_update(
    list(links = list(add = links(xy = new_link(from = "x", to = "y")))),
    brd
  )
)
#> Error in blockr_abort("Expecting all links to refer to known block IDs.",  : 
#>   Expecting all links to refer to known block IDs.
```
