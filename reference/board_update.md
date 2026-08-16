# Board update

Inside
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
every state change, and every request a consumer makes of the board,
flows through one `board_update` reactive. Core registers two observers
framing the change: an initial one that validates the payload and runs
`augment_board_update()` for auto-fixups, and a final one that runs
`apply_board_update()` and resets the reactive. Plugins or callbacks may
register their own observers in between, provided they use a *finite*
priority — the highest and lowest reactive priorities are reserved for
core.

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

## Request components

Three components carry a request rather than a state change: `evaluate`,
a character vector of block IDs to evaluate once; `sustain`, a list of
per-owner deltas over the blocks that are to stay evaluated; and
`construct`, a character vector of block IDs to build without
evaluating. Each `sustain` delta is `set`, `add` and `rm` — `set` states
that owner's whole set and is exclusive with the other two — so no owner
writes another's claim. The two evaluation components put the named
blocks (and their upstream closure) into the eval set while `construct`
leaves them `dormant`, and none of the three touches what the front-end
shows — see the Evaluation requests and Construction requests sections
of
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md).
All three resolve their IDs against the post-update block set, so a
payload may add a block and ask for it in one go. A `sustain` `rm` is
the exception, naming blocks to release rather than to evaluate, and so
may name one the board no longer has. They are applied after the state
delta, so a payload that edits a block and evaluates it sees the edit.

The three are independent sets rather than alternatives: a payload may
name one block in several of them and core takes the union. Overlap is
redundant rather than rejected, which it has to be — claims are
per-owner, so a consumer asking for a block cannot know that another
owner already holds it.

A locked board (see
[`is_board_locked()`](https://bristolmyerssquibb.github.io/blockr.core/reference/locked-board.md))
still accepts a payload of request components alone; one that also
carries a state change is dropped whole rather than applied in part.

## Augment

The default `.board` method inserts implied link removals and stack
updates that follow from block removals, plus link-input completion.
Subclass methods may extend the payload with their own fixups; an error
thrown here aborts the update before apply runs.

## Apply

The default `.board` method applies the core delta to the supplied board
and returns it: added blocks are appended, link and stack deltas are
folded in through
[`modify_board_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md)
/
[`modify_board_stacks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md),
and removed blocks are dropped last (once the earlier steps have freed
them of every link and stack). Subclass methods compose with
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) to layer their
own payload slots on top of the core-updated board (blockr.dock, for
instance, cascades view membership), so a single `apply_board_update()`
call yields the full board for any subclass. The board handed in is a
plain `board` snapshot with no reactive surface; the returned `board` is
what the final observer assigns back to `rv$board`. The reactive side
effects that mirror the delta — block UI insertion / removal, server
construction / teardown, link and stack wiring — run around this single
reduce.

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

## See also

On a locked board (see
[`is_board_locked()`](https://bristolmyerssquibb.github.io/blockr.core/reference/locked-board.md))
the update is dropped rather than applied.

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
