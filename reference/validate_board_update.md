# Validate a board update payload

Run the same validation logic that the
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
observer applies to every `board_update` payload, but against a
caller-supplied payload value and board. Useful for downstream packages
that want to surface validation errors before committing a proposed
update — for example a staging layer that accumulates pending changes
from LLM tool calls and needs each call to fail loudly if it would
conflict with the live board.

## Usage

``` r
validate_board_update(payload, board)
```

## Arguments

- payload:

  A list of the shape accepted by the `board_update` reactiveVal, with
  optional `blocks`, `links`, and `stacks` slots, each a list with
  optional `add`, `mod`, and `rm` entries.

- board:

  A `board` object to validate the payload against.

## Value

`invisible(payload)` on success. On failure, throws a
[`blockr_abort()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_abort.md)
error with a class such as `board_update_*_invalid`.

## Details

Validation runs in two passes: a structural check on the payload shape
and per-slot block / link / stack rules, followed by a cross-reference
check that link `from`/`to` endpoints and stack members resolve in the
post-update merged view.

Preprocessing (auto-cleanup of dangling refs from block removals) is an
apply-time concern and is **not** performed here — the payload is
validated as-is.

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
