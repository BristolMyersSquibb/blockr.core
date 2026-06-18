# blockr.core 0.1.3

* New exported `trim_rv()` removes entries from a `reactiveValues`
  object, which assigning `NULL` does not do -- the key lingers in
  `names()` with a `NULL` value. Unlinking a variadic block argument now
  drops it from the block's `...args` outright rather than leaving a
  phantom `NULL` entry behind (#227).
* Boards can be deployed read-only via the `blockr.locked` option,
  enforced server-side rather than by UI hiding (which a forged
  `Shiny.setInputValue` bypasses). While locked, the two channels every
  mutation funnels through -- the `board_update` lifecycle and
  `set_board_option_value()` -- refuse to apply changes.
  `is_board_locked()` is an S3 generic (the default method reads the
  option; a subclass can source the state differently, e.g. an
  authenticated unlock). `set_board_option_value()` gains a required
  `board` argument so the option channel can resolve the lock. Locking
  is not a defense against arbitrary R executing in the session -- which
  can flip the flag or edit the board directly -- so untrusted
  deployments must be isolated at the deployment layer (#229).
* The default `notify_user()` plugin now tracks each block's conditions
  individually rather than re-deriving the whole board's condition frame
  on every change. A single block's condition change updates only the
  notifications it affects, restoring O(block)-per-change work and an
  O(N) rather than O(N^2) cost when a condition cascade touches many
  blocks on a large board. `board$conditions()` is unchanged for
  programmatic consumers (#222).
* `notify()` gains `glue` and `log` arguments (both `TRUE` by default, so
  existing behaviour is unchanged). `glue = FALSE` surfaces literal text
  without [cli::pluralize()] interpolation — so caught condition messages
  containing brace characters no longer error — and `log = FALSE` skips
  the log entry for already-logged text. The board update, link and stack
  error toasts now pass condition messages through with `glue = FALSE`,
  and the default `notify_user()` plugin renders through `notify()`
  (#222).
* Active block conditions (errors, warnings and messages captured during
  evaluation) are now emitted as tidy data frames. Each block server
  returns its conditions as a reactive `server$conditions` (one row per
  active condition, columns `block`, `phase`, `severity`, `message` and
  `id`), and `board_server()` combines these into a board-level reactive
  `board$conditions()` on the read-only board handed to plugins and
  callbacks. A consumer reads one reactive — a single block's frame for
  fine-grained updates, or the whole board — instead of walking the
  nested per-block condition state, and the default `notify_user()`
  plugin surfaces them to the user as toasts. The block server no
  longer returns its raw `cond` reactive values object — read
  `server$conditions()` instead (#217).
* `str_value()` now covers every domain class that has a full-tier
  `format()` / `print()` counterpart, completing the compact rendering
  tier: the scalars `link`, `board_option`, `llm_model_option` and
  `plugin`; the containers `blocks`, `stacks`, `links`, `board_options`
  and `plugins`; and the whole `board`. Each class also gains a
  `utils::str()` method that thinly displays its `str_value()`; a
  container or board renders one element per line below a `<class[n]>`
  header (#212).
* New exported generic `external_ctrl_vars()` (with a `block` method) and
  predicate `has_external_ctrl()` provide a public, polymorphic API for
  resolving a board component's `external_ctrl` declaration into the set of
  externally controllable variable names. This promotes the previously
  internal `block_external_ctrl_vars()`, letting dependent packages dispatch
  on it (e.g. for dock extensions) instead of re-reading the raw
  `external_ctrl` attribute (#192).
* `blockr_deser.list()` now forwards `...` to the dispatched per-class
  method, so callers can thread additional context (e.g. a producer
  version) down to nested deserializers. Previously such arguments were
  silently dropped at the list-dispatch boundary (#186).

# blockr.core 0.1.2

* The `mod` slots in `update(...)` payloads for **blocks, links and
  stacks** now uniformly expect a **delta** shape: a named list keyed
  by entry ID, where each entry is a named list of argument values to
  apply on top of the live entry.
  - For `blocks`, keys must be in `block_external_ctrl_vars(blk)` —
    non-ctrl-able changes go through `rm` + `add`. Ctrl-arg writes hit
    the corresponding `reactiveVal` in place; `block_name` (always
    treated as ctrl-able) updates the block's registry attribute.
  - For `links` and `stacks`, deltas are merged onto the current entry
    via the new `update_link()` / `update_stack()` S3 generics. The
    default methods reconstruct the entry through its stored
    constructor, preserving sub-class attributes. Sub-class owners
    (e.g. `dock_stack` adding `color`) only need to register a method
    when their constructor deviates from the convention (#175).
* `block_external_ctrl_vars()` always includes `"block_name"` — every
  block can be renamed through `update(...)` regardless of its
  `external_ctrl` opt-in. The default ctrl plugin panel renders a
  `block_name` field on every block card alongside any opted-in ctrl
  vars. `block_supports_external_ctrl()` (which would now be `TRUE` for
  every block) has been removed; the gates that used to call it have
  been simplified accordingly.
* The default `block_server.block` now constructs a `block_name`
  `reactiveVal` per block and appends it to the `vars` list passed to the
  ctrl plugin. Two guarded observers keep that `reactiveVal` in sync with
  the block's `block_name` attribute on the board (one emits an
  `update(mod = ...)` when the ctrl plugin writes the rv; the other
  pulls registry-attr changes back into the rv). The `ctrl_block_server`
  plugin signature is unchanged — block authors and custom ctrl plugins
  see uniform `vars` reactiveVals.
* A blockr option `attach_default_packages` can be set to opt into evaluating
  block expressions with objects from default packages directly available.
* Add `ctrl_block()` plugin for external block control, allowing blocks to be
  driven programmatically from outside the standard block UI.
* Add `clear_board()` for removing all blocks/stacks from a board.
* Add `bbquote()` and helpers (`.`, `..`) for cleaner code generation via
  `bquote()`-based quasiquotation.
* Export `custom_plugins()` and `custom_options()` for easier board
  customization.
* Add `block_metadata()` for retrieving per-block metadata and attach block
  metadata with defaults to block objects.
* Export test utilities (`blockr_test_exports()`, `new_mock_session()`,
  `export_safely()`) for use in downstream package tests.

# blockr.core 0.1.1

* Add Block-level notifications via (optional) `expr` server return value
  component `cond`.
* Export `get_board_option_value()` to make available current option settings
  via `session$UserData`.
* Introduce (optional) dependency on [thematic](
  https://rstudio.github.io/thematic/) to auto-style plots.
* Export `toolbar_ui()` which takes case of the "core" toolbar UI component.
* Utility functions `chr_ply()` and related, as well as miscellaneous utilities
  such as `set_names()`, `coal()`, etc. are now exported for use in dependent
  packages.
* Export `export_code()` to make it easier for third-party `generate_code()`
  plugin implementations.
* Use `evaluate::evaluate()` to capture plots.
* Add `new_fixed_block()` for applying a fixed (i.e. non-paramtetrized)
  transformation to data input.
* Board server callbacks are invoked with an additional argument `session`.
* Export assertion utilities such as `is_string()`, `is_count()`, etc.
* Improved ser/des, which now includes package/constructor information for all
  board, blocks, stacks and options. The corresponding infra
* Board options now contain UI/server components to provide more options for
  customization. Also blocks can require certain options to be available.
* Introduces `block_render_trigger()` to control per block class when to
  re-render the block output.
* Rework of block notifications to provide a `reactiveValues()` object
  containing notification types a separate components.
* Auto-ID generation can now be customized with a default provided by the ids
  package (if available).
* Use the glue package for logging/block notifications; add a glue-based text
  block.
* New board restore mechanism based on `session$reload()`.
* Improvements to the registry: block icons and a fixed set of categories.

# blockr.core 0.1.0

* Initial CRAN submission
