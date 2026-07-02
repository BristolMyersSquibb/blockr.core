# blockr.core 0.1.3

* Block registration metadata can now be declared with roxygen2 tags
  (`@block`, `@blockDescr`, `@blockCategory`, `@blockIcon`,
  `@blockGuidance`, `@blockKeywords`, `@blockDetails`, `@blockLink`,
  `@blockArg`, `@blockExamples` and `@blockCtor`) directly on a block
  constructor (#140). A new roclet, `block_registration_roclet()`, gathers
  them into `inst/registry/blocks.yml`, which the newly exported
  `register_package_blocks()` reads at load time. `register_core_blocks()`
  now sources its metadata from that registry instead of a hardcoded call,
  so adding a block to the registry is a matter of annotating its
  constructor. Extension packages can opt in by adding the roclet to their
  `DESCRIPTION` `Roxygen` field and calling `register_package_blocks()` from
  `.onLoad()`.
* The core blocks now carry richer registration metadata: model-facing
  `guidance`, search `keywords` (comma-separated, so a term may contain
  spaces), `details` (falling back to the constructor's `@section` prose
  when `@blockDetails` is omitted), a documentation `link` (derived from the
  package's pkgdown `url` and the documented topic when `@blockLink` is
  omitted), and per-argument JSON-schema `type` descriptors alongside
  descriptions and worked examples. Each argument is declared with one `@blockArg <name>
  <description>` tag; optional `[example] <expr>` and `[type] <expr>`
  markers give R expressions (evaluated at documentation time) for a worked
  example and an `arg_*()` type descriptor, so a typed argument can carry a
  typed example. Block-level worked configurations are declared with
  `@blockExamples`.
* The manage-links and manage-stacks plugins no longer flicker the table's
  cell selectizes on a board re-emit (#246). The observer that keeps the
  table in sync re-rendered every row whenever `upd$curr` was invalidated --
  `observeEvent(names(upd$curr))` fires on each re-emit, not only when the
  link or stack id set changes -- so `DT::replaceData` ran unconditionally,
  briefly blanking the cell inputs before the async redraw landed. The
  redraw is now guarded on the id set, so it runs only when a link or stack
  is actually added or removed; value edits and no-op re-emits are skipped.
* The manage-links and manage-stacks plugins no longer clobber staged
  edits on a board re-emit (#246). The board-sync observers replaced the
  working copy on every re-emit, dropping a half-finished staged add or
  edit while its entry lingered in `upd$add` / `upd$mod`. A new
  `merge_staged_links()` / `merge_staged_stacks()` overlay now merges the
  refreshed applied state with the staged add / edit / rm deltas instead of
  replacing it, mirroring the blockr.dock edit-board extension.
* The manage-links and manage-stacks plugins now dedupe board re-emits
  (#246). The board-sync observers keyed off `board_links()` /
  `board_stacks()` fired on each board invalidation -- `observeEvent` does
  not value-dedupe -- so a fresh board object carrying byte-identical links
  or stacks still re-ran the handler. A new shared
  `deduped_board_reactive()` helper wraps each board accessor in a
  `reactiveVal` + `identical()` guard and keys the re-sync off the deduped
  value, so a board update that leaves the links or stacks untouched never
  reaches the plugin.

* Block registry entries gained a structured argument specification, built
  with the new exported `new_block_args()` / `new_block_arg()`, carrying a
  per-argument `description`, a single worked `example`, and an optional
  machine-readable `type`. `register_block()` additionally accepts block-level
  `details`, `link`, `guidance`, `examples`, and `keywords`. The construction
  metadata formerly smuggled as `examples` / `prompt` attributes on `arguments`
  is now first-class and validated at registration (every argument is a real
  constructor formal and every worked example actually constructs); the legacy
  attributes are still absorbed, with a deprecation warning (#121).
* An argument's `type` is a dependency-free JSON-Schema-subset descriptor built
  with `arg_string()`, `arg_number()`, `arg_integer()`, `arg_boolean()`,
  `arg_enum()`, `arg_array()` and `arg_object()` (consumable directly, e.g. via
  `ellmer::type_from_schema()`), replacing the opaque `ellmer::type_*` slot.
  Registration now validates the descriptor and checks each worked example
  against it, so a malformed-but-constructible value is rejected (#121).
* Block metadata is exposed two ways. `block_metadata()` returns a
  `data.frame` over one or many blocks -- dispatching on a `block`, a `blocks`
  collection, a `block_registry_entry` or a registry ID -- with every attribute
  available as a column (the multi-valued `arguments`, `examples` and
  `keywords` as list-columns) and a `fields` argument to select a subset. Each
  attribute additionally has a single-block getter: `block_meta_name()`,
  `block_meta_description()`, `block_meta_guidance()`, `block_meta_arguments()`,
  `block_meta_keywords()`, and so on. A single argument's fields are read with
  `block_arg_description()`, `block_arg_example()` and `block_arg_type()`.
  `registry_metadata()` is deprecated in favour of these (#121).
* Board accessors (`board_blocks()`, `board_links()`, `board_stacks()`)
  are now pure reads. They previously re-validated their entire
  collection on every call, and because board setup reads links once per
  block, that cost scaled quadratically and dominated startup of large
  boards. Validation is unchanged at construction and on mutation, and
  `validate_board()` still checks each collection in full (#241).
* Board options contributed by blocks or the registry (e.g. the
  preview-row count) are no longer reset to their defaults on save and
  reload. The settings sidebar manages the wider `blockr_app_options()`
  set, but a board carried only its own `board_options()`, and that
  narrow set is what `serialize_board()` persisted — so a user's change
  to a block-backed option was dropped. `resolve_board()` now augments
  the board it returns with the managed option set, so the UI, the
  server and serialization all operate on the same options (#238).

* The board to build for an incoming request is now resolved by an
  app-level **board loader**, passed to `serve()` as its new `loader`
  argument (default: `local_loader()`, an in-process save/restore handoff).
  A `board_loader()` pairs a `resolve(query, session)` — returning the board
  to build, or `NULL` for the `serve()` default — with a `stage(board,
  session)`, which persists a board and returns the URL query parameters
  referencing it. `serve()` uses that one loader to resolve the board at the
  UI (GET, where `session` is `NULL`) and server (WS connect) builds; when a
  restore fires, `board_server()` exposes the board to restore as a
  `board_refresh` reactive, and `serve()` stages it through the loader,
  writes the parameters into the URL and drives `session$reload()` — so the
  reload stays a guaranteed core mechanism and the `preserve_board` plugin
  (unchanged at `{server, ui}`) neither holds the loader nor reloads. This
  removes the process-global slot core used to stage a board across a
  reload: the exported `get_serve_obj()`, the old board-server reload
  producer, `rv$reload_meta` and the `restore_board()` `meta` argument are
  gone. The default loader keeps its handoff in a per-loader store
  (single-process); a downstream loader (e.g. blockr.session) resolves from
  the URL query against a shared backend, which is multi-process-safe (#214).
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
