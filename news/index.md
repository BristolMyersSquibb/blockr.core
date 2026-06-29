# Changelog

## blockr.core 0.1.3

- Block registry entries gained a structured argument specification,
  built with the new exported
  [`new_block_args()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md)
  /
  [`new_block_arg()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  carrying a per-argument `description`, a single worked `example`, and
  an optional machine-readable `type`.
  [`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
  additionally accepts block-level `details`, `link`, `guidance`,
  `examples`, and `keywords`. The construction metadata formerly
  smuggled as `examples` / `prompt` attributes on `arguments` is now
  first-class and validated at registration (every argument is a real
  constructor formal and every worked example actually constructs); the
  legacy attributes are still absorbed, with a deprecation warning
  ([\#121](https://github.com/BristolMyersSquibb/blockr.core/issues/121)).

- An argument’s `type` is a dependency-free JSON-Schema-subset
  descriptor built with
  [`arg_string()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  [`arg_number()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  [`arg_integer()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  [`arg_boolean()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  [`arg_enum()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  [`arg_array()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md)
  and
  [`arg_object()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md)
  (consumable directly, e.g. via
  [`ellmer::type_from_schema()`](https://ellmer.tidyverse.org/reference/type_boolean.html)),
  replacing the opaque `ellmer::type_*` slot. Registration now validates
  the descriptor and checks each worked example against it, so a
  malformed-but-constructible value is rejected
  ([\#121](https://github.com/BristolMyersSquibb/blockr.core/issues/121)).

- Block metadata is exposed two ways.
  [`block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md)
  returns a `data.frame` over one or many blocks – dispatching on a
  `block`, a `blocks` collection, a `block_registry_entry` or a registry
  ID – with every attribute available as a column (the multi-valued
  `arguments`, `examples` and `keywords` as list-columns) and a `fields`
  argument to select a subset. Each attribute additionally has a
  single-block getter:
  [`block_meta_name()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md),
  [`block_meta_description()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md),
  [`block_meta_guidance()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md),
  [`block_meta_arguments()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md),
  [`block_meta_keywords()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md),
  and so on. A single argument’s fields are read with
  [`block_arg_description()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  [`block_arg_example()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md)
  and
  [`block_arg_type()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md).
  [`registry_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
  is deprecated in favour of these
  ([\#121](https://github.com/BristolMyersSquibb/blockr.core/issues/121)).

- Board options contributed by blocks or the registry (e.g. the
  preview-row count) are no longer reset to their defaults on save and
  reload. The settings sidebar manages the wider
  [`blockr_app_options()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  set, but a board carried only its own
  [`board_options()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md),
  and that narrow set is what
  [`serialize_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.md)
  persisted — so a user’s change to a block-backed option was dropped.
  `resolve_board()` now augments the board it returns with the managed
  option set, so the UI, the server and serialization all operate on the
  same options
  ([\#238](https://github.com/BristolMyersSquibb/blockr.core/issues/238)).

- The board to build for an incoming request is now resolved by an
  app-level **board loader**, passed to
  [`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  as its new `loader` argument (default:
  [`local_loader()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_loader.md),
  an in-process save/restore handoff). A
  [`board_loader()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_loader.md)
  pairs a `resolve(query, session)` — returning the board to build, or
  `NULL` for the
  [`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  default — with a `stage(board, session)`, which persists a board and
  returns the URL query parameters referencing it.
  [`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  uses that one loader to resolve the board at the UI (GET, where
  `session` is `NULL`) and server (WS connect) builds; when a restore
  fires,
  [`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
  exposes the board to restore as a `board_refresh` reactive, and
  [`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  stages it through the loader, writes the parameters into the URL and
  drives `session$reload()` — so the reload stays a guaranteed core
  mechanism and the `preserve_board` plugin (unchanged at
  `{server, ui}`) neither holds the loader nor reloads. This removes the
  process-global slot core used to stage a board across a reload: the
  exported `get_serve_obj()`, the old board-server reload producer,
  `rv$reload_meta` and the
  [`restore_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.md)
  `meta` argument are gone. The default loader keeps its handoff in a
  per-loader store (single-process); a downstream loader
  (e.g. blockr.session) resolves from the URL query against a shared
  backend, which is multi-process-safe
  ([\#214](https://github.com/BristolMyersSquibb/blockr.core/issues/214)).

- New exported
  [`trim_rv()`](https://bristolmyerssquibb.github.io/blockr.core/reference/trim_rv.md)
  removes entries from a `reactiveValues` object, which assigning `NULL`
  does not do – the key lingers in
  [`names()`](https://rdrr.io/r/base/names.html) with a `NULL` value.
  Unlinking a variadic block argument now drops it from the block’s
  `...args` outright rather than leaving a phantom `NULL` entry behind
  ([\#227](https://github.com/BristolMyersSquibb/blockr.core/issues/227)).

- Boards can be deployed read-only via the `blockr.locked` option,
  enforced server-side rather than by UI hiding (which a forged
  `Shiny.setInputValue` bypasses). While locked, the two channels every
  mutation funnels through – the `board_update` lifecycle and
  [`set_board_option_value()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.md)
  – refuse to apply changes.
  [`is_board_locked()`](https://bristolmyerssquibb.github.io/blockr.core/reference/locked-board.md)
  is an S3 generic (the default method reads the option; a subclass can
  source the state differently, e.g. an authenticated unlock).
  [`set_board_option_value()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.md)
  gains a required `board` argument so the option channel can resolve
  the lock. Locking is not a defense against arbitrary R executing in
  the session – which can flip the flag or edit the board directly – so
  untrusted deployments must be isolated at the deployment layer
  ([\#229](https://github.com/BristolMyersSquibb/blockr.core/issues/229)).

- The default
  [`notify_user()`](https://bristolmyerssquibb.github.io/blockr.core/reference/notify_user.md)
  plugin now tracks each block’s conditions individually rather than
  re-deriving the whole board’s condition frame on every change. A
  single block’s condition change updates only the notifications it
  affects, restoring O(block)-per-change work and an O(N) rather than
  O(N^2) cost when a condition cascade touches many blocks on a large
  board. `board$conditions()` is unchanged for programmatic consumers
  ([\#222](https://github.com/BristolMyersSquibb/blockr.core/issues/222)).

- [`notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.md)
  gains `glue` and `log` arguments (both `TRUE` by default, so existing
  behaviour is unchanged). `glue = FALSE` surfaces literal text without
  \[cli::pluralize()\] interpolation — so caught condition messages
  containing brace characters no longer error — and `log = FALSE` skips
  the log entry for already-logged text. The board update, link and
  stack error toasts now pass condition messages through with
  `glue = FALSE`, and the default
  [`notify_user()`](https://bristolmyerssquibb.github.io/blockr.core/reference/notify_user.md)
  plugin renders through
  [`notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.md)
  ([\#222](https://github.com/BristolMyersSquibb/blockr.core/issues/222)).

- Active block conditions (errors, warnings and messages captured during
  evaluation) are now emitted as tidy data frames. Each block server
  returns its conditions as a reactive `server$conditions` (one row per
  active condition, columns `block`, `phase`, `severity`, `message` and
  `id`), and
  [`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
  combines these into a board-level reactive `board$conditions()` on the
  read-only board handed to plugins and callbacks. A consumer reads one
  reactive — a single block’s frame for fine-grained updates, or the
  whole board — instead of walking the nested per-block condition state,
  and the default
  [`notify_user()`](https://bristolmyerssquibb.github.io/blockr.core/reference/notify_user.md)
  plugin surfaces them to the user as toasts. The block server no longer
  returns its raw `cond` reactive values object — read
  `server$conditions()` instead
  ([\#217](https://github.com/BristolMyersSquibb/blockr.core/issues/217)).

- [`str_value()`](https://bristolmyerssquibb.github.io/blockr.core/reference/str_value.md)
  now covers every domain class that has a full-tier
  [`format()`](https://rdrr.io/r/base/format.html) /
  [`print()`](https://rdrr.io/r/base/print.html) counterpart, completing
  the compact rendering tier: the scalars `link`, `board_option`,
  `llm_model_option` and `plugin`; the containers `blocks`, `stacks`,
  `links`, `board_options` and `plugins`; and the whole `board`. Each
  class also gains a [`utils::str()`](https://rdrr.io/r/utils/str.html)
  method that thinly displays its
  [`str_value()`](https://bristolmyerssquibb.github.io/blockr.core/reference/str_value.md);
  a container or board renders one element per line below a `<class[n]>`
  header
  ([\#212](https://github.com/BristolMyersSquibb/blockr.core/issues/212)).

- New exported generic
  [`external_ctrl_vars()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.md)
  (with a `block` method) and predicate
  [`has_external_ctrl()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.md)
  provide a public, polymorphic API for resolving a board component’s
  `external_ctrl` declaration into the set of externally controllable
  variable names. This promotes the previously internal
  `block_external_ctrl_vars()`, letting dependent packages dispatch on
  it (e.g. for dock extensions) instead of re-reading the raw
  `external_ctrl` attribute
  ([\#192](https://github.com/BristolMyersSquibb/blockr.core/issues/192)).

- [`blockr_deser.list()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_ser.md)
  now forwards `...` to the dispatched per-class method, so callers can
  thread additional context (e.g. a producer version) down to nested
  deserializers. Previously such arguments were silently dropped at the
  list-dispatch boundary
  ([\#186](https://github.com/BristolMyersSquibb/blockr.core/issues/186)).

## blockr.core 0.1.2

CRAN release: 2026-04-28

- The `mod` slots in `update(...)` payloads for **blocks, links and
  stacks** now uniformly expect a **delta** shape: a named list keyed by
  entry ID, where each entry is a named list of argument values to apply
  on top of the live entry.
  - For `blocks`, keys must be in `block_external_ctrl_vars(blk)` —
    non-ctrl-able changes go through `rm` + `add`. Ctrl-arg writes hit
    the corresponding `reactiveVal` in place; `block_name` (always
    treated as ctrl-able) updates the block’s registry attribute.
  - For `links` and `stacks`, deltas are merged onto the current entry
    via the new
    [`update_link()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_link.md)
    /
    [`update_stack()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_stack.md)
    S3 generics. The default methods reconstruct the entry through its
    stored constructor, preserving sub-class attributes. Sub-class
    owners (e.g. `dock_stack` adding `color`) only need to register a
    method when their constructor deviates from the convention
    ([\#175](https://github.com/BristolMyersSquibb/blockr.core/issues/175)).
- `block_external_ctrl_vars()` always includes `"block_name"` — every
  block can be renamed through `update(...)` regardless of its
  `external_ctrl` opt-in. The default ctrl plugin panel renders a
  `block_name` field on every block card alongside any opted-in ctrl
  vars. `block_supports_external_ctrl()` (which would now be `TRUE` for
  every block) has been removed; the gates that used to call it have
  been simplified accordingly.
- The default `block_server.block` now constructs a `block_name`
  `reactiveVal` per block and appends it to the `vars` list passed to
  the ctrl plugin. Two guarded observers keep that `reactiveVal` in sync
  with the block’s `block_name` attribute on the board (one emits an
  `update(mod = ...)` when the ctrl plugin writes the rv; the other
  pulls registry-attr changes back into the rv). The `ctrl_block_server`
  plugin signature is unchanged — block authors and custom ctrl plugins
  see uniform `vars` reactiveVals.
- A blockr option `attach_default_packages` can be set to opt into
  evaluating block expressions with objects from default packages
  directly available.
- Add
  [`ctrl_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/ctrl_block.md)
  plugin for external block control, allowing blocks to be driven
  programmatically from outside the standard block UI.
- Add
  [`clear_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md)
  for removing all blocks/stacks from a board.
- Add
  [`bbquote()`](https://bristolmyerssquibb.github.io/blockr.core/reference/bbquote.md)
  and helpers (`.`, `..`) for cleaner code generation via
  [`bquote()`](https://rdrr.io/r/base/bquote.html)-based quasiquotation.
- Export
  [`custom_plugins()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  and
  [`custom_options()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  for easier board customization.
- Add
  [`block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md)
  for retrieving per-block metadata and attach block metadata with
  defaults to block objects.
- Export test utilities
  ([`blockr_test_exports()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md),
  [`new_mock_session()`](https://bristolmyerssquibb.github.io/blockr.core/reference/testing.md),
  [`export_safely()`](https://bristolmyerssquibb.github.io/blockr.core/reference/testing.md))
  for use in downstream package tests.

## blockr.core 0.1.1

CRAN release: 2025-12-06

- Add Block-level notifications via (optional) `expr` server return
  value component `cond`.
- Export
  [`get_board_option_value()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.md)
  to make available current option settings via `session$UserData`.
- Introduce (optional) dependency on
  [thematic](https://rstudio.github.io/thematic/) to auto-style plots.
- Export
  [`toolbar_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_ui.md)
  which takes case of the “core” toolbar UI component.
- Utility functions
  [`chr_ply()`](https://bristolmyerssquibb.github.io/blockr.core/reference/chr_ply.md)
  and related, as well as miscellaneous utilities such as
  [`set_names()`](https://bristolmyerssquibb.github.io/blockr.core/reference/set_names.md),
  [`coal()`](https://bristolmyerssquibb.github.io/blockr.core/reference/set_names.md),
  etc. are now exported for use in dependent packages.
- Export
  [`export_code()`](https://bristolmyerssquibb.github.io/blockr.core/reference/export_code.md)
  to make it easier for third-party
  [`generate_code()`](https://bristolmyerssquibb.github.io/blockr.core/reference/generate_code.md)
  plugin implementations.
- Use
  [`evaluate::evaluate()`](https://bristolmyerssquibb.github.io/blockr.core/news/evaluate.r-lib.org/reference/evaluate.md)
  to capture plots.
- Add
  [`new_fixed_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.md)
  for applying a fixed (i.e. non-paramtetrized) transformation to data
  input.
- Board server callbacks are invoked with an additional argument
  `session`.
- Export assertion utilities such as
  [`is_string()`](https://bristolmyerssquibb.github.io/blockr.core/reference/assertions.md),
  [`is_count()`](https://bristolmyerssquibb.github.io/blockr.core/reference/assertions.md),
  etc.
- Improved ser/des, which now includes package/constructor information
  for all board, blocks, stacks and options. The corresponding infra
- Board options now contain UI/server components to provide more options
  for customization. Also blocks can require certain options to be
  available.
- Introduces
  [`block_render_trigger()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_server.md)
  to control per block class when to re-render the block output.
- Rework of block notifications to provide a
  [`reactiveValues()`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  object containing notification types a separate components.
- Auto-ID generation can now be customized with a default provided by
  the ids package (if available).
- Use the glue package for logging/block notifications; add a glue-based
  text block.
- New board restore mechanism based on `session$reload()`.
- Improvements to the registry: block icons and a fixed set of
  categories.

## blockr.core 0.1.0

CRAN release: 2025-05-20

- Initial CRAN submission
