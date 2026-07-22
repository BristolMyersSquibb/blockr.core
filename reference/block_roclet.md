# Block registration roclet

A custom roxygen2 roclet that turns block registration metadata,
declared as tags on block constructors, into a YAML registry
(`inst/registry/blocks.yml`) that
[`register_package_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
reads at load time. It runs alongside the standard roclets during
[`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
(or `devtools::document()`) for any package that lists
`blockr.core::block_registration_roclet` in the `Roxygen` field of its
`DESCRIPTION`.

## Usage

``` r
block_registration_roclet()
```

## Value

`block_registration_roclet()` returns a roclet object.

## Tags

Placed in the roxygen block above a block constructor:

- `@block <name>`:

  Human-readable block name. Required: its presence is what marks a
  constructor for registration.

- `@blockDescr <text>`:

  Short block description. Required.

- `@blockCategory <category>`:

  One of the
  [`suggested_categories()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md).
  Required.

- `@blockIcon <icon>`:

  Bootstrap icon name. Optional, defaulting to the category icon (see
  [`default_icon()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)).

- `@blockGuidance <text>`:

  Model-facing construction guidance – do and don't rules, enumerations,
  pitfalls. Optional.

- `@blockKeywords <terms>`:

  Comma-separated search terms for block discovery; a term may contain
  spaces. Optional.

- `@blockDetails <text>`:

  Longer human-facing description (e.g. for a help popover). Optional;
  when omitted it falls back to the constructor's `@details` or, failing
  that, its `@section` prose.

- `@blockLink <url>`:

  URL of the block's help or documentation page. Optional; when omitted
  it is derived from the package's pkgdown `url` (`_pkgdown.yml` or
  `DESCRIPTION`) and the documented topic, as
  `<url>/reference/<topic>.html`.

- `@blockArg <name> <description>`:

  One constructor argument's specification. The text after the name is a
  free-text description; `[example] <expr>` and `[type] <expr>` markers
  (each on its own line) supply an R expression – evaluated when
  documentation is generated – for a worked example value and an
  `arg_*()` type descriptor (see
  [`new_arg_spec()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)).
  An explicit `[description] <text>` marker may replace the inline
  description. Optional and repeatable.

- `@blockExamples <expr>`:

  Block-level worked example configurations, as an R expression –
  evaluated when documentation is generated – that yields a list of
  complete configurations (each a named list keyed by argument).
  Optional; supersedes the per-argument `[example]` assembly.

- `@blockCtor <name>`:

  Constructor name override. Optional: the documented object otherwise
  supplies the name.

Multi-argument worked `examples` (whole-block configurations, as opposed
to a per-argument `[example]`) remain out of the tags' scope; a block
needing those registers via
[`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
directly.
