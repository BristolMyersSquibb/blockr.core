.onLoad <- function(libname, pkgname) { # nocov start

  register_core_output_classes()

  register_module("basic", js_module("js/basic.js"))

  register_core_blocks()

  trace_block_classes(pkgname)

  invisible(NULL)
} # nocov end

# Core's output classes, with the JSON Schemas that the build generates from
# their TypeScript types in srcts/types/outputs.ts, each with core's plain
# renderer as its default.
register_core_output_classes <- function() {
  register_output_class(
    "table_output",
    "outputs/table-output.schema.json",
    renderer = js_module("js/basic-table.js")
  )
  register_output_class(
    "text_output",
    "outputs/text-output.schema.json",
    renderer = js_module("js/basic-text.js")
  )
  register_output_class(
    "plot_output",
    "outputs/plot-output.schema.json",
    renderer = js_module("js/basic-plot.js")
  )
}
