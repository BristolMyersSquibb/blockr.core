.onLoad <- function(libname, pkgname) { # nocov start

  register_core_output_classes()

  register_front_end("basic", js_module("js/basic.js"), class = "board")

  register_core_renderers()

  invisible(NULL)
} # nocov end

# Core's output classes, with the JSON Schemas that the build generates from
# their TypeScript types in srcts/types/outputs.ts.
register_core_output_classes <- function() {
  register_output_class("table_output", "outputs/table-output.schema.json")
  register_output_class("text_output", "outputs/text-output.schema.json")
  register_output_class("plot_output", "outputs/plot-output.schema.json")
}

# Core's plain renderers, one for each of its output classes.
register_core_renderers <- function() {
  register_renderer(
    "basic_table",
    js_module("js/basic-table.js"),
    class = "table_output"
  )
  register_renderer(
    "basic_text",
    js_module("js/basic-text.js"),
    class = "text_output"
  )
  register_renderer(
    "basic_plot",
    js_module("js/basic-plot.js"),
    class = "plot_output"
  )
}
