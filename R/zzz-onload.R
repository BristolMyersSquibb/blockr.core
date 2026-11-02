.onLoad <- function(libname, pkgname) {
  register_core_output_classes()
  invisible()
}

# Core's output classes, with the JSON Schemas that the build generates from
# their TypeScript types in srcts/types/outputs.ts.
register_core_output_classes <- function() {
  register_output_class("table_output", "outputs/table-output.schema.json")
  register_output_class("text_output", "outputs/text-output.schema.json")
  register_output_class("plot_output", "outputs/plot-output.schema.json")
}
