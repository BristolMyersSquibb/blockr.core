# What R tells the client about the app's packages, in its reply to `hello`,
# for neither side to edit: the entries of the app's block registry,
# `registry`, each with the renderers of its class's outputs, where R serves the
# modules that the available packages offer each other, `modules`, as
# `offered_modules()` gives them, the app's plugins, `plugins`, each under the
# ID of its class with the class's name and version, and where R serves the
# class's module, as `app_plugins()` gives them, what the app's frontend reads
# of the arguments of `serve()`, `frontend`, as `frontend_reply()` writes it,
# the app's board options, `options`, each with its package, where R serves
# its module and the version of its shape, as `app_options()` gives them, the
# renderers that the app names for board options, and the versions of the
# packages that all of these come from.
# An app hands it what `serve()` read as it built the app (see `serve_app()`),
# so that R reads no package's listing for it, in whatever process runs the
# app.
# These are the properties `packages`, `block_registry`, `modules`, `plugins`,
# `frontend`, `options` and `renderers` of inst/protocol/README.md, as R values
# that `encode_message()` writes as the reply's JSON.
# Building them builds no block.
loaded_reply <- function(registry = block_registry, plugins = list(),
                         frontend = as_json_object(list()),
                         options = app_options(),
                         modules = offered_modules()) {

  entries <- registry_entries(registry)
  renderers <- option_renderers()

  drawn <- lapply(entries, entry_renderers)

  # The packages that the reply's parts come from, whose versions it names
  sources <- c(
    unlst(
      lapply(
        list(lst_xtr(entries, "block_class"), modules, options, renderers,
             unlist(drawn, recursive = FALSE, use.names = FALSE)),
        lst_xtr,
        "package"
      ),
      recursive = TRUE
    ),
    id_package(names(plugins))
  )

  list(
    packages = package_versions(sources),
    block_registry = as_json_object(Map(registry_entry_reply, entries, drawn)),
    modules = as_json_object(lapply(modules, served_module)),
    plugins = as_json_object(lapply(plugins, plugin_entry_reply)),
    frontend = frontend,
    options = as_json_object(lapply(options, option_entry_reply)),
    renderers = as_json_object(lapply(renderers, option_renderer_reply))
  )
}

# The version of each package, by name, in the order of the names.
package_versions <- function(packages) {

  packages <- sort(unique(as.character(packages)))

  as_json_object(
    set_names(
      lapply(packages, function(pkg) as.character(pkg_version(pkg))),
      packages
    )
  )
}

# A block class's entry in the block registry as the client gets it, the
# `RegistryEntry` of srcts/types/registry.ts: the metadata that registration
# gives, with `register_block()`'s `link` as `url`, and what the client cannot
# read off a class that stays in R. Those are its data inputs, where R serves
# its UI module, the schema of its state that the module ships, the state a
# new block starts with, its requests, the board options that its output
# family reads and the renderers of its outputs, `renderers` from
# `entry_renderers()`. A class whose
# module ships no schema has the empty schema, which allows any state, and its
# state is written bare, so that its module takes a value of length one in
# either form.
registry_entry_reply <- function(x, renderers = entry_renderers(x)) {
  c(
    list(
      id = x[["id"]],
      name = x[["name"]],
      description = x[["description"]]
    ),
    Filter(
      not_null,
      list(
        url = x[["link"]],
        guidance = x[["guidance"]]
      )
    ),
    list(
      keywords = I(as.character(x[["keywords"]])),
      category = x[["category"]],
      icon = x[["icon"]],
      inputs = I(x[["inputs"]])
    ),
    served_module(x[["block_class"]][["ui"]]),
    list(
      version = x[["block_class"]][["version"]],
      schema = coal(x[["schema"]], as_json_object(list()), fail_all = FALSE),
      state = entry_state(x),
      requests = as_json_object(lapply(x[["requests"]], request_entry_reply)),
      output_options = I(family_option_ids(x[["block_class"]][["output"]])),
      renderers = I(lapply(unname(renderers), output_renderer_reply))
    )
  )
}

# The renderers that draw a block class's outputs, which R names in the
# class's entry of the block registry, so that the client loads them, and adds
# the board options that they name to the board, before it asks for a block's
# first output: those of the output family that the class names, and of the
# families that it hands its results to.
entry_renderers <- function(x) {
  unname(family_renderers(x[["block_class"]][["output"]]))
}

# The state a new block of a class starts with, as R writes a block's state, by
# the schema of its class's state.
entry_state <- function(x) {

  what <- paste(
    "The state a new block of class",
    x[["block_class"]][["id"]],
    "starts with"
  )

  as_json_object(
    shape_by_schema(plain_value(x[["state"]], what), x[["schema"]])
  )
}

# A request as the block registry describes it to the client: the data inputs
# and state args that its function names, the IDs of the board options it
# reads, whether it is volatile or an action, and whether it takes a file, each
# of the last three said only where it is so.
# The client needs no name of the argument that takes the file.
request_entry_reply <- function(x) {
  c(
    list(
      inputs = I(x[["inputs"]]),
      state_args = I(x[["state_args"]]),
      options = I(unname(x[["options"]]))
    ),
    if (isTRUE(x[["volatile"]])) list(volatile = TRUE),
    if (isTRUE(x[["action"]])) list(action = TRUE),
    if (not_null(x[["file"]])) list(file = TRUE)
  )
}

# One of the app's plugins as the client gets it, the `PluginEntry` of
# srcts/protocol/messages.ts: its class's name and version, and where R serves
# the class's module.
plugin_entry_reply <- function(x) {
  c(
    list(name = x[["name"]], version = x[["version"]]),
    served_module(x[["module"]])
  )
}

# A board option as the client gets it, the `OptionEntry` of
# srcts/protocol/messages.ts: the package that lists it, where R serves its
# module, and the version of its shape.
option_entry_reply <- function(x) {
  c(
    list(package = x[["package"]]),
    served_module(x[["module"]]),
    list(version = x[["version"]])
  )
}

# A renderer that the app names for a board option, with where R serves its
# module and the board option that it draws.
option_renderer_reply <- function(x) {
  c(served_module(x[["module"]]), list(option = x[["option"]]))
}

# The renderer of an output as R sends it in the output: its ID, which a
# request of its own names, and where R serves its module.
output_renderer_reply <- function(x) {
  c(list(id = x[["id"]]), served_module(x[["module"]]))
}
