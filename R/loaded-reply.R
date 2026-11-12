# What R tells the client about the packages it has loaded, in its reply to
# `hello`, for neither side to edit: the entries of the app's block registry,
# `registry`, each with the renderer of its class's outputs, where R serves the
# modules that the loaded packages offer each other, the app's plugins,
# `plugins`, each under the ID of its class with the class's name and version,
# and where R serves the class's module, as `app_plugins()` gives them, what
# the app's frontend reads of the arguments of `serve()`, `frontend`, as
# `frontend_reply()` writes it, the board options that the loaded packages
# list, each with where R serves its module and the version of its shape, the
# renderers that the app names for board options, and the versions of the
# packages that all of these come from.
# R loads no package for it: `serve()` loaded the packages of the block
# classes that the app offers and of the plugins that it runs, and the app's
# script the others, and loading them registered their methods.
# These are the properties `packages`, `block_registry`, `modules`, `plugins`,
# `frontend`, `options` and `renderers` of inst/protocol/README.md, as R values
# that `encode_message()` writes as the reply's JSON.
# Building them builds no block.
loaded_reply <- function(registry = block_registry, plugins = list(),
                         frontend = as_json_object(list())) {

  entries <- registry_entries(registry)
  modules <- offered_modules()
  options <- loaded_options()
  renderers <- option_renderers()

  coercions <- output_coercions()
  drawn <- lapply(entries, entry_renderer, coercions)

  # The packages that the reply's parts come from, whose versions it names
  sources <- c(
    unlst(
      lapply(
        list(entries, modules, options, renderers, drawn),
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
# new block starts with, its arg specs, its requests and the renderer of its
# outputs, `renderer` from `entry_renderer()`, where R knows one. A class whose
# module ships no schema has the empty schema, which allows any state, and its
# state is written bare, so that its module takes a value of length one in
# either form.
registry_entry_reply <- function(x, renderer = NULL) {

  what <- paste("Block class", x[["block_class"]][["id"]])

  c(
    list(
      id = x[["id"]],
      name = x[["name"]],
      description = x[["description"]]
    ),
    Filter(
      not_null,
      list(
        details = x[["details"]],
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
    served_module(x[["ui"]]),
    list(
      version = x[["block_class"]][["version"]],
      schema = coal(x[["schema"]], as_json_object(list()), fail_all = FALSE),
      state = entry_state(x),
      arguments = as_json_object(
        lapply(x[["arguments"]], arg_spec_reply, what)
      ),
      requests = as_json_object(lapply(x[["requests"]], request_entry_reply))
    ),
    if (not_null(renderer)) list(renderer = output_renderer_reply(renderer))
  )
}

# The renderer of a block class's outputs, which R names in the class's entry
# of the block registry, so that the client loads it, and adds the board
# options that it names to the board, before it asks for a block's first
# output: the renderer of the output class whose method of `output_coercion()`
# gives the coercion that the block class names, or `as_text_output()` where it
# names none, from the `coercions` that `output_coercions()` gives. It is
# `NULL` where no output class gives that coercion, or where R knows no
# renderer for the class, so that the client learns the renderer from the first
# output, and R reports its error there.
entry_renderer <- function(x, coercions = output_coercions()) {

  cls <- x[["block_class"]]
  coercion <- coal(cls[["output"]], as_text_output, fail_all = FALSE)
  class <- coercion_class(coercion, coercions)

  if (is.null(class)) {
    return(NULL)
  }

  tryCatch(
    renderer_of(structure(list(), class = c(class, "block_output"))),
    output_renderer_missing = function(e) NULL
  )
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

# An arg spec as the client gets it, the `ArgSpec` of srcts/types/registry.ts,
# without the parts it leaves unset, with its example written by its type.
arg_spec_reply <- function(x, what) {

  type <- arg_spec_type(x)
  example <- arg_spec_example(x)

  if (not_null(example)) {
    example <- shape_by_schema(
      plain_value(example, paste(what, "has an arg spec whose example")),
      type
    )
  }

  Filter(
    not_null,
    list(
      description = arg_spec_description(x),
      example = example,
      type = arg_type_reply(type)
    )
  )
}

# An arg spec's type, the JSON Schema it is, without the R attribute through
# which `arg_object()` learns which of its properties are required, and with
# the `properties` of an object without any as an empty object.
arg_type_reply <- function(x) {

  if (!is.list(x)) {
    return(x)
  }

  res <- lapply(x, arg_type_reply)

  if ("properties" %in% names(res)) {
    res[["properties"]] <- as_json_object(res[["properties"]])
  }

  res
}

# A request as the block registry describes it to the client: the data inputs
# and state args that its function names, the IDs of the board options it
# reads, and whether it is volatile or an action, each said only where it is.
request_entry_reply <- function(x) {
  c(
    list(
      inputs = I(x[["inputs"]]),
      state_args = I(x[["state_args"]]),
      options = I(unname(x[["options"]]))
    ),
    if (isTRUE(x[["volatile"]])) list(volatile = TRUE),
    if (isTRUE(x[["action"]])) list(action = TRUE)
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
