# What R tells the client about the packages it has loaded, in its reply to
# `hello` and to every `open`, for neither side to edit: the block registry's
# entries, where R serves the front-ends, the startup modules and the modules
# that packages offer each other, the renderers it picked, and the versions of
# the packages that all of these come from. These are the fields `packages`,
# `block_registry`, `front_ends`, `startup_modules`, `modules` and `renderers`
# of inst/protocol/README.md, as R values that `encode_message()` writes as
# the reply's JSON. Building them builds no block.
loaded_reply <- function() {

  entries <- available_blocks()
  front_ends <- lapply(set_names(nm = list_front_ends()), get_front_end)
  startup <- startup_modules()
  modules <- lapply(set_names(nm = list_modules()), get_module)
  renderers <- pick_renderers()

  packages <- unlst(
    lapply(
      list(entries, front_ends, startup, modules, renderers),
      lst_xtr,
      "package"
    ),
    recursive = TRUE
  )

  list(
    packages = package_versions(packages),
    block_registry = as_json_object(lapply(entries, registry_entry_reply)),
    front_ends = as_json_object(lapply(front_ends, declared_module_reply)),
    startup_modules = unname(lapply(startup, startup_module_reply)),
    modules = as_json_object(lapply(modules, declared_module_reply)),
    renderers = as_json_object(lapply(renderers, renderer_reply))
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
# new block starts with, its arg specs and its requests. A class whose module
# ships no schema has the empty schema, which allows any state, and its state
# is written bare, so that its module takes a value of length one in either
# form.
registry_entry_reply <- function(x) {

  what <- paste("Block class", x[["block_class"]][["class"]][1L])

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
      schema = coal(x[["schema"]], as_json_object(list()), fail_all = FALSE),
      state = entry_state(x),
      arguments = as_json_object(
        lapply(x[["arguments"]], arg_spec_reply, what)
      ),
      requests = as_json_object(lapply(x[["requests"]], request_entry_reply))
    )
  )
}

# The state a new block of a class starts with, as R writes a block's state, by
# the schema of its class's state.
entry_state <- function(x) {

  what <- paste(
    "The state a new block of class",
    x[["block_class"]][["class"]][1L],
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
# which `arg_object()` learns which of its fields are required, and with the
# properties of an object without fields as an empty object.
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

# Where R serves a module that a package declares, such as a front-end or a
# module that it offers others.
declared_module_reply <- function(x) {
  served_module(x[["module"]])
}

startup_module_reply <- function(x) {
  c(list(package = x[["package"]]), served_module(x[["module"]]))
}

# A renderer that R picked, with where R serves its module and the output class
# or the board option that it draws.
renderer_reply <- function(x) {
  c(
    served_module(x[["module"]]),
    Filter(not_null, list(class = x[["class"]], option = x[["option"]]))
  )
}
