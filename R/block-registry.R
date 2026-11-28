# A block registry holds block classes by package, each package's by the id
# of its class, in `packages`, and the classes that an app script defines,
# which record no package, by id, in `script`.
new_block_registry <- function() {
  list2env(
    list(
      packages = new.env(parent = emptyenv()),
      script = new.env(parent = emptyenv())
    ),
    parent = emptyenv()
  )
}

block_registry <- new_block_registry()

#' Block registry
#'
#' Listing of blocks is available via a block registry, which holds each block
#' class under its registry id, with the metadata registration gives it, in
#' order to provide a browsable block directory and so that R evaluates a
#' block, and resolves its class's requests, without building a block. A block
#' class is registered by its constructor, made by [block_constructor()], from
#' which registration reads the class, and the registry keeps one entry per
#' class.
#'
#' A block class's registry id comes from the class alone.
#' It holds the package whose code defines the class and the class's id (see
#' [new_block_class()]), as two parts of their own, as
#' `list(package = "blockr.core", id = "head_block")`.
#' A class that an app script defines records no package, so the package of
#' its registry id is `NULL`.
#' So renaming a constructor leaves the registry id as it is,
#' while changing a class's id changes it.
#' The `registry_id_from_block()` function reads the registry id from a
#' block's class, whether or not the block registry holds the class.
#'
#' Which block classes an app offers is the app's choice.
#' The methods of [serve()] for a board and for a path take them in `blocks`,
#' which defaults to `available_blocks()`: a data frame with a row for each
#' block class of each available package, with the class's package in
#' `package` and its id in `id`.
#' The app of a block that `serve()` serves alone offers the block's class
#' alone.
#' A package is available where it exports `._BLOCKR_MODULES_`, installed or
#' loaded by `pkgload::load_all()`, and lists its block classes there, as it
#' reads them from its `inst/registry/blocks.yml`, which the
#' [block_registration_roclet()] writes (see [read_blocks_yml()]).
#' A package is also available where the block registry of the process holds a
#' class of it (below).
#' R loads each available package to read what it lists.
#' A script narrows the rows with plain R, as
#' `subset(available_blocks(), package != "blockr.ggplot")` offers every class
#' but those of blockr.ggplot, or gives the path of a YAML file that lists
#' packages and, for each, the ids of the classes to offer, all of them where
#' it lists none:
#'
#' ```yaml
#' - blockr.core:
#'   - dataset_block
#'   - head_block
#' - blockr.dplyr
#' ```
#'
#' The file offers what the same rows would, and a package that it leaves out
#' offers nothing.
#' A row whose package is not available, or whose class its package does not
#' list, stops the app, naming the row, and so does a package that lists two
#' constructors whose classes give one registry id.
#' A package that lists block classes that R cannot read stops
#' `available_blocks()`, and every app that offers the package's classes, with
#' each fault of each entry (see [block_registration_roclet()]).
#'
#' Each app holds a block registry of its own, which `serve()` builds as it is
#' called, and which the app's sessions read alone.
#' R loads the packages of the rows and registers their classes in it.
#' The block registry of the R process holds the classes that
#' `register_block()` and `register_blocks()` register.
#' A package that lists no block classes may register its own classes there
#' as it loads, and an app script may register a package's class there in
#' place of the one that the package lists.
#' Each class of a package that the process registry holds is a row of
#' `available_blocks()`, as a class that a package lists is, and an app offers
#' it where its rows hold it, from the process registry.
#' A class that an app script defines itself records no package, so no row
#' names it, and every app for a board or a path that the script serves after
#' registering it holds it.
#' So a later `serve()` or `register_block()` changes no app that `serve()`
#' built before.
#'
#' Every block class in an app's block registry comes from a registration: a
#' package's listing, or the block registry of the process.
#' A block on the board that the script serves may be of a class that the
#' app's rows leave out, such as core's static block.
#' Then `serve()` registers that class in the app's block registry from its
#' registration, so that the block runs, and the app offers it like the classes
#' of its rows.
#' A block of a class that nothing registers stops the app, so a script
#' registers a class that it defines itself before it serves a board with a
#' block of it (see [serve()]).
#'
#' An entry holds, beside the metadata, the block class, its data inputs and
#' the JSON Schema of the block's state that the class's UI module ships beside
#' it, as `<name>.schema.json` for a module `<name>.js`,
#' decoded with an array of length one marked with [I()], so that R writes it
#' back as the module ships it, and whose properties must be the state args
#' where it ships one, a description of each request, with the inputs and
#' state args it names, the ids of the board options it reads, named by the
#' arguments that take their values, whether it is volatile or an action, and
#' the argument that takes a file, where it takes one,
#' and the state a new block starts with, from `expr` called without args.
#' The schema is what describes the block's state, to the client and to
#' models alike.
#' Each of its properties gives a state arg's type, and may give the arg's
#' description (see `vignette("create-block")`).
#'
#' R sends the client what it needs of each entry, in its reply to `hello`,
#' without building a block: the metadata, with `link` as `url`, the data
#' inputs, where R serves the UI module, the schema, the state a new block
#' starts with and the requests. R serves the
#' module, and the stylesheet beside it, at a URL that holds a hash of the
#' file's content, so that no browser runs a changed file from its cache. It
#' writes the starting state by the schema of the block's state.
#' So registration refuses a class whose starting state is not a plain value,
#' as a block's state has to be (see [blockr_ser()]).
#'
#' Blocks can be registered (i.e. added to the registry) via `register_block()`
#' with scalar-valued arguments and `register_blocks()`, where arguments may be
#' vector-valued, while de-registration (or removal) is handled via
#' `unregister_blocks()`, all in the block registry of the process.
#' The registered classes come from `list_blocks()`, as rows with each class's
#' package in `package` and its id in `id`, the package `NA` for a class that
#' an app script defines.
#' The function `unregister_blocks()` takes such rows, or the registry id of
#' one class, as `registry_id_from_block()` gives it.
#' The function `available_blocks()` lists the block classes of the available
#' packages, which an app may offer, as rows in the same shape.
#' Finally, block construction via a registry id is available as
#' `create_block()`, which takes the class's id, and its package in `package`
#' for a class that a package defines.
#'
#' @param ctor Block constructor, made by [block_constructor()]
#' @param name,description Metadata describing the block
#' @param category Useful to sort blocks by topics. If not specified,
#'   blocks are uncategorized.
#' @param icon Icon
#' @param link Optional URL to a block's help or documentation page
#' @param guidance Optional model-facing construction guidance (do/don't rules,
#'   enums, pitfalls)
#' @param keywords Optional character vector of free-text search terms for block
#'   discovery
#' @param overwrite Overwrite existing entry
#'
#' @examples
#' new_identity_block <- block_constructor(
#'   new_block_class(
#'     "identity_block",
#'     inputs = "data",
#'     expr = function() bbquote(.(data)),
#'     ui = js_module("identity-block.js")
#'   )
#' )
#'
#' register_block(new_identity_block, "Identity", "Returns its input")
#'
#' subset(list_blocks(), id == "identity_block")
#'
#' id <- registry_id_from_block(new_identity_block())
#' id
#'
#' create_block("identity_block")
#'
#' unregister_blocks(id)
#' subset(list_blocks(), id == "identity_block")
#'
#' @return `register_block()` and `register_blocks()` are invoked for their side
#' effects and return `block_registry_entry` object(s) invisibly, while
#' `unregister_blocks()` returns `NULL` (invisibly).
#' Listing via `list_blocks()` and `available_blocks()` returns a data frame of
#' `package` and `id` with a row for each class.
#' Block construction via `create_block()` returns a newly instantiated `block`
#' object, and the lookup `registry_id_from_block()` returns the registry id
#' that a block's class gives, a list of its `package`, a string or `NULL`, and
#' its `id`, a string.
#' The categories that registration expects a class to name come from
#' `suggested_categories()`, as a character vector.
#'
#' @export
register_block <- function(ctor, name, description, category = NULL,
                           icon = NULL, link = NULL, guidance = NULL,
                           keywords = NULL, overwrite = FALSE) {

  add_block_entry(
    block_registry, ctor, name, description, category, icon, link, guidance,
    keywords, overwrite
  )
}

# Registers a block class in `registry`, as `register_block()` does in the
# block registry of the process, and `serve()` in an app's.
add_block_entry <- function(registry, ctor, name, description, category = NULL,
                            icon = NULL, link = NULL, guidance = NULL,
                            keywords = NULL, overwrite = FALSE) {

  if (is.null(category)) {
    category <- default_category()
  }

  if (!is_string(category)) {
    blockr_abort(
      "A block category is expected to be a string.",
      class = "block_category_invalid"
    )
  }

  if (!category %in% suggested_categories()) {
    blockr_warn(
      "Block category {category} is not among suggested categories ",
      "{suggested_categories()}. Consider choosing a different one.",
      class = "block_category_discouraged",
      frequency = "once",
      frequency_id = paste0("block_category_", category, "_discouraged")
    )
  }

  if (is.null(icon)) {

    icon <- default_icon(category)

  } else {

    if (!is_string(icon)) {
      blockr_abort(
        "A block icon is expected to be the name of a Bootstrap icon, as a ",
        "string.",
        class = "block_icon_invalid"
      )
    }

    if (grepl("[-[:space:]]fill$", icon, ignore.case = TRUE)) {
      blockr_warn(
        "Using block icons with 'fill' style, such as {icon} is discouraged.",
        class = "block_icon_fill_discouraged",
        frequency = "once",
        frequency_id = paste0("block_icon_", icon, "_discouraged")
      )
    }

    icon <- tryCatch(
      as.character(bsicons::bs_icon(icon)),
      error = function(e) {
        blockr_abort(
          "{conditionMessage(e)}",
          class = "block_icon_invalid"
        )
      }
    )
  }

  cls <- constructor_class(ctor)
  id <- registry_id_from_block(cls)

  if (not_null(registry_entry(id, registry)) && !isTRUE(overwrite)) {
    blockr_abort(
      "Block class {pkg_id_label(id)} is registered already. Set ",
      "`overwrite = TRUE` to replace it, or remove it first with ",
      "`unregister_blocks()`.",
      class = "block_already_in_registry"
    )
  }

  entry <- class_entry(
    cls,
    list(
      name = name,
      description = description,
      link = link,
      guidance = guidance,
      keywords = keywords,
      category = category,
      icon = icon
    )
  )

  put_registry_entry(entry, registry)

  invisible(entry)
}

# The entry of block class `cls` in a block registry, with the metadata that
# its registration gives, `meta`, as `add_block_entry()` checks it: the
# entry's `name`, `description`, `link`, `guidance`, `keywords`, `category`
# and `icon`, the icon as SVG markup.
class_entry <- function(cls, meta) {

  state <- expr_state(cls, list())

  # R sends the client the state a new block of the class starts with, so it
  # has to be a plain value, as the JSON of a board holds it.
  plain_value(
    state,
    paste("The state a new block of class", cls[["id"]], "starts with")
  )

  do.call(
    new_registry_entry,
    c(
      list(package = cls[["package"]], id = cls[["id"]]),
      meta,
      list(
        block_class = cls,
        inputs = class_inputs(cls),
        schema = class_schema(cls),
        requests = lapply(cls[["requests"]], request_entry, cls),
        state = state
      )
    ),
    quote = TRUE
  )
}

# The metadata that the registration of registry entry `x` gave, as
# `class_entry()` takes it.
entry_metadata <- function(x) {
  x[c("name", "description", "link", "guidance", "keywords", "category",
      "icon")]
}

class_schema <- function(cls) {

  schema <- module_schema(cls[["ui"]])

  if (is.null(schema)) {
    return(NULL)
  }

  props <- names(schema[["properties"]])
  args <- class_state_args(cls)

  if (!setequal(coal(props, character(), fail_all = FALSE), args)) {
    blockr_abort(
      "The schema of block class {cls[['class']][1L]} has properties ",
      "{props}, where the class's state args are {args}.",
      class = "block_schema_invalid"
    )
  }

  schema
}

#' @rdname register_block
#' @export
default_icon <- function(category = default_category()) {

  if (!is_string(category)) {
    blockr_abort(
      "A block category is expected to be a string.",
      class = "block_category_invalid"
    )
  }

  res <- switch(
    category,
    input = "upload",
    transform = "magic",
    structured = "asterisk",
    plot = "file-bar-graph",
    table = "table",
    model = "gear",
    output = "save",
    utility = "tools",
    "question-square"
  )

  as.character(bsicons::bs_icon(res))
}

#' @rdname register_block
#' @export
default_category <- function() "uncategorized"

#' @rdname register_block
#' @export
suggested_categories <- function() {
  c(
    "input", "transform", "structured", "plot", "table", "model", "output",
    "utility", "uncategorized"
  )
}

new_registry_entry <- function(...) {
  structure(list(...), class = "block_registry_entry")
}

is_registry_entry <- function(x) inherits(x, "block_registry_entry")

#' @export
format.block_registry_entry <- function(x, ...) {
  c(
    paste0("<block_registry_entry> ", pkg_id_label(x)),
    paste0("Name: \"", x[["name"]], "\""),
    paste0("Description: \"", x[["description"]], "\""),
    paste0("Category: ", x[["category"]]),
    paste0("Block class: ", x[["block_class"]][["id"]])
  )
}

#' @export
print.block_registry_entry <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
board_options.block_registry_entry <- function(x, ...) {
  class_board_options(x[["block_class"]])
}

#' @rdname register_block
#' @export
list_blocks <- function() {
  registry_rows(block_registry)
}

# The block classes of a block registry, as rows of `list_blocks()`, in the
# order of `registry_entries()`.
registry_rows <- function(registry) {

  entries <- registry_entries(registry)

  block_rows(
    package = chr_ply(
      entries,
      function(x) coal(x[["package"]], NA_character_, fail_all = FALSE)
    ),
    id = chr_xtr(entries, "id")
  )
}

#' @param block A block or a block class
#' @rdname register_block
#' @export
registry_id_from_block <- function(block) {

  cls <- if (is_block_class(block)) block else block_class(block)

  if (!is_block_class(cls)) {
    blockr_abort(
      "Expecting a block or a block class.",
      class = "block_class_invalid"
    )
  }

  pkg_id(cls[["package"]], cls[["id"]])
}

#' @param blocks The block classes to remove, as rows with each class's package
#'   in `package` and its id in `id`, as `list_blocks()` gives them, or the
#'   registry id of one class, as `registry_id_from_block()` gives it
#' @rdname register_block
#' @export
unregister_blocks <- function(blocks = list_blocks()) {

  for (id in registry_ids(blocks)) {
    drop_registry_entry(id, block_registry)
  }

  invisible()
}

# The registry ids that `x` gives: rows with a `package` and an `id`, as
# `list_blocks()` gives them, or one registry id, as
# `registry_id_from_block()` gives it.
registry_ids <- function(x) {

  if (is_pkg_id(x)) {
    return(list(pkg_id(x[["package"]], x[["id"]])))
  }

  if (!is.data.frame(x) || !all(c("package", "id") %in% names(x))) {
    blockr_abort(
      "Expecting block classes as rows with a `package` and an `id`, as ",
      "`list_blocks()` gives them, or the registry id of one class, as ",
      "`registry_id_from_block()` gives it.",
      class = "block_classes_invalid"
    )
  }

  unname(
    Map(pkg_id, as.character(x[["package"]]), as.character(x[["id"]]))
  )
}

#' @param ... For `register_blocks()`, arguments forwarded to
#'   `register_block()`
#' @rdname register_block
#' @export
register_blocks <- function(...) {
  arg_processor <- function(ctor, ...) {
    wrap_list <- function(x) {
      if (length(x) > 1L) list(x) else x
    }

    if (length(ctor) > 1L) {
      return(c(list(ctor), list(...)))
    }

    c(list(ctor), lapply(list(...), wrap_list))
  }

  invisible(
    do.call(Map, c(register_block, arg_processor(...)))
  )
}

# The entry of the block class whose registry id is `id` in `registry`, which
# stops where the registry holds none.
get_registry_entry <- function(id, registry = block_registry) {

  res <- registry_entry(id, registry)

  if (is.null(res)) {
    blockr_abort(
      "No block class is registered under the registry id ",
      "{pkg_id_label(id)}.",
      class = "registry_id_unknown"
    )
  }

  res
}

# The entry of the block class whose registry id is `id` in `registry`, or
# `NULL` where the registry holds none, as for an `id` that is no registry id.
registry_entry <- function(id, registry = block_registry) {

  if (!is_pkg_id(id)) {
    return(NULL)
  }

  part <- registry_part(registry, id[["package"]])

  if (is.null(part)) {
    return(NULL)
  }

  res <- get0(id[["id"]], envir = part, inherits = FALSE)

  if (is_registry_entry(res)) res
}

# The environment of `registry` that holds the block classes of `package` by
# id, or those that an app script defines for `NULL`, or `NULL` where the
# registry holds no class of the package, unless `add` has it make one.
registry_part <- function(registry, package, add = FALSE) {

  if (is.null(package)) {
    return(registry[["script"]])
  }

  packages <- registry[["packages"]]
  part <- get0(package, envir = packages, inherits = FALSE)

  if (is.null(part) && add) {
    part <- new.env(parent = emptyenv())
    assign(package, part, envir = packages)
  }

  part
}

# Puts the entry `x` into `registry`, under the package and the id of its
# class, in place of an entry that the registry holds there.
put_registry_entry <- function(x, registry) {

  part <- registry_part(registry, x[["package"]], add = TRUE)
  assign(x[["id"]], x, envir = part)

  invisible(x)
}

# Removes the entry of the block class whose registry id is `id` from
# `registry`, where the registry holds one.
drop_registry_entry <- function(id, registry) {

  part <- registry_part(registry, id[["package"]])

  if (not_null(part) && exists(id[["id"]], envir = part, inherits = FALSE)) {
    rm(list = id[["id"]], envir = part, inherits = FALSE)
  }

  invisible()
}

#' @rdname register_block
#' @export
available_blocks <- function() {

  rows <- lapply(available_packages(), package_block_rows)

  res <- unique(
    do.call(rbind, c(list(block_rows()), rows, list(registered_block_rows())))
  )
  rownames(res) <- NULL

  res
}

# The block classes of packages that the block registry of the process holds,
# as rows of `available_blocks()`.
registered_block_rows <- function() {

  classes <- Filter(
    function(x) not_null(x[["package"]]),
    lst_xtr(registry_entries(), "block_class")
  )

  block_rows(
    package = chr_xtr(classes, "package"),
    id = chr_xtr(classes, "id")
  )
}

# The entries of the block registry of the process whose classes record
# `package`, or for `NULL`, those of the classes that an app script defined
# itself, which record none.
process_entries <- function(package = NULL) {
  Filter(
    function(x) identical(x[["block_class"]][["package"]], package),
    registry_entries()
  )
}

# Block classes as `available_blocks()` gives them, one row each, with the
# class's package in `package` and its id in `id`.
block_rows <- function(package = character(), id = character()) {
  data.frame(package = package, id = id)
}

#' @param id The id of the block class, as `list_blocks()` gives it in `id`
#' @param package For `create_block()`, the package of the block class, as
#'   `list_blocks()` gives it in `package`, or `NULL` for a class that an app
#'   script defines
#' @rdname register_block
#' @export
create_block <- function(id, ..., package = NULL) {
  entry <- get_registry_entry(pkg_id(package, id))
  block_constructor(entry[["block_class"]])(...)
}

# The entries of a block registry, the process's by default: those of each
# package, in the order of the packages' names, each package's in the order
# of their ids, and then those of the classes that an app script defines, in
# the order of their ids.
registry_entries <- function(registry = block_registry) {

  packages <- registry[["packages"]]

  # Sorted as the C locale sorts, so that the order is the same everywhere
  ids <- function(env) sort(ls(envir = env), method = "radix")

  parts <- c(
    lapply(ids(packages), get, envir = packages),
    list(registry[["script"]])
  )

  unlst(lapply(parts, function(part) lapply(ids(part), get, envir = part)))
}

# The block classes that a package lists, as rows of `available_blocks()`.
package_block_rows <- function(package) {

  entries <- listed_blocks(package)

  block_rows(
    package = rep(package, length(entries)),
    id = chr_xtr(entries, "id")
  )
}

# The block classes that package `pkg` lists in the `blocks` of its
# `._BLOCKR_MODULES_`, by constructor, each with the id of its class and its
# registry metadata, as the roclet writes them into its
# `inst/registry/blocks.yml`, from which the package reads them (see
# `read_blocks_yml()`).
# A part that R cannot read stops the app (see `check_listed_blocks()`).
listed_blocks <- function(pkg, listing = package_listing(pkg)) {

  entries <- listing[["blocks"]]

  check_listed_blocks(entries, pkg)

  entries
}

# A package lists each of its block classes by constructor, with the registry
# metadata of the class that the constructor makes, as the block registration
# roclet writes them.
# R reads each entry's `id`, `name`, `description` and `category` as strings,
# so an entry that lacks one, or gives one as anything else, stops the app,
# with each such fault of each entry, while `register_block()` checks the
# entry's other fields as for any class.
check_listed_blocks <- function(entries, package) {

  if (!is_named_list(entries)) {
    blockr_abort(
      "Package {package} lists its block classes in its `._BLOCKR_MODULES_` ",
      "as no list of block constructors by name, each with the registry ",
      "metadata of its class, as the block registration roclet writes them.",
      class = c("package_blocks_invalid", "package_listing_invalid")
    )
  }

  fields <- c("id", "name", "description", "category")

  faults <- unlst(
    Map(listing_entry_faults, entries, names(entries), list(fields))
  )

  if (!length(faults)) {
    return(invisible())
  }

  no_id <- lgl_ply(
    entries,
    function(x) is_named_list(x) && is.null(x[["id"]])
  )

  hint <- if (any(no_id)) {
    paste(
      "",
      "The block registration roclet of this version of blockr.core writes",
      "the `id` of each class, which older versions leave out, so rebuilding",
      "the package's documentation, as `devtools::document()` does, adds it."
    )
  } else {
    ""
  }

  faults <- paste(faults, collapse = " ")

  blockr_abort(
    "Package {package} lists block classes in its `._BLOCKR_MODULES_` with ",
    "registry metadata that R cannot read. {faults} Each entry needs `id`, ",
    "`name`, `description` and `category`, each a string, as the block ",
    "registration roclet writes them.",
    hint,
    class = c(
      "package_blocks_entry_invalid",
      "package_blocks_invalid",
      "package_listing_invalid"
    )
  )
}

# The block registry of an app, which `serve()` builds as it starts the app,
# and which the app's sessions read alone.
# It holds the block classes that the app offers: those that `blocks` gives,
# whose packages R loads, and the classes that the app script defined and
# registered itself.
# Then `serve()` adds the classes of the board's blocks that it lacks, which
# the app offers too (see `register_board_classes()`).
# Building it reads the listing of each package that the app offers, as it
# registers the package's classes.
app_block_registry <- function(blocks = available_blocks()) {

  offered <- offered_block_classes(blocks)
  registry <- new_block_registry()

  # Reading a package's listing loads the package
  for (pkg in names(offered)) {
    register_offered_classes(pkg, registry, offered[[pkg]])
  }

  for (entry in process_entries()) {
    put_registry_entry(entry, registry)
  }

  registry
}

# The block classes that an app offers, as `serve()` takes them in `blocks`,
# by package: the ids of the package's classes to offer, or `NULL` for all of
# them, from the rows of a data frame (see `check_class_rows()`) or from a YAML
# file at the path (see `yaml_block_rows()`).
# A package that is not available stops the app, and so does an id that its
# package does not list, as R registers the package's classes.
offered_block_classes <- function(blocks) {

  rows <- if (is_string(blocks)) {
    yaml_block_rows(blocks)
  } else {
    check_class_rows(
      blocks,
      "block classes that an app offers",
      "available_blocks()",
      "block_classes_invalid",
      yaml = TRUE
    )
  }

  # A package that the rows name twice offers the classes of both rows, and
  # all of its classes where a row's id is `NA`
  offered <- lapply(
    set_names(nm = unique(rows[["package"]])),
    function(pkg) {
      ids <- rows[["id"]][rows[["package"]] == pkg]
      if (!anyNA(ids)) unique(ids)
    }
  )

  check_block_class_packages(names(offered))

  offered
}

# The rows of a YAML file that lists packages, each as its name, for all of its
# block classes, or as its name with the ids of the classes to offer, as the
# example in `?register_block` shows, where a package that the file lists
# without ids gives one row whose id is `NA`.
yaml_block_rows <- function(path) {

  if (is.na(path) || !file.exists(path) || dir.exists(path)) {
    blockr_abort(
      "There is no file {path} that lists the block classes of an app.",
      class = "block_classes_invalid"
    )
  }

  items <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      blockr_abort(
        "The file {path} holds no YAML: {conditionMessage(e)}",
        class = "block_classes_invalid"
      )
    }
  )

  # A list of package names alone reads as a character vector
  items <- as.list(coal(items, list(), fail_all = FALSE))

  listed <- if (is.null(names(items))) {
    lapply(items, yaml_block_item)
  }

  if (!is.list(listed) || !all(lgl_ply(listed, is.list))) {
    blockr_abort(
      "The file {path} is expected to list packages, each as its name or as ",
      "its name with the ids of the block classes to offer.",
      class = "block_classes_invalid"
    )
  }

  do.call(
    rbind,
    c(
      list(block_rows()),
      lapply(
        listed,
        function(x) {
          block_rows(
            package = x[["package"]],
            id = coal(x[["ids"]], NA_character_, fail_all = FALSE)
          )
        }
      )
    )
  )
}

# A package that a YAML file of block classes lists, as its name, or as its
# name with the ids of its classes to offer, or `NULL` for anything else.
yaml_block_item <- function(x) {

  if (is_nonempty_string(x)) {
    return(list(package = x, ids = NULL))
  }

  valid <- is.list(x) && length(x) == 1L && is_nonempty_string(names(x)) &&
    (is.null(x[[1L]]) || (is.character(x[[1L]]) && !anyNA(x[[1L]])))

  if (valid) list(package = names(x), ids = x[[1L]])
}

check_block_class_packages <- function(packages) {

  missing <- setdiff(
    packages,
    c(available_packages(), registered_block_rows()[["package"]])
  )

  if (length(missing)) {
    blockr_abort(
      "{qty(missing)}Package{?s} {missing}, whose block classes the app ",
      "offers, {?is/are} not available (see `available_blocks()`).",
      class = "block_classes_unavailable"
    )
  }

  invisible()
}

# Registers in `registry` the block classes of `package` that an app offers,
# those whose ids `ids` gives, or all of them for `NULL`: each from the block
# registry of the process where that holds it, in place of the class that the
# package lists, and from the package's listing otherwise.
register_offered_classes <- function(package, registry, ids = NULL) {

  held <- process_entries(package)
  held_ids <- chr_ply(held, function(x) x[["block_class"]][["id"]])

  if (not_null(ids)) {
    held <- held[held_ids %in% ids]
    ids <- setdiff(ids, held_ids)
  }

  register_listed_blocks(package, registry, ids)

  for (entry in held) {
    put_registry_entry(entry, registry)
  }

  invisible()
}

# Registers block class `cls` in `registry` from its registration, as `serve()`
# registers the class of a block on its board: from the block registry of the
# process where that holds the class's registry id, and from the listing of
# the class's package otherwise.
# It returns whether the class has a registration, and registers nothing
# where it has none, as for a class that an app script defined and did not
# register, or one that its package does not list.
register_class <- function(cls, registry) {

  held <- registry_entry(registry_id_from_block(cls), block_registry)

  if (not_null(held)) {
    put_registry_entry(held, registry)
    return(TRUE)
  }

  package <- cls[["package"]]

  if (is.null(package)) {
    return(FALSE)
  }

  if (!cls[["id"]] %in% chr_xtr(listed_blocks(package), "id")) {
    return(FALSE)
  }

  register_listed_blocks(package, registry, cls[["id"]])

  TRUE
}

# Registers the block classes that a package lists in `registry`, or those of
# them whose ids `ids` gives, where an id that the package does not list stops
# the app.
# A package that lists none registers none.
register_listed_blocks <- function(package, registry, ids = NULL) {

  entries <- listed_blocks(package)

  if (not_null(ids)) {

    listed <- chr_xtr(entries, "id")
    unknown <- setdiff(ids, listed)

    if (length(unknown)) {
      blockr_abort(
        "The app offers {qty(unknown)}block class{?es} {unknown} of ",
        "{package}, which its package does not list.",
        class = "block_classes_unknown"
      )
    }

    entries <- entries[listed %in% ids]
  }

  if (!length(entries)) {
    return(invisible())
  }

  ctors <- lapply(
    set_names(nm = names(entries)),
    get,
    envir = asNamespace(package),
    mode = "function"
  )

  check_package_registry_ids(ctors, package)

  invisible(
    Map(
      add_block_entry,
      ctor = ctors,
      name = chr_xtr(entries, "name"),
      description = chr_xtr(entries, "description"),
      link = lapply(entries, `[[`, "link"),
      category = chr_xtr(entries, "category"),
      icon = lapply(entries, `[[`, "icon"),
      guidance = lapply(entries, `[[`, "guidance"),
      keywords = lapply(entries, `[[`, "keywords"),
      MoreArgs = list(registry = registry, overwrite = TRUE)
    )
  )
}

# Registering the block classes of a package refuses two constructors whose
# classes give one registry id, since the second would replace the first.
check_package_registry_ids <- function(ctors, package) {

  ids <- lapply(lapply(ctors, constructor_class), registry_id_from_block)
  twice <- ids[duplicated(ids)]

  same <- if (length(twice)) {
    names(ctors)[lgl_ply(ids, same_pkg_id, twice[[1L]])]
  }

  if (length(same)) {
    blockr_abort(
      "Package {package} declares block constructors {same}, whose classes ",
      "give one registry id, {pkg_id_label(twice[[1L]])}.",
      class = "package_blocks_invalid"
    )
  }

  invisible()
}
