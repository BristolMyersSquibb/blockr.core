registered_option_requests <- new.env()

registered_extension_requests <- new.env()

#' Requests of board options and extensions
#'
#' A request asks R for an answer that the client needs, by running a function
#' that an object registers in R. A block class names its own requests (see
#' [new_block_request()]). An output class and a renderer are registered with
#' their requests (see [register_output_class()] and [register_renderer()]).
#' A board option and an extension have no definition in R, so a package
#' registers their requests with `register_requests()`, from its `.onLoad()`.
#' It registers those of a board option under the board option's ID, in
#' `option`. It registers those of an extension under the extension's name, in
#' `extension`, which gives the extension's ID `{pkg}-{name}`, as a registry
#' id is. The client names the board option or the extension by that ID when
#' it sends a request.
#'
#' Registering the requests of a board option or an extension again replaces
#' them, as when a package is loaded again. A package cannot register requests
#' under an ID that another package has taken.
#'
#' R calls a board option's request with the arguments that the client passes,
#' by name. It calls an extension's request with those arguments as well, and
#' with the blocks that the request reads, in its argument `reads`. That is a
#' list by block ID, and each entry is a list of the block's `result` and the
#' block expression `expr` that R built for it. A request that receives a
#' file, such as an upload, gets it in its argument `file`. That is a list of
#' the path of the temporary file that R wrote the bytes to, in `datapath`,
#' and the file's `name`, `type` and `size`. R binds `reads` and `file` only
#' where the request's function takes them, and the client passes neither.
#'
#' A request answers with a value, which R writes with every vector as an
#' array, or with a file, as a reply body that [new_reply_body()] makes. The
#' client never keeps the answer of a board option's or an extension's
#' request, and such a request may write. A package names in `allow_locked`
#' the requests that may run on a locked board, such as one that only reads,
#' as the code export does.
#'
#' @param requests A named list of the requests, each a function
#' @param option The ID of the board option whose requests these are
#' @param extension The name part of the ID of the extension whose requests
#'   these are
#' @param allow_locked The names of the requests that may run on a locked
#'   board
#' @param package The package that registers the requests (`NULL` outside a
#'   package, where only a board option's requests can be registered)
#'
#' @examples
#' register_requests(
#'   list(list_dir = function(path = ".") list.files(path)),
#'   option = "data_dir"
#' )
#'
#' \dontrun{
#' # In the `.onLoad()` of a package such as blockr.code
#' register_requests(
#'   list(script = function(reads) export_script(reads)),
#'   extension = "code",
#'   allow_locked = "script"
#' )
#' }
#'
#' @return The requests' entry, invisibly: a list holding the `id` of the
#' board option or the extension, the `package` that registered them, the
#' `requests` and `allow_locked`.
#'
#' @export
register_requests <- function(requests, option = NULL, extension = NULL,
                              allow_locked = character(),
                              package = pkg_name(parent.frame())) {

  if (is.null(option) == is.null(extension)) {
    blockr_abort(
      "Requests are registered for either a board option, in `option`, or an ",
      "extension, in `extension`.",
      class = "registered_requests_invalid"
    )
  }

  if (not_null(option)) {

    if (!is_nonempty_string(option)) {
      blockr_abort(
        "A board option whose requests are registered is named by its ID, a ",
        "non-empty string.",
        class = "registered_requests_invalid"
      )
    }

    id <- option
    what <- paste("board option", option)
    env <- registered_option_requests

  } else {

    if (!is_nonempty_string(package)) {
      blockr_abort(
        "The requests of an extension are registered by a package, from its ",
        "`.onLoad()`.",
        class = "registered_requests_invalid"
      )
    }

    if (!is_nonempty_string(extension)) {
      blockr_abort(
        "The name of an extension of package {package} is expected to be a ",
        "non-empty string.",
        class = "registered_requests_invalid"
      )
    }

    id <- paste0(package, "-", extension)
    what <- paste("extension", id)
    env <- registered_extension_requests
  }

  if (!is.list(requests) || !all(lgl_ply(requests, is.function)) ||
        !has_unique_names(requests)) {
    blockr_abort(
      "The requests of ", what, " are expected to be a list of functions ",
      "with unique names.",
      class = "registered_requests_invalid"
    )
  }

  if (!is.character(allow_locked) || anyNA(allow_locked) ||
        !all(allow_locked %in% names(requests))) {
    blockr_abort(
      "The requests of ", what, " that may run on a locked board are ",
      "expected to be named among its requests.",
      class = "registered_requests_invalid"
    )
  }

  known <- get0(id, envir = env, inherits = FALSE)

  if (not_null(known) && !identical(known[["package"]], package)) {

    owner <- coal(known[["package"]], "a script", fail_all = FALSE)

    blockr_abort(
      "The requests of ", what, " are registered already, by ", owner, ".",
      class = "registered_requests_taken"
    )
  }

  entry <- list(
    id = id,
    package = package,
    requests = requests,
    allow_locked = allow_locked
  )

  assign(id, entry, envir = env)

  invisible(entry)
}

# The function of a request that a package registered, by the object's ID and
# the request's name. The object is a board option or an extension, as `what`
# says.
registered_request <- function(what, id, name) {

  env <- switch(
    what,
    option = registered_option_requests,
    extension = registered_extension_requests
  )

  fun <- get0(id, envir = env, inherits = FALSE)[["requests"]][[name]]

  if (!is.function(fun)) {
    blockr_abort(
      "No package registers a request {name} for ",
      if (identical(what, "option")) "board option " else "extension ",
      "{id}.",
      class = "registered_request_unknown"
    )
  }

  fun
}

unregister_requests <- function(option = character(), extension = character()) {
  rm(list = option, envir = registered_option_requests, inherits = FALSE)
  rm(list = extension, envir = registered_extension_requests, inherits = FALSE)
  invisible()
}
