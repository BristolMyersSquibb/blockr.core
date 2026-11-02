# The Shiny bridge, which carries the messages of the wire protocol between the
# client and R over a Shiny session, as inst/protocol/README.md gives them, and
# the binary bodies beside them. Its other half is the client's
# srcts/bridge/shiny.ts. Shiny only carries the messages, so leaving it later
# replaces these two files and nothing else.

# Sets the bridge up for a session, handing each message that the client sends
# to the handler, the session's evaluator, as `handler(message, send)`: the
# message decoded by `decode_message()`, a list of requests, and `send()`,
# which sends a message of R's, an unnamed list of replies and progress notes,
# with `session$sendCustomMessage()`, which writes to the websocket at once, so
# that progress reaches the client while a request runs.
#
# The client sends each message as its JSON text, through
# `Shiny.setInputValue("blockr", ...)`, since Shiny's own decoding leaves every
# array a list and flattens a message that is itself an array. The observer
# returns no promise, so that Shiny never holds the session's inputs while a
# request runs, and it ignores the value that the input starts a session with,
# which Shiny replays from the page when the page starts a new session after
# the server ended the last, so that R never sees the last message that the
# page sent to the session before.
#
# A request's body reaches the handler as the request's `body`, a list of its
# `name`, where it has one, its `size`, its `type` and in `datapath` the file
# that R wrote its bytes to, as `shiny::fileInput()` gives a file, which stays
# until the session ends. A reply's body is given in its `body` too, as a list
# of the bytes, a raw vector or a file's path, in `x`, the MIME type in `type`
# and, for a download, a file name in `name`, which `send()` carries beside the
# reply as the blockr options `max_message_body` and `message_body_encoding`
# say, which the bridge reads as it sets a session up (see `carry_body()`).
shiny_bridge <- function(handler, session = shiny::getDefaultReactiveDomain()) {

  stopifnot(is.function(handler), not_null(session))

  opts <- message_body_opts()

  dir <- tempfile("blockr-bodies-")
  dir.create(dir)

  session$onSessionEnded(function() unlink(dir, recursive = TRUE))

  bodies <- new.env(parent = emptyenv())

  send <- function(message) send_message(message, session, opts)

  upload <- session$registerDataObj(
    "blockr-bodies",
    NULL,
    function(data, req) receive_body(req, dir, bodies)
  )

  # The page then starts a new session after the server ended the last, as
  # Shiny Server and Connect let it for a session they no longer know.
  session$allowReconnect(TRUE)

  # Starts the session on the client, which then sends its hello, and gives it
  # the URL that it posts request bodies to.
  session$sendCustomMessage(
    "blockr",
    json_str(session_start(upload), auto_unbox = TRUE)
  )

  shiny::observeEvent(
    session$input[["blockr"]],
    receive_message(session$input[["blockr"]], handler, send, bodies),
    ignoreInit = TRUE
  )

  invisible()
}

# The bridge's message that starts a session on the client, with the URL that
# the client posts request bodies to and, where Shiny limits a request's size,
# the limit, so that the client refuses a larger body itself, where a refusal
# from Shiny can reach the browser as a connection that was reset while the
# browser still sent the body.
session_start <- function(upload) {

  max <- getOption("shiny.maxRequestSize", 5 * 1024^2)

  if (is.numeric(max) && max > 0) {
    return(list(upload = upload, max_request_size = max))
  }

  list(upload = upload)
}

receive_message <- function(text, handler, send, bodies) {

  # Checked first, since yyjsonr prints the text where it fails to parse it.
  message <- if (is_string(text) && yyjsonr::validate_json_str(text)) {
    tryCatch(decode_message(text), error = function(e) NULL)
  }

  if (!is.list(message) || not_null(names(message))) {

    log_warn("The Shiny bridge ignores a message that is no array.")

    return(invisible())
  }

  message <- with_bodies(message, bodies, send)

  tryCatch(
    handler(message, send),
    error = function(e) {
      log_error("The Shiny bridge's handler failed: {conditionMessage(e)}")
    }
  )

  invisible()
}

# Gives each request that announces a body the file that the client posted for
# it, and replies `error` to a request whose body never came, which the handler
# then never sees.
with_bodies <- function(message, bodies, send) {

  lost <- character()
  drop <- logical(length(message))

  for (i in seq_along(message)) {

    req <- message[[i]]

    if (!is.list(req) || !is.list(req[["body"]])) {
      next
    }

    id <- req[["id"]]
    file <- if (is_string(id)) get0(id, envir = bodies, inherits = FALSE)

    if (is.null(file)) {

      if (is_string(id)) {
        lost <- c(lost, id)
      }

      drop[i] <- TRUE

      next
    }

    message[[i]][["body"]] <- filter_empty(
      list(
        name = req[["body"]][["name"]],
        size = file[["size"]],
        type = req[["body"]][["type"]],
        datapath = file[["path"]]
      )
    )
  }

  if (length(lost)) {
    send(lapply(lost, lost_body_reply))
  }

  message[!drop]
}

lost_body_reply <- function(id) {

  cnd <- tryCatch(
    blockr_abort(
      "R received no body for request {id}, which announces one.",
      class = "blockr_no_body"
    ),
    error = identity
  )

  # R raised it itself, so it has no phase.
  rec <- condition_record(cnd, NULL)
  rec[["phase"]] <- NULL

  list(
    id = id,
    status = "error",
    conditions = list(rec),
    omitted = list(warning = 0L, message = 0L)
  )
}

# Receives a request's body, which the client posts to the URL that the bridge
# gives it, naming the request in the query's `id`, before it sends the
# request. Shiny refuses a body over its option `shiny.maxRequestSize`, 5 MB by
# default, before R reads it, as it refuses what `shiny::fileInput()` uploads,
# and httpuv writes the bytes to a file as they arrive, so that R holds no
# large body in memory.
receive_body <- function(req, dir, bodies) {

  if (!identical(req[["REQUEST_METHOD"]], "POST")) {
    return(shiny::httpResponse(405L, "text/plain", "A body is posted."))
  }

  query <- coal(req[["QUERY_STRING"]], "", fail_all = FALSE)
  id <- shiny::parseQueryString(query)[["id"]]

  if (!is_string(id) || !nzchar(id)) {
    return(
      shiny::httpResponse(400L, "text/plain", "A body names its request.")
    )
  }

  path <- tempfile("body-", tmpdir = dir)
  write_input(req[["rook.input"]], path)

  assign(id, list(path = path, size = file.size(path)), envir = bodies)

  shiny::httpResponse(200L, "text/plain", "")
}

write_input <- function(input, path) {

  con <- file(path, open = "wb")
  on.exit(close(con))

  repeat {

    chunk <- input$read(2^20)

    if (!length(chunk)) {
      break
    }

    writeBin(chunk, con)
  }

  invisible(path)
}

# Sends a message of R's in the order of its items, with a reply whose body
# travels in a binary frame in a frame of its own, and the items between such
# replies together.
send_message <- function(message, session, opts = message_body_opts()) {

  if (!is.list(message) || not_null(names(message))) {
    blockr_abort(
      "A message that R sends is an unnamed list of replies and progress ",
      "notes.",
      class = "blockr_message_invalid"
    )
  }

  parts <- lapply(message, carry_body, session, opts)
  run <- list()

  for (part in parts) {

    if (is.null(part[["frame"]])) {
      run <- c(run, list(part[["item"]]))
      next
    }

    send_items(run, session)
    run <- list()

    send_frame(part[["item"]], part[["frame"]], session)
  }

  send_items(run, session)

  invisible()
}

send_items <- function(items, session) {

  if (length(items)) {
    session$sendCustomMessage("blockr", encode_message(items))
  }

  invisible()
}

# Sends a reply with its body as a binary frame, which holds the length of the
# JSON of a message of the reply alone in four bytes, little-endian, that JSON
# and then the body's bytes.
send_frame <- function(item, bytes, session) {

  json <- charToRaw(enc2utf8(unclass(encode_message(list(item)))))
  size <- writeBin(length(json), raw(), size = 4L, endian = "little")

  session$sendBinaryMessage("blockr", c(size, json, bytes))

  invisible()
}

# Carries a reply's body beside it, giving the reply and, for a body that
# travels in a binary frame, its bytes: a body of up to `max_message_body`
# bytes in the message, as base64 in the body's `data` or in a binary frame as
# `message_body_encoding` says, and a larger one from a URL of the session's in
# its `url`, which the client fetches. A small body, such as a plot's PNG, thus
# arrives with its reply, while one that R serves arrives only once R has
# finished what it runs next, and a large body holds up neither R nor the
# messages behind it on the websocket. A reply whose `body` holds no bytes in
# `x` goes as it is.
carry_body <- function(item, session, opts) {

  body <- if (is.list(item)) item[["body"]]

  if (!is.list(body) || is.null(body[["x"]])) {
    return(list(item = item))
  }

  x <- body[["x"]]
  type <- body[["type"]]

  if (!(is.raw(x) || (is_string(x) && file.exists(x))) || !is_string(type)) {
    blockr_abort(
      "A reply's body holds its bytes, as a raw vector or the path of a ",
      "file, and their MIME type.",
      class = "blockr_body_invalid"
    )
  }

  size <- if (is.raw(x)) length(x) else file.size(x)

  item[["body"]] <- filter_empty(list(type = type, name = body[["name"]]))

  if (size > opts[["max"]]) {
    item[["body"]][["url"]] <- serve_body(x, type, item[["id"]], session)
    return(list(item = item))
  }

  bytes <- if (is.raw(x)) x else readBin(x, "raw", size)

  if (identical(opts[["encoding"]], "binary")) {
    return(list(item = item, frame = bytes))
  }

  item[["body"]][["data"]] <- base64enc::base64encode(bytes)

  list(item = item)
}

# How bodies travel, from the blockr options `max_message_body`, the largest
# body in bytes that travels in its reply's message, 1 MiB by default, which R
# writes as base64 in about 30 ms, with 0 for every body from a URL and `Inf`
# for every body in its message, and `message_body_encoding`, how a body
# travels in its message, `"base64"` by default or `"binary"`, a binary frame,
# which costs R and the client less, but reaches the page only where Shiny's
# websocket is not wrapped in SockJS, as Shiny Server and Connect wrap it.
message_body_opts <- function() {

  max <- blockr_option("max_message_body", 2^20)
  encoding <- blockr_option("message_body_encoding", "base64")

  # An environment variable gives a string.
  if (is.character(max)) {
    max <- suppressWarnings(as.numeric(max))
  }

  if (!is.numeric(max) || !is_scalar(max) || is.na(max) || max < 0) {
    blockr_abort(
      "The blockr option `max_message_body` is a number of bytes, 0 or more.",
      class = "blockr_option_invalid"
    )
  }

  if (!is_string(encoding) || !encoding %in% c("base64", "binary")) {
    blockr_abort(
      "The blockr option `message_body_encoding` is \"base64\" or \"binary\".",
      class = "blockr_option_invalid"
    )
  }

  list(max = max, encoding = encoding)
}

# Serves a reply's body once, from memory or from its file, at a URL of the
# session's, named by the reply's id, and then drops it, so that R holds no
# body that the client has fetched. What takes its place, `body_gone()`, is
# defined here rather than where the body is served, so that it holds on to
# nothing, where a function made in the serving one would hold its frame and
# so the body. A body that the client never fetches stays until the session
# ends, and the file of a body given as one stays.
serve_body <- function(x, type, id, session) {

  name <- paste0("blockr-body-", id)

  session$registerDataObj(
    name,
    x,
    function(data, req) {

      force(data)

      session$registerDataObj(name, NULL, body_gone)

      shiny::httpResponse(
        200L,
        type,
        if (is.raw(data)) data else list(file = data, owned = FALSE)
      )
    }
  )
}

body_gone <- function(data, req) {
  shiny::httpResponse(404L, "text/plain", "The body was fetched already.")
}
