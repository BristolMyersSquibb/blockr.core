# The Shiny transport, which carries the messages of the wire protocol between
# the client and R over a Shiny session, as inst/protocol/README.md gives them,
# and the binary bodies beside them. Its other half is the client's
# srcts/transport/shiny.ts. Shiny only carries the messages, so leaving it later
# replaces these two files and nothing else.

# Sets the transport up for a session, handing each message that the client
# sends to the handler, the session's evaluator, as `handler(message, send)`:
# the message decoded by `decode_message()`, a list of requests, and `send()`,
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
# until the session ends. A reply's body is given in its `body` too, as a reply
# body that `new_reply_body()` makes, such as `shape_output()` gives for an
# output, which `send()` carries beside the reply as the blockr options
# `max_message_body` and `message_body_encoding` say, which the transport reads
# as it sets a session up (see `carry_body()`).
shiny_transport <- function(handler, session = get_session()) {

  if (!is.function(handler)) {
    blockr_abort(
      "The Shiny transport's handler is a function.",
      class = "shiny_transport_handler_invalid"
    )
  }

  if (is.null(session)) {
    blockr_abort(
      "The Shiny transport needs a Shiny session, as a server function has.",
      class = "shiny_transport_session_missing"
    )
  }

  opts <- message_body_opts()

  dir <- tempfile("blockr-bodies-")
  dir.create(dir)

  session$onSessionEnded(function() unlink(dir, recursive = TRUE))

  bodies <- new.env(parent = emptyenv())

  send <- function(message) send_message(message, session, opts)

  body_url <- session$registerDataObj(
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
    json_str(session_start(body_url), auto_unbox = TRUE)
  )

  observeEvent(
    session$input[["blockr"]],
    receive_message(session$input[["blockr"]], handler, send, bodies),
    ignoreInit = TRUE
  )

  invisible()
}

# R's message that starts a session on the client, with the URL that the client
# posts request bodies to and, where Shiny limits a request's size, the limit,
# so that the client refuses a larger body itself, where a refusal from Shiny
# can reach the browser as a connection that was reset while the browser still
# sent the body.
session_start <- function(body_url) {

  max <- getOption("shiny.maxRequestSize", 5 * 1024^2)

  if (is.numeric(max) && max > 0) {
    return(list(body_url = body_url, max_request_size = max))
  }

  list(body_url = body_url)
}

receive_message <- function(text, handler, send, bodies) {

  # Checked first, since yyjsonr prints the text where it fails to parse it.
  message <- if (is_string(text) && yyjsonr::validate_json_str(text)) {
    tryCatch(decode_message(text), error = function(e) NULL)
  }

  if (!is.list(message) || not_null(names(message))) {

    log_warn("The Shiny transport ignores a message that is no array.")

    return(invisible())
  }

  message <- with_bodies(message, bodies, send)

  tryCatch(
    handler(message, send),
    error = function(e) {
      log_error("The Shiny transport's handler failed: {conditionMessage(e)}")
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
      class = "shiny_transport_request_body_missing"
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

# Receives a request's body, which the client posts to the URL that R
# gives it, naming the request in the query's `id`, before it sends the
# request. Shiny refuses a body over its option `shiny.maxRequestSize`, 5 MB by
# default, before R reads it, as it refuses what `shiny::fileInput()` uploads,
# and httpuv writes the bytes to a file as they arrive, so that R holds no
# large body in memory.
receive_body <- function(req, dir, bodies) {

  if (!identical(req[["REQUEST_METHOD"]], "POST")) {
    return(httpResponse(405L, "text/plain", "A body is posted."))
  }

  query <- coal(req[["QUERY_STRING"]], "", fail_all = FALSE)
  id <- parseQueryString(query)[["id"]]

  if (!is_string(id) || !nzchar(id)) {
    return(httpResponse(400L, "text/plain", "A body names its request."))
  }

  path <- tempfile("body-", tmpdir = dir)
  write_input(req[["rook.input"]], path)

  assign(id, list(path = path, size = file.size(path)), envir = bodies)

  httpResponse(200L, "text/plain", "")
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
      class = "shiny_transport_message_invalid"
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
# messages behind it on the websocket. A reply whose `body` is no reply body
# goes as it is.
carry_body <- function(item, session, opts) {

  body <- if (is.list(item)) item[["body"]]

  if (!is_reply_body(body)) {
    return(list(item = item))
  }

  x <- body[["content"]]
  size <- if (is.raw(x)) length(x) else file.size(x)

  item[["body"]] <- body_announcement(body)

  if (size > opts[["max"]]) {
    item[["body"]][["url"]] <- serve_body(
      x,
      body[["type"]],
      item[["id"]],
      session
    )
    return(list(item = item))
  }

  bytes <- reply_body_bytes(body)

  if (identical(opts[["encoding"]], "binary")) {
    return(list(item = item, frame = bytes))
  }

  item[["body"]][["data"]] <- base64enc::base64encode(bytes)

  list(item = item)
}

# How bodies travel, from the blockr options `message_body_encoding`, how a
# body travels in its reply's message, `"base64"` by default or `"binary"`, a
# binary frame, which costs R and the client less, but reaches the page only
# where Shiny's websocket is not wrapped in SockJS, as Shiny Server and Connect
# wrap it, and `max_message_body`, the largest body in bytes that travels in
# its message, with 0 for every body from a URL and `Inf` for every body in its
# message. Its default follows the encoding: 1 MiB for base64, from which a URL
# delivers a body sooner while R has nothing else to run and base64 starts to
# hold up the messages behind it, and 4 MiB for binary frames, which keep up
# with a URL to about 5 MB. Below its default, a body in the message also
# arrives while R runs what comes next, which a body from a URL waits for.
message_body_opts <- function() {

  encoding <- blockr_option("message_body_encoding", "base64")

  if (!is_string(encoding) || !encoding %in% c("base64", "binary")) {
    blockr_abort(
      "The blockr option `message_body_encoding` is \"base64\" or \"binary\".",
      class = "message_body_encoding_invalid"
    )
  }

  max <- blockr_option(
    "max_message_body",
    if (identical(encoding, "binary")) 4 * 2^20 else 2^20
  )

  # An environment variable gives a string.
  if (is.character(max)) {
    max <- suppressWarnings(as.numeric(max))
  }

  if (!is.numeric(max) || !is_scalar(max) || is.na(max) || max < 0) {
    blockr_abort(
      "The blockr option `max_message_body` is a number of bytes, 0 or more.",
      class = "max_message_body_invalid"
    )
  }

  list(max = max, encoding = encoding)
}

# Serves a reply's body, from memory or from its file, at a URL of the
# session's, named by the reply's id, to every GET until the client deletes
# it, which the client does once it has read the body, since only it knows
# when it has all of it. A GET that the browser or a proxy sends again, after a
# connection broke before the body arrived, is thus still answered. What takes
# the body's place, `body_gone()`, is defined here rather than where the body
# is served, so that it holds on to nothing, where a function made in the
# serving one would hold its frame and so the body. A body that the client
# never deletes stays until the session ends, and the file of a body given as
# one stays.
serve_body <- function(x, type, id, session) {

  name <- paste0("blockr-body-", id)

  session$registerDataObj(
    name,
    x,
    function(data, req) {

      method <- coal(req[["REQUEST_METHOD"]], "GET", fail_all = FALSE)

      # Answered with 200 and a text, where Chrome, given 204, failed the next
      # request that it sent over the same connection more often than not.
      if (identical(method, "DELETE")) {
        session$registerDataObj(name, NULL, body_gone)
        return(httpResponse(200L, "text/plain", "Deleted."))
      }

      if (!identical(method, "GET")) {
        return(httpResponse(405L, "text/plain", "A body is got or deleted."))
      }

      httpResponse(
        200L,
        type,
        if (is.raw(data)) data else list(file = data, owned = FALSE)
      )
    }
  )
}

body_gone <- function(data, req) {
  httpResponse(404L, "text/plain", "The body was fetched already.")
}
