# A mock Shiny session for the Shiny transport, which keeps the custom messages
# and binary frames sent, in their order, the data objects registered and the
# reconnect allowed, so that a test can read what R sent the client and play
# the client's HTTP requests.
# It also keeps what the server function exports for end-to-end tests,
# as Shiny keeps it: each value's expression with the environment to evaluate
# it in, by name.
transport_session <- function() {

  session <- shiny::MockShinySession$new()

  session$sent <- list()
  session$data_objs <- list()
  session$reconnect <- NULL
  session$exports <- list()

  session$sendCustomMessage <- function(type, message) {
    session$sent[[length(session$sent) + 1L]] <- list(
      type = type,
      message = message
    )
  }

  session$sendBinaryMessage <- function(type, message) {
    session$sent[[length(session$sent) + 1L]] <- list(
      type = type,
      message = message,
      binary = TRUE
    )
  }

  session$registerDataObj <- function(name, data, filter) {
    session$data_objs[[name]] <- list(data = data, filter = filter)
    paste0("session/mock/dataobj/", name, "?w=&nonce=", length(session$sent))
  }

  session$allowReconnect <- function(value) {
    session$reconnect <- value
  }

  session$exportTestValues <- function(..., quoted_ = FALSE,
                                       env_ = parent.frame()) {

    exprs <- if (quoted_) list(...) else eval(substitute(alist(...)))

    for (nm in names(exprs)) {
      session$exports[[nm]] <- list(expr = exprs[[nm]], env = env_)
    }

    invisible()
  }

  session
}

# Runs the transport with a handler in a test session and then the code, with
# `session` bound to the session, flushing once the transport is set up, as
# Shiny flushes after the message that starts a session.
with_transport <- function(handler, code = NULL,
                           session = transport_session()) {

  code <- substitute(code)
  env <- parent.frame()

  shiny::testServer(
    function(input, output, session) shiny_transport(handler, session),
    {
      session$flushReact()
      eval(code, list(session = session), env)
    },
    session = session
  )

  invisible(session)
}

# The messages that R sent the client as text, each as its JSON text.
transport_sent <- function(session) {
  text <- Filter(function(x) !isTRUE(x[["binary"]]), session$sent)
  chr_ply(text, function(x) unclass(x[["message"]]))
}

# The binary frames that R sent the client, each as the JSON text of its
# message and the body's bytes.
transport_frames <- function(session) {

  frames <- Filter(function(x) isTRUE(x[["binary"]]), session$sent)

  lapply(
    frames,
    function(x) {
      bytes <- x[["message"]]
      size <- readBin(bytes[1:4], "integer", size = 4L, endian = "little")
      list(
        json = rawToChar(bytes[seq.int(5L, length.out = size)]),
        bytes = bytes[-seq_len(4L + size)]
      )
    }
  )
}

# Plays an HTTP request to a data object that the transport registered, as Shiny
# hands it the object's data and the request.
transport_http <- function(session, name, method = "GET", query = "",
                           body = raw()) {

  obj <- session$data_objs[[name]]

  stopifnot(not_null(obj))

  obj[["filter"]](
    obj[["data"]],
    list2env(
      list(
        REQUEST_METHOD = method,
        QUERY_STRING = query,
        rook.input = transport_input(body)
      )
    )
  )
}

# Posts a request's body, as the client does before it sends the request.
transport_post <- function(session, id, body) {
  transport_http(
    session,
    "blockr-bodies",
    "POST",
    paste0("?w=&nonce=x&id=", id),
    body
  )
}

# A request body as httpuv gives it, read in chunks.
transport_input <- function(bytes) {

  pos <- 0L

  list(
    read = function(l = -1L) {

      if (l < 0L) {
        l <- length(bytes) - pos
      }

      n <- min(l, length(bytes) - pos)

      if (n <= 0L) {
        return(raw())
      }

      res <- bytes[seq.int(pos + 1L, pos + n)]
      pos <<- pos + n

      res
    }
  )
}

# Runs the server function of an app that serve() returned in a test session,
# sends it the client's messages in turn, each a list of requests, as the
# transport carries them, and runs what R queued for each.
# It gives the replies that R sent, in their order, after the message that
# starts the session on the client.
served_replies <- function(app, ..., session = transport_session()) {

  replies <- list()

  shiny::testServer(
    app,
    {
      session$flushReact()

      for (message in list(...)) {

        session$setInputs(blockr = unclass(encode_message(message)))

        while (!later::loop_empty()) {
          later::run_now()
        }
      }

      for (sent in transport_sent(session)[-1L]) {
        replies <<- c(replies, decode_message(sent))
      }
    },
    session = session
  )

  replies
}

# Runs the server function of an app that serve() returned as
# `served_replies()` does, and gives what the app exports for end-to-end tests
# once R has answered the messages, by name, as Shiny gives it in test mode.
served_exports <- function(app, ...) {

  session <- transport_session()
  served_replies(app, ..., session = session)

  lapply(session$exports, function(x) eval(x$expr, x$env))
}
