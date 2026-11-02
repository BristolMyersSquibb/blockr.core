# A mock Shiny session for the Shiny bridge, which keeps the custom messages
# and binary frames sent, in their order, the data objects registered and the
# reconnect allowed, so that a test can read what R sent the client and play
# the client's HTTP requests.
bridge_session <- function() {

  session <- shiny::MockShinySession$new()

  session$sent <- list()
  session$data_objs <- list()
  session$reconnect <- NULL

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

  session
}

# Runs the bridge with a handler in a test session and then the code, with
# `session` bound to the session, flushing once the bridge is set up, as Shiny
# flushes after the message that starts a session.
with_bridge <- function(handler, code = NULL, session = bridge_session()) {

  code <- substitute(code)
  env <- parent.frame()

  shiny::testServer(
    function(input, output, session) shiny_bridge(handler, session),
    {
      session$flushReact()
      eval(code, list(session = session), env)
    },
    session = session
  )

  invisible(session)
}

# The messages that R sent the client as text, each as its JSON text.
bridge_sent <- function(session) {
  text <- Filter(function(x) !isTRUE(x[["binary"]]), session$sent)
  chr_ply(text, function(x) unclass(x[["message"]]))
}

# The binary frames that R sent the client, each as the JSON text of its
# message and the body's bytes.
bridge_frames <- function(session) {

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

# Plays an HTTP request to a data object that the bridge registered, as Shiny
# hands it the object's data and the request.
bridge_http <- function(session, name, method = "GET", query = "",
                        body = raw()) {

  obj <- session$data_objs[[name]]

  stopifnot(not_null(obj))

  obj[["filter"]](
    obj[["data"]],
    list2env(
      list(
        REQUEST_METHOD = method,
        QUERY_STRING = query,
        rook.input = bridge_input(body)
      )
    )
  )
}

# Posts a request's body, as the client does before it sends the request.
bridge_post <- function(session, id, body) {
  bridge_http(
    session,
    "blockr-bodies",
    "POST",
    paste0("?w=&nonce=x&id=", id),
    body
  )
}

# A request body as httpuv gives it, read in chunks.
bridge_input <- function(bytes) {

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
