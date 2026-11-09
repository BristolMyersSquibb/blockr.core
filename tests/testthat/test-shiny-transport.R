test_that("the transport starts a session that the page may start anew", {

  withr::local_options(shiny.maxRequestSize = NULL)

  session <- with_transport(function(message, send) NULL)

  expect_true(session$reconnect)

  expect_identical(
    decode_message(transport_sent(session)),
    list(
      body_url = "session/mock/dataobj/blockr-bodies?w=&nonce=0",
      max_request_size = 5 * 1024^2,
      max_message_size = 4 * 2^20
    )
  )
  expect_s3_class(session$sent[[1L]][["message"]], "json")
  expect_identical(session$sent[[1L]][["type"]], "blockr")

  # Shiny takes a body of any size where its limit is 0, and R sends a message
  # of any size where its own is `Inf`.
  withr::local_options(shiny.maxRequestSize = 0, blockr.max_message_size = Inf)

  session <- with_transport(function(message, send) NULL)

  expect_named(decode_message(transport_sent(session)), "body_url")
})

test_that("the transport needs a handler and a session", {

  expect_error(
    with_transport("echo"),
    class = "shiny_transport_handler_invalid"
  )

  # Outside a server function, get_session() finds no session.
  expect_error(
    shiny_transport(function(message, send) NULL),
    class = "shiny_transport_session_missing"
  )
})

test_that("the transport hands each message the client sends to the handler", {

  got <- list()

  with_transport(
    function(message, send) got[[length(got) + 1L]] <<- message,
    {

      for (name in protocol_fixtures()) {

        # The client's message as the client sends it, alone.
        client <- yyjsonr::read_json_str(
          protocol_fixture(name),
          opts = json_read_opts(length1_array_asis = TRUE)
        )[["client"]]
        client <- yyjsonr::write_json_str(
          client,
          opts = yyjsonr::opts_write_json(auto_unbox = TRUE)
        )

        if (name %in% c("request-action", "request-output")) {
          transport_post(session, decode_message(client)[[1L]][["id"]], raw(1L))
        }

        session$setInputs(blockr = client)
      }
    }
  )

  expect_length(got, length(protocol_fixtures()))

  for (i in seq_along(got)) {

    name <- protocol_fixtures()[i]
    expected <- decode_message(protocol_fixture(name))[["client"]]

    if (name %in% c("request-action", "request-output")) {
      expected[[1L]][["body"]] <- got[[i]][[1L]][["body"]]
    }

    expect_identical(got[[i]], expected, label = name)
  }
})

test_that("the transport ignores the value that the input starts with", {

  got <- list()

  session <- transport_session()

  # Shiny replays the page's last message when it starts a new session.
  session$setInputs(blockr = '[{"kind": "forget", "blocks": ["a"]}]')

  with_transport(
    function(message, send) got[[length(got) + 1L]] <<- message,
    session$setInputs(blockr = '[{"id": "1", "kind": "hello"}]'),
    session
  )

  expect_identical(got, list(list(list(id = "1", kind = "hello"))))
})

test_that("the transport sends R's messages as encode_message() writes them", {

  values <- protocol_r_values()

  with_transport(
    function(message, send) NULL,
    {

      for (name in protocol_fixtures()) {

        expected <- read_json_strictly(protocol_fixture(name))[["r"]]

        for (i in seq_along(expected)) {

          send_message(values[[name]][[i]], session)

          expect_identical(
            read_json_strictly(utils::tail(transport_sent(session), 1L)),
            expected[[i]],
            label = paste0("message ", i, " the transport sends for ", name)
          )
        }
      }
    }
  )
})

test_that("the handler's send() sends a message", {

  session <- with_transport(
    function(message, send) {
      send(list(list(id = message[[1L]][["id"]], status = "ok", value = 1L)))
    },
    session$setInputs(blockr = '[{"id": "7", "kind": "blockr_options"}]')
  )

  expect_identical(
    utils::tail(transport_sent(session), 1L),
    '[{"id":"7","status":"ok","value":1}]'
  )
})

test_that("the transport refuses to send what is no unnamed list", {

  with_transport(
    function(message, send) NULL,
    {
      expect_error(
        send_message(list(id = "1", status = "ok"), session),
        class = "shiny_transport_message_invalid"
      )
      expect_error(
        send_message("[]", session),
        class = "shiny_transport_message_invalid"
      )
    }
  )
})

test_that("a small body travels in its reply's message", {

  file <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b", file)

  with_transport(
    function(message, send) NULL,
    {

      send_message(
        list(
          list(
            id = "1",
            status = "ok",
            body = new_reply_body(as.raw(1:3), "image/png")
          ),
          list(
            id = "2",
            status = "ok",
            body = new_reply_body(file, "text/csv", name = "a.csv")
          )
        ),
        session
      )

      sent <- decode_message(utils::tail(transport_sent(session), 1L))

      expect_identical(
        sent[[1L]][["body"]],
        list(type = "image/png", data = base64enc::base64encode(as.raw(1:3)))
      )

      expect_identical(
        sent[[2L]][["body"]],
        list(
          type = "text/csv",
          name = "a.csv",
          data = base64enc::base64encode(readBin(file, "raw", 100L))
        )
      )
    }
  )
})

test_that("a large body is served from a URL until the client deletes it", {

  big <- as.raw(rep(0:255, length.out = 2^20 + 1))

  file <- withr::local_tempfile(fileext = ".bin")
  writeBin(big, file)

  with_transport(
    function(message, send) NULL,
    {

      send_message(
        list(
          list(
            id = "3",
            status = "ok",
            body = new_reply_body(big, "application/octet-stream")
          ),
          list(
            id = "4",
            status = "ok",
            body = new_reply_body(file, "text/plain", name = "big.txt")
          )
        ),
        session
      )

      sent <- decode_message(utils::tail(transport_sent(session), 1L))

      expect_identical(
        sent[[1L]][["body"]],
        list(
          type = "application/octet-stream",
          url = "session/mock/dataobj/blockr-body-3?w=&nonce=1"
        )
      )

      res <- transport_http(session, "blockr-body-3")

      expect_identical(res[["status"]], 200L)
      expect_identical(res[["content_type"]], "application/octet-stream")
      expect_identical(res[["content"]], big)

      # Until the client deletes it, R serves the body to every GET, one that
      # the browser sends again included.
      expect_identical(
        transport_http(session, "blockr-body-3")[["content"]],
        big
      )
      expect_identical(
        transport_http(session, "blockr-body-3", "POST")[["status"]],
        405L
      )

      res <- transport_http(session, "blockr-body-3", "DELETE")
      expect_identical(res[["status"]], 200L)

      # Deleted, the bytes are dropped, by what takes their place too.
      expect_identical(
        transport_http(session, "blockr-body-3")[["status"]],
        404L
      )
      expect_null(session$data_objs[["blockr-body-3"]][["data"]])
      expect_identical(
        session$data_objs[["blockr-body-3"]][["filter"]],
        body_gone
      )

      expect_identical(
        sent[[2L]][["body"]][c("type", "name")],
        list(type = "text/plain", name = "big.txt")
      )

      res <- transport_http(session, "blockr-body-4")

      expect_identical(
        res[["content"]],
        list(file = normalizePath(file), owned = FALSE)
      )
    }
  )

  # The file of a body given as one stays.
  expect_true(file.exists(file))
})

test_that("the blockr options set which bodies travel in their message", {

  reply <- function(id, n) {
    list(
      id = id,
      status = "ok",
      body = new_reply_body(as.raw(rep(1L, n)), "image/png")
    )
  }

  sent_bodies <- function() {

    session <- with_transport(
      function(message, send) NULL,
      send_message(list(reply("1", 3L), reply("2", 11L)), session)
    )

    sent <- decode_message(utils::tail(transport_sent(session), 1L))

    lapply(sent, `[[`, "body")
  }

  withr::with_options(list(blockr.max_message_body = 0), {
    expect_named(sent_bodies()[[1L]], c("type", "url"))
  })

  withr::with_options(list(blockr.max_message_body = Inf), {
    expect_named(sent_bodies()[[2L]], c("type", "data"))
  })

  # An environment variable gives the option as a string.
  withr::with_envvar(c(BLOCKR_MAX_MESSAGE_BODY = "10"), {
    bodies <- sent_bodies()
    expect_named(bodies[[1L]], c("type", "data"))
    expect_named(bodies[[2L]], c("type", "url"))
  })
})

test_that("a body travels in a binary frame where the blockr option says so", {

  withr::local_options(blockr.message_body_encoding = "binary")

  png <- as.raw(c(0x89, 0x50, 0x4e, 0x47))

  session <- with_transport(
    function(message, send) NULL,
    send_message(
      list(
        list(id = "1", progress = list(value = 0.5)),
        list(
          id = "1",
          status = "ok",
          body = new_reply_body(png, "image/png", name = "a.png")
        ),
        list(id = "2", status = "ok", value = 1L)
      ),
      session
    )
  )

  # In the order of the message's items, the reply with its body in a frame of
  # its own between the others.
  expect_identical(
    lgl_ply(utils::tail(session$sent, 3L), function(x) isTRUE(x[["binary"]])),
    c(FALSE, TRUE, FALSE)
  )
  expect_identical(
    utils::tail(transport_sent(session), 2L),
    c(
      '[{"id":"1","progress":{"value":0.5}}]',
      '[{"id":"2","status":"ok","value":1}]'
    )
  )

  frames <- transport_frames(session)

  expect_length(frames, 1L)
  expect_identical(
    frames[[1L]][["json"]],
    '[{"id":"1","status":"ok","body":{"type":"image/png","name":"a.png"}}]'
  )
  expect_identical(frames[[1L]][["bytes"]], png)
})

test_that("the largest body in a message follows the encoding by default", {

  expect_identical(
    message_opts(),
    list(encoding = "base64", max_body = 2^20, max_size = 4 * 2^20)
  )

  withr::with_options(list(blockr.message_body_encoding = "binary"), {
    expect_identical(
      message_opts(),
      list(encoding = "binary", max_body = 4 * 2^20, max_size = 4 * 2^20)
    )
  })

  # As set, whatever the encoding.
  withr::with_options(
    list(
      blockr.message_body_encoding = "binary",
      blockr.max_message_body = 0,
      blockr.max_message_size = 100
    ),
    expect_identical(
      message_opts(),
      list(encoding = "binary", max_body = 0, max_size = 100)
    )
  )
})

test_that("the transport refuses blockr options it cannot follow", {

  withr::with_options(list(blockr.max_message_body = -1), {
    expect_error(
      with_transport(function(message, send) NULL),
      class = "max_message_body_invalid"
    )
  })

  withr::with_envvar(c(BLOCKR_MAX_MESSAGE_BODY = "big"), {
    expect_error(
      with_transport(function(message, send) NULL),
      class = "max_message_body_invalid"
    )
  })

  withr::with_options(list(blockr.message_body_encoding = "zip"), {
    expect_error(
      with_transport(function(message, send) NULL),
      class = "message_body_encoding_invalid"
    )
  })

  # A message of no bytes at all would hold no reply.
  withr::with_options(list(blockr.max_message_size = 0), {
    expect_error(
      with_transport(function(message, send) NULL),
      class = "max_message_size_invalid"
    )
  })

  withr::with_envvar(c(BLOCKR_MAX_MESSAGE_SIZE = "big"), {
    expect_error(
      with_transport(function(message, send) NULL),
      class = "max_message_size_invalid"
    )
  })
})

test_that("R sends nothing of a message larger than the blockr option says", {

  withr::local_options(blockr.max_message_size = 100)

  text <- function(n) list(id = "1", status = "ok", value = strrep("x", n))

  with_transport(
    function(message, send) NULL,
    {
      before <- length(session$sent)

      # The note goes with the reply, and the two are too large together.
      expect_error(
        send_message(
          list(list(id = "1", progress = list(value = 0.5)), text(60L)),
          session
        ),
        "R's message of 133 bytes is larger than the 100 bytes that the blockr",
        class = "protocol_message_too_large"
      )
      expect_length(session$sent, before)

      # A message of as many bytes as the limit goes.
      send_message(list(text(63L)), session)

      expect_identical(
        nchar(utils::tail(transport_sent(session), 1L), type = "bytes"),
        100L
      )
    }
  )
})

test_that("a body travels in its message where the message stays small", {

  withr::local_options(blockr.max_message_size = 150)

  reply <- function(id, n) {
    list(
      id = id,
      status = "ok",
      body = new_reply_body(as.raw(rep(1L, n)), "image/png")
    )
  }

  sent_body <- function(n) {

    session <- with_transport(
      function(message, send) NULL,
      send_message(list(reply("1", n)), session)
    )

    decode_message(utils::tail(transport_sent(session), 1L))[[1L]][["body"]]
  }

  # Its base64 would make the message larger than the limit.
  expect_named(sent_body(100L), c("type", "url"))
  expect_named(sent_body(30L), c("type", "data"))

  withr::local_options(blockr.message_body_encoding = "binary")

  session <- with_transport(
    function(message, send) NULL,
    send_message(list(reply("1", 120L), reply("2", 30L)), session)
  )

  # The frame would hold the length of its JSON, that JSON and the body.
  expect_named(
    decode_message(utils::tail(transport_sent(session), 1L))[[1L]][["body"]],
    c("type", "url")
  )
  expect_length(transport_frames(session), 1L)
})

test_that("R serves none of the bodies of a message that it refuses", {

  withr::local_options(
    blockr.max_message_size = 200,
    blockr.max_message_body = 0
  )

  with_transport(
    function(message, send) NULL,
    {
      expect_error(
        send_message(
          list(
            list(
              id = "3",
              status = "ok",
              value = strrep("x", 300L),
              body = new_reply_body(as.raw(1:3), "image/png")
            )
          ),
          session
        ),
        class = "protocol_message_too_large"
      )

      expect_identical(
        session$data_objs[["blockr-body-3"]][["filter"]],
        body_gone
      )
    }
  )
})

test_that("a reply goes as it is where its body is no reply body", {

  with_transport(
    function(message, send) NULL,
    {

      send_message(
        list(list(id = "5", status = "ok", body = list(type = "image/png"))),
        session
      )

      expect_identical(
        utils::tail(transport_sent(session), 1L),
        '[{"id":"5","status":"ok","body":{"type":"image/png"}}]'
      )
    }
  )
})

test_that("an output's body travels beside its reply", {

  png <- as.raw(c(0x89, 0x50, 0x4e, 0x47))
  out <- new_plot_output(new_reply_body(png, "image/png"), 480L, 480L)
  parts <- shape_output(out)

  with_transport(
    function(message, send) NULL,
    {

      send_message(list(c(list(id = "9", status = "ok"), parts)), session)

      sent <- decode_message(utils::tail(transport_sent(session), 1L))[[1L]]

      expect_identical(sent[["output"]], list(width = 480L, height = 480L))
      expect_identical(
        sent[["body"]],
        list(type = "image/png", data = base64enc::base64encode(png))
      )
    }
  )
})

test_that("a request's body reaches the handler as a file", {

  got <- list()
  bytes <- charToRaw("a,b\n1,2\n")

  with_transport(
    function(message, send) got[[length(got) + 1L]] <<- message,
    {

      res <- transport_post(session, "25", bytes)
      expect_identical(res[["status"]], 200L)

      transport_post(session, "26", as.raw(1:4))

      session$setInputs(
        blockr = paste0(
          '[{"id": "25", "kind": "request", "name": "upload",',
          ' "body": {"type": "text/csv", "name": "sales.csv"}},',
          ' {"id": "26", "kind": "request", "name": "download",',
          ' "body": {"type": "image/png"}}]'
        )
      )

      bodies <- lapply(got[[1L]], `[[`, "body")

      expect_named(bodies[[1L]], c("name", "size", "type", "datapath"))
      expect_identical(bodies[[1L]][["name"]], "sales.csv")
      expect_identical(bodies[[1L]][["size"]], as.double(length(bytes)))
      expect_identical(bodies[[1L]][["type"]], "text/csv")
      expect_identical(readBin(bodies[[1L]][["datapath"]], "raw", 100L), bytes)

      expect_named(bodies[[2L]], c("size", "type", "datapath"))
      expect_identical(
        readBin(bodies[[2L]][["datapath"]], "raw", 100L),
        as.raw(1:4)
      )
    }
  )

  # The files go with the session, which the test ended.
  datapath <- got[[1L]][[1L]][["body"]][["datapath"]]

  expect_false(file.exists(datapath))
  expect_false(dir.exists(dirname(datapath)))
})

test_that("R replies error to a request whose body never came", {

  got <- list()

  session <- with_transport(
    function(message, send) got[[length(got) + 1L]] <<- message,
    session$setInputs(
      blockr = paste0(
        '[{"id": "27", "kind": "request", "name": "upload",',
        ' "body": {"type": "text/csv"}},',
        ' {"id": "28", "kind": "request", "name": "echo"}]'
      )
    )
  )

  # The handler gets the other request alone.
  expect_identical(
    got,
    list(list(list(id = "28", kind = "request", name = "echo")))
  )

  reply <- decode_message(utils::tail(transport_sent(session), 1L))[[1L]]

  expect_identical(reply[["id"]], "27")
  expect_identical(reply[["status"]], "error")
  expect_named(reply, c("id", "status", "conditions"))

  cnd <- reply[["conditions"]][[1L]]

  expect_named(cnd, c("severity", "class", "message"))
  expect_identical(cnd[["severity"]], "error")
  expect_identical(
    cnd[["class"]],
    c("shiny_transport_request_body_missing", "blockr_error", "rlang_error",
      "error", "condition")
  )
  expect_match(cnd[["message"]], "request 27")
})

test_that("a request whose body is no object goes as it is", {

  got <- list()

  with_transport(
    function(message, send) got[[length(got) + 1L]] <<- message,
    session$setInputs(
      blockr = '[{"id": "29", "kind": "request", "name": "x", "body": "bytes"}]'
    )
  )

  expect_identical(
    got,
    list(list(list(id = "29", kind = "request", name = "x", body = "bytes")))
  )
})

test_that("a later hello drops the bodies posted and those still to fetch", {

  big <- as.raw(rep(0:255, length.out = 2^20 + 1))
  got <- list()

  # A handler that answers a request with a body that R serves from a URL.
  handler <- function(message, send) {

    got[[length(got) + 1L]] <<- message

    if (length(message) && identical(message[[1L]][["kind"]], "request")) {

      req <- message[[1L]]

      send(
        list(
          list(
            id = req[["id"]],
            status = "ok",
            body = new_reply_body(big, "application/octet-stream")
          )
        )
      )
    }
  }

  hello <- function(id) {
    paste0(
      '[{"id": "', id, '", "kind": "hello", "protocol": "1.0",',
      ' "capabilities": []}]'
    )
  }

  upload <- function(id) {
    paste0(
      '[{"id": "', id, '", "kind": "request", "name": "upload",',
      ' "body": {"type": "image/png"}}]'
    )
  }

  with_transport(
    handler,
    {

      session$setInputs(blockr = hello("1"))

      transport_post(session, "2", as.raw(1:4))
      session$setInputs(blockr = upload("2"))

      datapath <- got[[2L]][[1L]][["body"]][["datapath"]]

      expect_true(file.exists(datapath))
      expect_identical(
        transport_http(session, "blockr-body-2")[["status"]],
        200L
      )

      # A body posted for a request that the client sends after the hello.
      transport_post(session, "3", as.raw(5:8))

      session$setInputs(blockr = hello("4"))

      # The handler hears the hello once the bodies are gone.
      expect_identical(got[[3L]][[1L]][["kind"]], "hello")
      expect_false(file.exists(datapath))
      expect_length(
        list.files(dirname(datapath), all.files = TRUE, no.. = TRUE),
        0L
      )

      res <- transport_http(session, "blockr-body-2")
      expect_identical(res[["status"]], 404L)
      expect_identical(
        session$data_objs[["blockr-body-2"]][["filter"]],
        body_gone
      )

      session$setInputs(blockr = upload("3"))

      reply <- decode_message(utils::tail(transport_sent(session), 1L))[[1L]]

      expect_identical(reply[["id"]], "3")
      expect_identical(reply[["status"]], "error")

      # The handler gets the message without the request.
      expect_identical(got[[4L]], list())
    }
  )
})

test_that("a body that the client deleted is none that a hello drops", {

  served <- new.env(parent = emptyenv())

  with_transport(
    function(message, send) NULL,
    {

      send_message(
        list(
          list(
            id = "5",
            status = "ok",
            body = new_reply_body(
              as.raw(rep(1L, 2^20 + 1)),
              "application/octet-stream"
            )
          )
        ),
        session,
        served = served
      )

      expect_identical(ls(served), "blockr-body-5")

      transport_http(session, "blockr-body-5", "DELETE")

      expect_identical(ls(served), character())
    }
  )
})

test_that("the transport takes posted bodies that name their request alone", {

  with_transport(
    function(message, send) NULL,
    {

      res <- transport_http(session, "blockr-bodies", "GET", "?id=1")
      expect_identical(res[["status"]], 405L)

      res <- transport_http(session, "blockr-bodies", "POST", "?w=", raw(1L))
      expect_identical(res[["status"]], 400L)
    }
  )
})

test_that("the transport ignores what it cannot decode and outlives errors", {

  withr::local_options(
    blockr.logger = cnd_logger,
    blockr.log_level = "warn",
    width = 500L
  )

  got <- 0L

  handler <- function(message, send) {

    got <<- got + 1L

    if (identical(message[[1L]][["id"]], "fails")) {
      stop("the evaluator failed")
    }
  }

  with_transport(
    handler,
    {

      expect_warning(
        session$setInputs(blockr = "not JSON"),
        "ignores a message that is no array"
      )

      expect_warning(
        session$setInputs(blockr = '{"id": "1", "kind": "hello"}'),
        "ignores a message that is no array"
      )

      expect_warning(
        session$setInputs(blockr = '[{"id": "fails", "kind": "hello"}]'),
        "handler failed: the evaluator failed"
      )

      session$setInputs(blockr = '[{"id": "2", "kind": "hello"}]')
    }
  )

  expect_identical(got, 2L)
})
