test_that("the bridge starts a session that the page may start anew", {

  session <- with_bridge(function(message, send) NULL)

  expect_true(session$reconnect)

  expect_identical(
    bridge_sent(session),
    paste0('{"upload":"session/mock/dataobj/blockr-bodies?w=&nonce=0"}')
  )
  expect_s3_class(session$sent[[1L]][["message"]], "json")
  expect_identical(session$sent[[1L]][["type"]], "blockr")
})

test_that("the bridge hands each message the client sends to the handler", {

  got <- list()

  with_bridge(
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
          bridge_post(session, decode_message(client)[[1L]][["id"]], raw(1L))
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

test_that("the bridge ignores the value that the input starts with", {

  got <- list()

  session <- bridge_session()

  # Shiny replays the page's last message when it starts a new session.
  session$setInputs(blockr = '[{"kind": "forget", "blocks": ["a"]}]')

  with_bridge(
    function(message, send) got[[length(got) + 1L]] <<- message,
    session$setInputs(blockr = '[{"id": "1", "kind": "hello"}]'),
    session
  )

  expect_identical(got, list(list(list(id = "1", kind = "hello"))))
})

test_that("the bridge sends R's messages as encode_message() writes them", {

  values <- protocol_r_values()

  with_bridge(
    function(message, send) NULL,
    {

      for (name in protocol_fixtures()) {

        expected <- read_json_strictly(protocol_fixture(name))[["r"]]

        for (i in seq_along(expected)) {

          send_message(values[[name]][[i]], session)

          expect_identical(
            read_json_strictly(utils::tail(bridge_sent(session), 1L)),
            expected[[i]],
            label = paste0("message ", i, " the bridge sends for ", name)
          )
        }
      }
    }
  )
})

test_that("the handler's send() sends a message", {

  session <- with_bridge(
    function(message, send) {
      send(list(list(id = message[[1L]][["id"]], status = "ok", value = 1L)))
    },
    session$setInputs(blockr = '[{"id": "7", "kind": "open"}]')
  )

  expect_identical(
    utils::tail(bridge_sent(session), 1L),
    '[{"id":"7","status":"ok","value":1}]'
  )
})

test_that("the bridge refuses to send what is no unnamed list", {

  with_bridge(
    function(message, send) NULL,
    {
      expect_error(
        send_message(list(id = "1", status = "ok"), session),
        class = "blockr_message_invalid"
      )
      expect_error(
        send_message("[]", session),
        class = "blockr_message_invalid"
      )
    }
  )
})

test_that("a small body travels in its reply's message", {

  file <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b", file)

  with_bridge(
    function(message, send) NULL,
    {

      send_message(
        list(
          list(
            id = "1",
            status = "ok",
            body = list(x = as.raw(1:3), type = "image/png")
          ),
          list(
            id = "2",
            status = "ok",
            body = list(x = file, type = "text/csv", name = "a.csv")
          )
        ),
        session
      )

      sent <- decode_message(utils::tail(bridge_sent(session), 1L))

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

test_that("a large body is served once from a URL", {

  big <- as.raw(rep(0:255, length.out = inline_body_max() + 1))

  file <- withr::local_tempfile(fileext = ".bin")
  writeBin(big, file)

  with_bridge(
    function(message, send) NULL,
    {

      send_message(
        list(
          list(
            id = "3",
            status = "ok",
            body = list(x = big, type = "application/octet-stream")
          ),
          list(
            id = "4",
            status = "ok",
            body = list(x = file, type = "text/plain", name = "big.txt")
          )
        ),
        session
      )

      sent <- decode_message(utils::tail(bridge_sent(session), 1L))

      expect_identical(
        sent[[1L]][["body"]],
        list(
          type = "application/octet-stream",
          url = "session/mock/dataobj/blockr-body-3?w=&nonce=1"
        )
      )

      res <- bridge_http(session, "blockr-body-3")

      expect_identical(res[["status"]], 200L)
      expect_identical(res[["content_type"]], "application/octet-stream")
      expect_identical(res[["content"]], big)

      # Served once, the bytes are dropped.
      expect_identical(bridge_http(session, "blockr-body-3")[["status"]], 404L)

      expect_identical(
        sent[[2L]][["body"]][c("type", "name")],
        list(type = "text/plain", name = "big.txt")
      )

      res <- bridge_http(session, "blockr-body-4")

      expect_identical(res[["content"]], list(file = file, owned = FALSE))
    }
  )

  # The file of a body given as one stays.
  expect_true(file.exists(file))
})

test_that("a reply goes as it is where its body holds no bytes", {

  with_bridge(
    function(message, send) NULL,
    {

      send_message(
        list(list(id = "5", status = "ok", body = list(type = "image/png"))),
        session
      )

      expect_identical(
        utils::tail(bridge_sent(session), 1L),
        '[{"id":"5","status":"ok","body":{"type":"image/png"}}]'
      )
    }
  )
})

test_that("a reply's body holds bytes that R can send", {

  reply <- function(body) list(list(id = "6", status = "ok", body = body))

  with_bridge(
    function(message, send) NULL,
    {
      expect_error(
        send_message(reply(list(x = 1:3, type = "image/png")), session),
        class = "blockr_body_invalid"
      )
      expect_error(
        send_message(reply(list(x = tempfile(), type = "image/png")), session),
        class = "blockr_body_invalid"
      )
      expect_error(
        send_message(reply(list(x = as.raw(1L))), session),
        class = "blockr_body_invalid"
      )
    }
  )
})

test_that("a request's body reaches the handler as a file", {

  got <- list()
  bytes <- charToRaw("a,b\n1,2\n")

  with_bridge(
    function(message, send) got[[length(got) + 1L]] <<- message,
    {

      res <- bridge_post(session, "25", bytes)
      expect_identical(res[["status"]], 200L)

      bridge_post(session, "26", as.raw(1:4))

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

  session <- with_bridge(
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

  reply <- decode_message(utils::tail(bridge_sent(session), 1L))[[1L]]

  expect_identical(reply[["id"]], "27")
  expect_identical(reply[["status"]], "error")
  expect_identical(reply[["omitted"]], list(warning = 0L, message = 0L))

  cnd <- reply[["conditions"]][[1L]]

  expect_named(cnd, c("severity", "class", "message"))
  expect_identical(cnd[["severity"]], "error")
  expect_true("blockr_no_body" %in% cnd[["class"]])
  expect_match(cnd[["message"]], "request 27")
})

test_that("a request whose body is no object goes as it is", {

  got <- list()

  with_bridge(
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

test_that("the bridge takes posted bodies that name their request alone", {

  with_bridge(
    function(message, send) NULL,
    {

      res <- bridge_http(session, "blockr-bodies", "GET", "?id=1")
      expect_identical(res[["status"]], 405L)

      res <- bridge_http(session, "blockr-bodies", "POST", "?w=", raw(1L))
      expect_identical(res[["status"]], 400L)
    }
  )
})

test_that("the bridge ignores what it cannot decode and outlives its handler", {

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

  with_bridge(
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
