test_that("R answers the handshake as the fixture hello-declared has it", {

  dir <- withr::local_tempdir()
  write_declared_files(dir)

  local_empty_declarations()
  local_declared_package(dir)
  register_script_blocks(dir)
  local_fixture_renderer_urls()

  module <- function(path) js_module(path, package = "blockr.test")

  withr::local_options(
    blockr.renderers = list(
      table_output = new_renderer("table", module("js/table.js")),
      dark_mode = new_renderer(
        "dark",
        module("js/dark.js"),
        option = "dark_mode"
      )
    )
  )

  session <- local_evaluator(proto_declared_board(), hello = FALSE)

  expect_fixture_replies(session, "hello-declared")
})

test_that("R sends the JSON board that serve() was given as it is", {

  file <- structure("{\"format\":1,\"board\":{}}", class = "json")
  session <- local_evaluator(board = file, hello = FALSE)

  session$receive(hello_message())
  session$run()

  reply <- last_reply(session)

  expect_identical(reply$board, file)
  expect_false("file" %in% names(reply))
  expect_null(reply$bound)
  expect_null(reply$lock)
  expect_true("lock" %in% names(reply))
})

test_that("R names the inputs it binds itself for a block served alone", {

  session <- local_evaluator(data = list(data = datasets::iris), hello = FALSE)

  session$receive(hello_message())
  session$run()

  expect_identical(last_reply(session)$bound, I("data"))

  session$sent <- list()

  session$receive(
    list(
      evaluate_request(
        "2",
        "b",
        1L,
        list(
          registry_id = "blockr.core-head_block",
          state = list(n = 6L, direction = "head"),
          inputs = list(list(input = "data"))
        )
      ),
      list(
        id = "3",
        kind = "request",
        type = "block_class",
        registry_id = "blockr.core-head_block",
        block = "b",
        name = "n_max",
        args = obj(),
        reads = list(list(input = "data"))
      )
    )
  )

  session$run()

  expect_identical(session$sent[[1L]][[1L]]$summary, "<data.frame> [6 × 5]")
  expect_identical(unclass(session$sent[[2L]][[1L]]$value), "[150]")
})

test_that("an evaluator sends one opening board and binds named data", {

  expect_error(new_evaluator(), class = "evaluator_board_invalid")
  expect_error(new_evaluator(NULL), class = "evaluator_board_invalid")

  expect_error(
    new_evaluator(board = obj(), data = list(datasets::iris)),
    class = "evaluator_data_invalid"
  )
})

test_that("R names the capabilities cancel and progress, not interrupt", {

  session <- local_evaluator(hello = FALSE)

  session$receive(hello_message(capabilities = list()))
  session$run()

  reply <- last_reply(session)

  expect_identical(reply$capabilities, I(c("cancel", "progress")))
  expect_identical(reply$protocol, protocol_version())
})

test_that("R ignores every message before the session's hello", {

  session <- local_evaluator(hello = FALSE)

  session$receive(
    list(
      evaluate_request("11", "a", 2L, sample_recipe("a")),
      list(kind = "forget", blocks = "a"),
      list(id = "12", kind = "nonsense")
    )
  )

  session$run()

  expect_length(session$sent, 0L)

  # A hello without id is one that R cannot answer, which starts nothing.
  session$receive(list(list(kind = "hello", protocol = "1.0",
                            capabilities = list())))
  session$run()

  session$receive(list(evaluate_request("13", "a", 2L, sample_recipe("a"))))
  session$run()

  expect_length(session$sent, 0L)
})

test_that("a later hello starts the session afresh, forgetting every block", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  session$receive(
    list(
      evaluate_request(
        "12", "b", 3L, sample_recipe("b", list(read_of("a", 2L, "data")))
      )
    )
  )

  session$receive(hello_message("13"))
  session$run()

  expect_length(session$sent, 2L)
  expect_identical(
    session$sent[[1L]],
    list(list(id = "12", status = "cancelled"))
  )
  expect_identical(session$sent[[2L]][[1L]]$id, "13")
  expect_identical(session$sent[[2L]][[1L]]$status, "ok")

  session$sent <- list()

  session$receive(list(output_request("14", "a", 2L)))
  session$run()

  expect_identical(
    last_reply(session),
    list(id = "14", status = "missing",
         missing = list(list(block = "a", revision = 2L)))
  )
})

test_that("R evaluates a block from its recipe, as the fixture has it", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))

  expect_fixture_replies(session, "evaluate")
})

test_that("R reports the conditions an evaluation raises with their phases", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  expect_fixture_replies(session, "evaluate-warning")
  expect_fixture_replies(session, "evaluate-error")
})

test_that("R binds a variadic block's inputs in link order", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))
  hold_result(session, "d", 6L, list(read_of("a", 2L, "data")))

  expect_fixture_replies(session, "evaluate-variadic")
})

test_that("R replies invalid where the state leaves a used arg unset", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))

  expect_fixture_replies(session, "evaluate-invalid")
})

test_that("R passes the parameters of a one-time evaluation to block_eval()", {

  local_write_block()

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))
  hold_result(
    session, "c", 4L,
    list(read_of("a", 2L, "x"), read_of("b", 3L, "y"))
  )

  expect_fixture_replies(session, "evaluate-params")

  req <- decode_message(protocol_fixture("evaluate-params"))$client
  req[[1L]]$params <- NULL
  req[[1L]]$id <- "31"

  session$receive(req)
  session$run()

  expect_identical(last_reply(session)$status, "invalid")
})

test_that("R replies missing to a revision it does not hold", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 5L, list(read_of("a", 2L, "data")))

  expect_fixture_replies(session, "evaluate-missing")
})

test_that("R runs several requests in one message in the order they arrive", {

  local_fixture_renderer_urls()

  session <- local_evaluator()

  expect_fixture_replies(session, "several-requests")
})

test_that("an evaluation that fails leaves R no result of its block", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "d", 6L, list(read_of("a", 2L, "data")))

  failing <- sample_recipe("d", list(read_of("a", 2L, "data")))
  failing$state$subset <- "Sepal.Widht > 4"

  session$receive(
    list(
      evaluate_request("20", "d", 8L, failing),
      output_request("21", "d", 6L)
    )
  )

  session$run()

  expect_identical(session$sent[[1L]][[1L]]$status, "error")
  expect_identical(session$sent[[2L]][[1L]]$status, "missing")
})

test_that("R replies error where it cannot evaluate a recipe", {

  session <- local_evaluator()

  session$receive(
    list(
      evaluate_request(
        "2", "z", 1L,
        list(registry_id = "nopkg-no_block", state = obj(), inputs = list())
      )
    )
  )

  session$run()

  reply <- last_reply(session)

  expect_identical(reply$status, "error")
  expect_identical(reply$block, "z")
  expect_identical(reply$revision, 1L)
  expect_null(reply$summary)
  expect_true("summary" %in% names(reply))
  expect_identical(reply$stdout, "")
  expect_null(reply$conditions[[1L]]$phase)
  expect_identical(
    condition_class(reply),
    "registry_id_unknown"
  )
})

test_that("R captures what an evaluation writes, bounded", {

  session <- local_evaluator()

  loud <- list(
    registry_id = "blockr.core-static_block",
    state = list(
      expression = paste(
        "{ cat('out\\n'); cat('err\\n', file = stderr());",
        "for (i in 1:7) warning('w', i); message('m'); 1 }"
      )
    ),
    inputs = list()
  )

  local_block_class(static_block, "blockr.core-static_block")

  sinks <- sink.number()
  messages <- sink.number(type = "message")

  session$receive(list(evaluate_request("2", "s", 1L, loud)))
  session$run()

  expect_identical(sink.number(), sinks)
  expect_identical(sink.number(type = "message"), messages)

  reply <- last_reply(session)

  expect_identical(reply$status, "ok")
  expect_identical(reply$stdout, "out")
  expect_identical(reply$stderr, "err")

  severity <- chr_xtr(reply$conditions, "severity")

  expect_identical(sum(severity == "warning"), max_conditions())
  expect_identical(sum(severity == "message"), 1L)
  expect_identical(reply$omitted, list(warning = 2L, message = 0L))
  expect_identical(reply$conditions[[1L]]$message, "w1")

  long <- strrep("x", max_output_chars() + 10L)

  expect_identical(
    bounded_text(long, max_output_chars()),
    paste0(strrep("x", max_output_chars()), "\n[ omitted 10 characters ]")
  )

  expect_identical(bounded_text("\xff", 10L), "?")
})

test_that("R makes a block's output, as the fixtures of outputs have it", {

  local_fixture_renderer_urls()

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))
  hold_result(
    session, "c", 4L,
    list(read_of("a", 2L, "x"), read_of("b", 3L, "y"))
  )
  hold_result(session, "f", 5L, list(read_of("a", 2L, "data")))

  expect_fixture_replies(session, "output")
  expect_fixture_replies(session, "output-plot")
})

test_that("an output that fails replies error with the phase output", {

  cls <- new_block_class(
    "list_block",
    expr = function() quote(list(1, 2)),
    ui = js_module("list-block.js", package = NULL),
    output = as_table_output
  )

  local_block_class(cls, "list_block")

  session <- local_evaluator()

  hold_result(
    session, "k", 14L,
    recipe = list(registry_id = "list_block", state = obj(), inputs = list())
  )

  expect_fixture_replies(session, "output-error")
})

test_that("R shows a result as text where its class names no output", {

  session <- local_evaluator()

  hold_result(
    session, "u", 1L,
    recipe = list(
      registry_id = "blockr.core-upload_block",
      state = list(file_path = "sales.csv"),
      inputs = list()
    )
  )

  session$receive(list(output_request("2", "u", 1L)))
  session$run()

  reply <- last_reply(session)

  expect_identical(reply$status, "ok")
  expect_identical(reply$output$renderer$id, "blockr.core-text_output")
  expect_identical(reply$output$text, "[1] \"sales.csv\"")
})

test_that("R answers a block class's request, as the fixtures have it", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  expect_fixture_replies(session, "request")

  withr::local_options(useFancyQuotes = TRUE)
  skip_if_not(l10n_info()[["UTF-8"]])

  expect_fixture_replies(session, "request-error")
})

test_that("an action receives the request's body in file", {

  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  testthat::local_mocked_bindings(
    upload_folder = function() "uploads/3f9a2c"
  )

  session <- local_evaluator()

  expect_fixture_replies(
    session,
    "request-action",
    function(msg) {
      msg[[1L]] <- received_body(
        msg[[1L]],
        charToRaw("a,b\n1,2\n"),
        file.path(dir, "body")
      )
      msg
    }
  )

  expect_identical(
    readLines(file.path(dir, "uploads", "3f9a2c", "sales.csv")),
    c("a,b", "1,2")
  )
})

test_that("R answers a board option's request by the board option's class", {

  local_request_types()

  session <- local_evaluator()

  expect_fixture_replies(session, "request-option")
})

test_that("R answers a request of a type that a package adds", {

  local_request_types()

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))

  expect_fixture_replies(session, "request-added")
})

test_that("a request's function sends progress notes while it runs", {

  local_request_types()

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))
  hold_result(
    session, "c", 4L,
    list(read_of("a", 2L, "x"), read_of("b", 3L, "y"))
  )

  expect_fixture_replies(session, "request-progress")

  # Without the capability progress, R sends no notes.
  quiet <- local_evaluator(hello = FALSE)

  quiet$receive(hello_message(capabilities = "cancel"))
  quiet$run()

  hold_result(quiet, "a", 2L)
  hold_result(quiet, "b", 3L, list(read_of("a", 2L, "data")))
  hold_result(
    quiet, "c", 4L,
    list(read_of("a", 2L, "x"), read_of("b", 3L, "y"))
  )

  quiet$sent <- list()
  quiet$receive(decode_message(protocol_fixture("request-progress"))$client)
  quiet$run()

  expect_length(quiet$sent, 1L)
  expect_identical(last_reply(quiet)$status, "ok")

  # Outside a request, R sends nothing.
  expect_false(report_progress(0.5))

  expect_error(report_progress(2), class = "request_progress_invalid")
  expect_error(
    report_progress(message = 1),
    class = "request_progress_invalid"
  )
})

test_that("R answers the requests of an output class and of a renderer", {

  local_chart_output()
  local_labelled_block()
  local_table_preview()

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(
    session, "h", 9L,
    recipe = list(
      registry_id = "chart_block",
      state = obj(),
      inputs = list(read_of("a", 2L, "data"))
    )
  )

  make_output(session, "h", 9L)

  dir <- withr::local_tempdir()

  expect_fixture_replies(
    session,
    "request-output",
    function(msg) {
      msg[[1L]] <- received_body(
        msg[[1L]],
        as.raw(1:4),
        file.path(dir, "body")
      )
      msg
    }
  )

  expect_fixture_replies(session, "request-output-volatile")

  hold_result(
    session, "a", 3L,
    recipe = list(registry_id = "adsl_block", state = obj(), inputs = list())
  )

  make_output(session, "a", 3L, list(n_rows = 50L))

  expect_fixture_replies(session, "request-renderer")
})

test_that("R resolves an output's request by an output it made", {

  local_chart_output()

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(
    session, "h", 9L,
    recipe = list(
      registry_id = "chart_block",
      state = obj(),
      inputs = list(read_of("a", 2L, "data"))
    )
  )

  req <- decode_message(protocol_fixture("request-output-volatile"))$client

  session$receive(req)
  session$run()

  reply <- last_reply(session)

  expect_identical(reply$status, "error")
  expect_identical(
    condition_class(reply),
    "output_request_unresolved"
  )
  expect_null(reply$conditions[[1L]]$phase)
})

test_that("a request names its failures by phase, and lacking results", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  request <- function(id, name, args = obj(), reads = list()) {
    list(
      id = id,
      kind = "request",
      type = "block_class",
      registry_id = "blockr.core-head_block",
      block = "b",
      name = name,
      args = args,
      reads = reads
    )
  }

  session$receive(
    list(
      request("2", "nonsense", reads = list(read_of("a", 2L, "data"))),
      request("3", "n_max", reads = list(read_of("a", 7L, "data"))),
      request("4", "n_max", args = list(n = 1L),
              reads = list(read_of("a", 2L, "data")))
    )
  )

  session$run()

  unknown <- session$sent[[1L]][[1L]]

  expect_identical(unknown$status, "error")
  expect_null(unknown$conditions[[1L]]$phase)
  expect_identical(condition_class(unknown), "block_request_unknown")

  expect_identical(
    session$sent[[2L]][[1L]],
    list(id = "3", status = "missing",
         missing = list(list(block = "a", revision = 7L)))
  )

  expect_identical(
    condition_class(session$sent[[3L]][[1L]]),
    "block_request_args_invalid"
  )
})

test_that("R runs one request at a time, between them handling messages", {

  session <- local_evaluator()

  session$receive(
    list(
      evaluate_request("11", "a", 2L, sample_recipe("a")),
      evaluate_request(
        "12", "b", 3L, sample_recipe("b", list(read_of("a", 2L, "data")))
      )
    )
  )

  # R runs the evaluation of a,
  # and returns to its event loop before it starts the next.
  later::run_now(all = FALSE)

  session$receive(list(list(kind = "cancel", request = "12")))
  session$run()

  expect_identical(
    session$sent[[1L]],
    list(list(id = "12", status = "cancelled"))
  )
  expect_identical(session$sent[[2L]][[1L]]$id, "11")
  expect_length(session$sent, 2L)
})

test_that("a cancel drops a request that has not started", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))

  session$receive(
    list(
      evaluate_request(
        "17", "c", 4L,
        sample_recipe("c", list(read_of("a", 2L, "x"), read_of("b", 3L, "y")))
      )
    )
  )

  expect_fixture_replies(session, "cancel")

  # Where the client names no capability cancel, R ignores a cancel.
  quiet <- local_evaluator(hello = FALSE)

  quiet$receive(hello_message(capabilities = "progress"))
  quiet$receive(list(evaluate_request("2", "a", 2L, sample_recipe("a"))))
  quiet$receive(list(list(kind = "cancel", request = "2")))
  quiet$run()

  expect_identical(last_reply(quiet)$status, "ok")
})

test_that("a forget drops results and the queued requests that read them", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))
  hold_result(session, "d", 6L, list(read_of("a", 2L, "data")))
  hold_result(
    session, "e", 7L,
    list(read_of("b", 3L, ""), read_of("d", 6L, ""))
  )

  session$receive(list(output_request("34", "e", 7L)))

  expect_fixture_replies(session, "forget")

  session$sent <- list()

  session$receive(
    list(
      output_request("35", "d", 6L),
      output_request("36", "b", 3L, list(n_rows = 50L))
    )
  )
  session$run()

  expect_identical(session$sent[[1L]][[1L]]$status, "missing")
  expect_identical(session$sent[[2L]][[1L]]$status, "ok")
})

test_that("R reads no open, since the client opens every board itself", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  session$receive(list(list(id = "2", kind = "open")))
  session$run()

  reply <- last_reply(session)

  expect_identical(reply$status, "error")
  expect_identical(condition_class(reply), "protocol_request_invalid")
  expect_true(exists("a", envir = session$evaluator$results))
})

test_that("R reads blockr options, as the fixture blockr-options has it", {

  withr::local_options(
    blockr.evaluation_queue = 3L,
    blockr.background_evaluation_delay = NULL,
    blockr.gate_visibility = NULL,
    blockr.rand_id = "uuid"
  )

  withr::local_envvar(
    BLOCKR_BACKGROUND_EVALUATION_DELAY = "30",
    BLOCKR_GATE_VISIBILITY = NA,
    BLOCKR_EVALUATION_QUEUE = NA,
    BLOCKR_RAND_ID = "letters_8"
  )

  session <- local_evaluator()

  expect_fixture_replies(session, "blockr-options")

  session$receive(list(list(id = "4", kind = "blockr_options", names = list())))
  session$run()

  expect_identical(
    last_reply(session),
    list(id = "4", status = "ok", values = set_names(list(), character()))
  )
})

test_that("R leaves out a blockr option it cannot read or write, and warns", {

  local_table_preview()

  withr::local_options(
    blockr.evaluation_queue = 3L,
    blockr.gate_visibility = FALSE,
    blockr.release = as.Date("2026-11-09")
  )

  withr::local_envvar(BLOCKR_EVALUATION_QUEUE = "4")

  session <- local_evaluator()

  withr::local_options(
    blockr.logger = cnd_logger,
    blockr.log_level = "warn",
    width = 500L
  )

  session$receive(
    list(
      list(
        id = "2",
        kind = "blockr_options",
        names = c("evaluation_queue", "renderers", "release", "gate_visibility")
      )
    )
  )

  warnings <- testthat::capture_warnings(session$run())

  expect_length(warnings, 2L)

  expect_match(
    warnings[1L],
    paste(
      "R leaves blockr option evaluation_queue out of its reply, since it",
      "cannot read it: Conflicting options set for evaluation_queue"
    ),
    fixed = TRUE
  )

  expect_match(
    warnings[2L],
    paste(
      "R leaves blockr option renderers out of its reply, since it cannot",
      "write it as JSON:"
    ),
    fixed = TRUE
  )

  expect_identical(
    sent_json(session),
    list(
      list(
        list(
          id = "2",
          status = "ok",
          values = list(release = "2026-11-09", gate_visibility = FALSE)
        )
      )
    )
  )
})

test_that("R replies error to a request it cannot read", {

  session <- local_evaluator()

  session$receive(
    list(
      list(id = "2", kind = "nonsense"),
      list(id = "3", kind = "evaluate", block = "a"),
      list(id = "4", kind = "request", name = "x", option = "a",
           output = obj()),
      list(id = "5", kind = "output", block = "a", revision = 1L, info = "x"),
      list(kind = "evaluate", block = "a", revision = 1L),
      "nonsense",
      list(kind = "forget"),
      list(kind = "cancel")
    )
  )

  session$run()

  ids <- chr_ply(session$sent, function(x) x[[1L]]$id)

  expect_identical(ids, c("2", "3", "4", "5"))

  for (msg in session$sent) {
    expect_identical(msg[[1L]]$status, "error")
    expect_identical(
      condition_class(msg[[1L]]),
      "protocol_request_invalid"
    )
  }
})

test_that("R replies error where answering a request fails in R", {

  session <- local_evaluator()

  testthat::local_mocked_bindings(
    answer_request = function(ev, job) stop("R failed.")
  )

  withr::local_options(
    blockr.logger = cnd_logger,
    blockr.log_level = "error",
    width = 500L
  )

  session$receive(list(list(id = "2", kind = "blockr_options", names = "a")))

  expect_warning(session$run(), "R failed to answer request 2: R failed.")

  reply <- last_reply(session)

  expect_identical(reply$status, "error")
  expect_identical(reply$conditions[[1L]]$message, "R failed.")
  expect_null(reply$conditions[[1L]]$phase)
})

test_that("R replies error where it cannot send a reply", {

  sent <- list()
  failed <- FALSE

  send <- function(message) {
    if (!failed) {
      failed <<- TRUE
      stop("Cannot write the reply.")
    }
    sent[[length(sent) + 1L]] <<- message
  }

  withr::local_options(
    blockr.logger = cnd_logger,
    blockr.log_level = "error",
    width = 500L
  )

  ev <- new_evaluator(board = obj())
  receive_requests(ev, hello_message(), function(message) NULL)
  receive_requests(
    ev,
    list(list(id = "2", kind = "blockr_options", names = "a")),
    send
  )

  expect_warning(run_later(), "R failed to send its reply to 2")

  expect_length(sent, 1L)
  expect_identical(sent[[1L]][[1L]]$status, "error")
  expect_identical(
    sent[[1L]][[1L]]$conditions[[1L]]$message,
    "Cannot write the reply."
  )
})

test_that("the Shiny transport carries the evaluator's replies and bodies", {

  withr::local_options(blockr.log_level = "error")

  ev <- new_evaluator(board = list(blocks = list()))

  session <- with_transport(
    function(message, send) receive_requests(ev, message, send),
    {
      hello <- paste0(
        "[{\"id\": \"1\", \"kind\": \"hello\", \"protocol\": \"1.0\", ",
        "\"capabilities\": [\"cancel\", \"progress\"]}]"
      )

      evaluate <- paste0(
        "[{\"id\": \"2\", \"kind\": \"evaluate\", \"block\": \"a\", ",
        "\"revision\": 1, \"recipe\": {\"registry_id\": ",
        "\"blockr.core-dataset_block\", \"state\": {\"dataset\": \"iris\", ",
        "\"package\": \"datasets\"}, \"inputs\": []}}, ",
        "{\"id\": \"3\", \"kind\": \"evaluate\", \"block\": \"f\", ",
        "\"revision\": 1, \"recipe\": {\"registry_id\": ",
        "\"blockr.core-scatter_block\", \"state\": {\"x\": \"Sepal.Length\", ",
        "\"y\": \"Sepal.Width\"}, \"inputs\": [{\"input\": \"data\", ",
        "\"block\": \"a\", \"revision\": 1}]}}, ",
        "{\"id\": \"4\", \"kind\": \"output\", \"block\": \"f\", ",
        "\"revision\": 1, \"options\": {}, \"info\": {\"width\": 640, ",
        "\"height\": 400, \"pixel_ratio\": 1, \"bg\": \"#ffffff\", ",
        "\"fg\": \"#000000\", \"accent\": \"#0000ff\", \"font\": \"\"}, ",
        "\"params\": {}}]"
      )

      session$setInputs(blockr = hello)
      run_later()

      session$setInputs(blockr = evaluate)
      run_later()
    }
  )

  sent <- lapply(transport_sent(session), read_json_strictly)

  # The message that starts the session, the reply to hello,
  # and one message for each reply.
  expect_length(sent, 5L)
  expect_identical(sent[[2L]][[1L]]$id, "1")
  expect_identical(sent[[3L]][[1L]]$status, "ok")
  expect_identical(sent[[4L]][[1L]]$summary, "<captured_plot>")

  plot <- sent[[5L]][[1L]]

  expect_identical(plot$status, "ok")
  expect_identical(plot$output$renderer$id, "blockr.core-plot_output")
  expect_identical(plot$body$type, "image/png")
  expect_true(nzchar(plot$body$data))
})

test_that("R answers a type that a package adds with the fields it gives", {

  got <- NULL

  testthat::local_mocked_s3_method(
    "request_call",
    "notes_request",
    function(x, reads, ...) {
      got <<- list(x = x, reads = reads)
      list(fun = function(text) toupper(text), args = list(text = x$text))
    }
  )

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  session$receive(
    list(
      list(
        id = "2",
        kind = "request",
        type = "notes",
        text = "hello",
        reads = list(read_of("a", 2L))
      )
    )
  )

  session$run()

  expect_identical(unclass(last_reply(session)$value), "[\"HELLO\"]")
  expect_s3_class(got$x, c("notes_request", "request"), exact = TRUE)
  expect_identical(got$x$text, "hello")
  expect_identical(names(got$reads), "a")
  expect_identical(got$reads$a$expr, quote(datasets::iris))
  expect_identical(nrow(got$reads$a$result), 150L)
})

test_that("R checks what a request of any type reads, and forgets it", {

  local_request_types()

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  code <- function(id, revision) {
    list(
      id = id,
      kind = "request",
      type = "code",
      name = "script",
      reads = list(read_of("a", revision))
    )
  }

  session$receive(list(code("2", 7L)))
  session$run()

  expect_identical(
    last_reply(session),
    list(id = "2", status = "missing",
         missing = list(list(block = "a", revision = 7L)))
  )

  session$sent <- list()

  session$receive(list(code("3", 2L)))
  session$receive(list(list(kind = "forget", blocks = "a")))
  session$run()

  expect_identical(
    session$sent,
    list(list(list(id = "3", status = "cancelled")))
  )
})

test_that("R replies error to a type that no method answers", {

  session <- local_evaluator()

  session$receive(
    list(
      list(id = "2", kind = "request", type = "nothing", name = "x"),
      list(id = "3", kind = "request", type = "option", option = "data_dir",
           name = "list_dir", args = obj()),
      list(id = "4", kind = "request", type = "no such type", name = "x"),
      list(id = "5", kind = "request", name = "x", registry_id = "a")
    )
  )

  session$run()

  replies <- lapply(session$sent, `[[`, 1L)

  expect_identical(chr_xtr(replies, "id"), c("4", "5", "2", "3"))
  expect_identical(
    chr_ply(replies, condition_class),
    c("protocol_request_invalid", "protocol_request_invalid",
      "request_type_unknown", "option_request_unknown")
  )
  expect_null(replies[[3L]]$conditions[[1L]]$phase)
})

test_that("an evaluator is an object of the class session_evaluator", {

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  ev <- session$evaluator

  expect_s3_class(ev, c("session_evaluator", "evaluator"), exact = TRUE)
  expect_identical(evaluator_capabilities(ev), c("cancel", "progress"))

  expect_identical(
    format(ev),
    c("<session_evaluator>", "Session: started", "Results: 1",
      "Queued requests: 0")
  )

  expect_output(print(ev), "<session_evaluator>")

  drop_results(ev, "a")

  expect_identical(format(ev)[3L], "Results: 0")
  expect_null(stop_request(ev, list(id = "1")))
})

test_that("an evaluator that names interrupt stops a request that started", {

  stopped <- list()

  testthat::local_mocked_s3_method(
    "evaluator_capabilities",
    "test_evaluator",
    function(x) c("cancel", "interrupt", "progress")
  )

  testthat::local_mocked_s3_method(
    "stop_request",
    "test_evaluator",
    function(x, job) stopped[[length(stopped) + 1L]] <<- job[["id"]]
  )

  session <- local_evaluator(hello = FALSE)

  class(session$evaluator) <- c("test_evaluator", class(session$evaluator))

  session$receive(hello_message())
  session$run()

  expect_identical(
    last_reply(session)$capabilities,
    I(c("cancel", "interrupt", "progress"))
  )

  session$receive(list(evaluate_request("11", "a", 2L, sample_recipe("a"))))

  # R starts the evaluation of a, whose reply is still to go out when the
  # cancel arrives.
  later::run_now(all = FALSE)

  session$receive(list(list(kind = "cancel", request = "11")))
  session$run()

  expect_identical(stopped, list("11"))
})
