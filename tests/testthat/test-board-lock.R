# The three locks: the load lock and the board lock, which blockr options set,
# and each block's lock, and what R refuses for them in what it runs.

test_that("both locks are off unless an app or a deployment sets them", {

  withr::local_options(blockr.locked = NULL, blockr.lock_load = NULL)
  withr::local_envvar(BLOCKR_LOCKED = NA, BLOCKR_LOCK_LOAD = NA)

  expect_identical(app_locks(), list(board = FALSE, load = FALSE))

  withr::local_options(blockr.locked = TRUE)
  expect_identical(app_locks(), list(board = TRUE, load = FALSE))

  # A deployment sets one as a string, which R reads as as.logical() does.
  withr::local_options(blockr.locked = NULL)
  withr::local_envvar(BLOCKR_LOCK_LOAD = "true", BLOCKR_LOCKED = " T")
  expect_identical(app_locks(), list(board = TRUE, load = TRUE))

  withr::local_envvar(BLOCKR_LOCK_LOAD = "yes")
  expect_error(load_lock_on(), class = "lock_option_invalid")

  withr::local_envvar(BLOCKR_LOCK_LOAD = NA)
  withr::local_options(blockr.lock_load = c(TRUE, TRUE))
  expect_error(load_lock_on(), class = "lock_option_invalid")
})

test_that("R sends the locks that the app sets with its reply to hello", {

  withr::local_options(blockr.locked = TRUE, blockr.lock_load = NULL)

  session <- local_evaluator(hello = FALSE)
  session$receive(hello_message())
  session$run()

  expect_identical(last_reply(session)$lock, list(board = TRUE, load = FALSE))
  expect_identical(
    unclass(encode_message(last_reply(session)["lock"])),
    "{\"lock\":{\"board\":true,\"load\":false}}"
  )
})

test_that("R reads a block's lock by the schema of core's block", {

  expect_identical(read_block_lock(NULL, "Block \"a\""), NA)
  expect_true(read_block_lock(TRUE, "Block \"a\""))
  expect_false(read_block_lock(FALSE, "Block \"a\""))
  expect_error(
    read_block_lock("yes", "Block \"a\""),
    "Block \"a\" holds a lock that is neither true, false nor null.",
    class = "block_locked_invalid"
  )

  session <- local_evaluator()
  req <- evaluate_request("1", "a", 1L, locked_recipe_a(), locked = "yes")

  session$receive(list(req))
  session$run()

  expect_identical(last_reply(session)$status, "error")
  expect_identical(
    condition_class(last_reply(session)),
    "protocol_request_invalid"
  )
})

test_that("with both locks, R refuses a locked block's other state", {

  session <- local_locked_evaluator(proto_locked_board())

  expect_fixture_replies(session, "evaluate-locked")
})

test_that("with both locks, R evaluates a locked block as it opened", {

  session <- local_locked_evaluator(proto_locked_board())

  reply <- evaluated(session, "1", "a", 1L, locked_recipe_a(), locked = FALSE)
  expect_identical(reply$status, "ok")
  expect_identical(reply$summary, "<data.frame> [150 × 5]")

  reply <- evaluated(
    session, "2", "a", 2L, locked_recipe_a("mtcars"),
    locked = FALSE
  )
  expect_identical(reply$status, "error")
  expect_identical(condition_class(reply), "lock_refused")
  expect_null(reply$conditions[[1L]]$phase)
})

test_that("with both locks, R evaluates an open block by its opening links", {

  session <- local_locked_evaluator(proto_locked_board())

  expect_identical(
    evaluated(session, "1", "a", 1L, locked_recipe_a())$status,
    "ok"
  )

  reply <- evaluated(session, "2", "b", 1L, locked_recipe_b(n = 3L))
  expect_identical(reply$status, "ok")
  expect_identical(reply$summary, "<data.frame> [3 × 5]")

  # The bindings come from the opening board, whatever the request names.
  reply <- evaluated(
    session, "3", "b", 2L,
    locked_recipe_b(inputs = list(read_of("b", 1L, "data")))
  )
  expect_identical(reply$status, "error")
  expect_match(
    reply$conditions[[1L]]$message,
    "The board lock fixes the data inputs of block \"b\""
  )

  reply <- evaluated(session, "4", "b", 3L, locked_recipe_b(inputs = list()))
  expect_identical(condition_class(reply), "lock_refused")

  # So does the class, whatever registry id the request names.
  recipe <- locked_recipe_b(n = 2L)
  recipe$registry_id <- "blockr.core-subset_block"
  reply <- evaluated(session, "5", "b", 4L, recipe)
  expect_identical(reply$summary, "<data.frame> [2 × 5]")
})

test_that("with both locks, R refuses a block that the opening board lacks", {

  session <- local_locked_evaluator(proto_locked_board())

  reply <- evaluated(session, "1", "z", 1L, locked_recipe_a())

  expect_identical(reply$status, "error")
  expect_identical(reply$block, "z")
  expect_identical(
    reply$conditions[[1L]]$message,
    paste(
      "The load lock and the board lock hold every block to the opening",
      "board, which has no block \"z\"."
    )
  )
})

test_that("with both locks, R builds a locked block's expression once", {

  calls <- 0L

  cls <- new_block_class(
    "counted_block",
    expr = function(n = 1L) {
      calls <<- calls + 1L
      bbquote(seq_len(.(n)))
    },
    ui = js_module("counted-block.js", package = NULL)
  )

  local_block_class(cls, "blockr.test-counted_block")

  board <- proto_json_board(
    obj(
      id = "counted",
      name = "Counted",
      blocks = obj(
        a = proto_block("blockr.test-counted_block", obj(n = 3L), "Counted")
      ),
      links = obj(),
      stacks = obj(),
      options = obj(),
      plugins = obj()
    )
  )

  recipe <- list(
    registry_id = "blockr.test-counted_block",
    state = list(n = 3L),
    inputs = list()
  )

  # Without the locks, every evaluation calls expr.
  plain <- local_evaluator()
  before <- calls
  evaluated(plain, "1", "a", 1L, recipe)
  expect_identical(calls, before + 1L)

  session <- local_locked_evaluator(board)
  before <- calls

  for (revision in 1:2) {
    reply <- evaluated(session, "1", "a", revision, recipe)
    expect_identical(reply$summary, "<integer> [3]")
  }

  expect_identical(calls, before)
})

test_that("with both locks, R keeps what building a locked block signalled", {

  board <- proto_json_board(
    obj(
      id = "unset",
      name = "Unset",
      blocks = obj(
        a = proto_block(
          "blockr.core-dataset_block",
          obj(package = "datasets"),
          "Data"
        )
      ),
      links = obj(),
      stacks = obj(),
      options = obj(),
      plugins = obj()
    )
  )

  session <- local_locked_evaluator(board)

  recipe <- locked_recipe_a()
  recipe$state <- list(package = "datasets")

  for (revision in 1:2) {
    reply <- evaluated(session, "1", "a", revision, recipe)
    expect_identical(reply$status, "invalid")
    expect_identical(reply$conditions[[1L]]$phase, "expr")
    expect_match(reply$conditions[[1L]]$message, "dataset")
  }
})

test_that("with both locks, R takes the parameters from the request", {

  local_write_block()

  board <- proto_json_board(
    obj(
      id = "writes",
      name = "Writes",
      blocks = obj(
        a = proto_block(
          "blockr.core-dataset_block",
          obj(dataset = "iris", package = "datasets"),
          "Iris"
        ),
        w = proto_block("blockr.io-write_block", obj(), "Write")
      ),
      links = obj(aw = proto_link("a", "w", "")),
      stacks = obj(),
      options = obj(),
      plugins = obj()
    )
  )

  session <- local_locked_evaluator(board)

  expect_identical(
    evaluated(session, "1", "a", 1L, locked_recipe_a())$status,
    "ok"
  )

  recipe <- list(
    registry_id = "blockr.io-write_block",
    state = set_names(list(), character()),
    inputs = list(read_of("a", 1L, ""))
  )

  expect_identical(evaluated(session, "2", "w", 1L, recipe)$status, "invalid")
  expect_identical(
    evaluated(session, "3", "w", 2L, recipe, params = list(save = TRUE))$status,
    "ok"
  )
})

test_that("with both locks, R runs every request as on any board", {

  session <- local_locked_evaluator(proto_locked_board())

  hold_result(session, "a", 2L, recipe = locked_recipe_a())

  # The request names the head block b with the args it gives.
  expect_fixture_replies(session, "request")

  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  testthat::local_mocked_bindings(upload_folder = function() "uploads/3f9a2c")

  # An action runs too, for a block that the opening board lacks.
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
})

test_that("with both locks, R refuses a JSON board it cannot read", {

  withr::local_options(blockr.locked = TRUE, blockr.lock_load = TRUE)

  board <- proto_json_board(
    obj(
      id = "older",
      name = "Older",
      blocks = obj(
        a = proto_block("blockr.foo-bar_block", obj(), "Bar"),
        b = proto_block(
          "blockr.core-head_block",
          obj(n = 6L, direction = "head"),
          "Head"
        )
      ),
      links = obj(),
      stacks = obj(),
      options = obj(),
      plugins = obj()
    )
  )

  board$board$blocks$b$version <- 2L

  text <- structure(unclass(encode_message(board)), class = "json")

  expect_error(
    lock_board(text),
    paste0(
      "It cannot read block \"a\", whose class blockr.foo-bar_block the block ",
      "registry lacks and block \"b\", whose state is at version 2, where its ",
      "class is at version 1."
    ),
    fixed = TRUE,
    class = "locked_board_unreadable"
  )

  # Without the load lock, R reads no block of it.
  withr::local_options(blockr.lock_load = NULL)
  expect_null(lock_board(text))
})

test_that("with both locks, R reads a JSON board's blocks, links and locks", {

  withr::local_options(blockr.locked = TRUE, blockr.lock_load = TRUE)

  text <- structure(
    unclass(encode_message(proto_locked_board())),
    class = "json"
  )

  lock <- lock_board(text)

  expect_named(lock, c("a", "b"))
  expect_identical(lock$a$locked, NA)
  expect_false(lock$b$locked)
  expect_identical(lock$b$inputs, list(data = "a"))
  expect_identical(lock$a$state, list(dataset = "iris", package = "datasets"))
  expect_null(lock$b$state)
  expect_identical(
    deparse1(lock$a$built$value),
    deparse1(quote(datasets::iris))
  )

  # An evaluator with both locks holds every block to what lock_board() read.
  expect_error(
    new_evaluator(text, lock = NULL),
    class = "evaluator_lock_missing"
  )
})

test_that("without the load lock, R holds a block to its first evaluation", {

  session <- local_board_locked_evaluator()

  hold_result(session, "a", 2L)
  hold_result(session, "d", 6L, list(read_of("a", 2L, "data")))
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))

  reply <- evaluated(
    session, "1", "b", 4L,
    sample_recipe("b", list(read_of("d", 6L, "data")))
  )
  expect_identical(reply$status, "error")
  expect_identical(
    reply$conditions[[1L]]$message,
    paste(
      "The board lock fixes the data inputs of block \"b\", and the",
      "evaluation binds them to other blocks."
    )
  )

  # Its lock is null, which follows the board lock.
  recipe <- sample_recipe("a")
  recipe$state$dataset <- "mtcars"
  expect_identical(condition_class(evaluated(session, "2", "a", 3L, recipe)),
                   "lock_refused")

  reply <- evaluated(session, "3", "a", 3L, recipe, locked = TRUE)
  expect_identical(
    reply$conditions[[1L]]$message,
    paste(
      "The lock of block \"a\" fixes its state under the board lock, and the",
      "evaluation gives it another state."
    )
  )

  # A client that sends another lock loosens it.
  expect_identical(
    evaluated(session, "4", "a", 3L, recipe, locked = FALSE)$status,
    "ok"
  )

  # Its first state evaluates, at any revision, in any order of its members.
  recipe <- sample_recipe("a")
  recipe$state <- rev(recipe$state)
  expect_identical(evaluated(session, "5", "a", 4L, recipe)$status, "ok")
})

test_that("without the load lock, a later hello starts afresh, a forget not", {

  session <- local_board_locked_evaluator()

  hold_result(session, "a", 2L)

  recipe <- sample_recipe("a")
  recipe$state$dataset <- "mtcars"

  session$receive(list(list(kind = "forget", blocks = "a")))
  session$run()

  expect_identical(
    condition_class(evaluated(session, "1", "a", 3L, recipe)),
    "lock_refused"
  )

  session$receive(hello_message("9"))
  session$run()

  expect_identical(evaluated(session, "2", "a", 3L, recipe)$status, "ok")
})

test_that("without the board lock, R holds no block to anything", {

  withr::local_options(blockr.locked = NULL, blockr.lock_load = TRUE)

  session <- local_evaluator()

  hold_result(session, "a", 2L)

  recipe <- sample_recipe("a")
  recipe$state$dataset <- "mtcars"

  expect_identical(evaluated(session, "1", "a", 3L, recipe)$status, "ok")
})

test_that("R compares the states of locked blocks as JSON", {

  expect_true(same_json(list(n = 6L, d = "a"), list(d = "a", n = 6)))
  expect_true(same_json(c(1L, NA), c(1, NA)))
  expect_true(same_json(set_names(list(), character()),
                        set_names(list(), character())))

  expect_false(same_json(list(), set_names(list(), character())))
  expect_false(same_json(list(n = 6L), list(n = 7L)))
  expect_false(same_json(list(1L, 2L), list(2L, 1L)))
  expect_false(same_json("1", 1L))
  expect_false(same_json(NULL, list()))
})
