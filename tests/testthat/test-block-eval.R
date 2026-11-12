test_that("a block is evaluated from its class, state and upstream results", {

  res <- evaluate_block(
    head_class,
    list(n = 3L, direction = "tail"),
    list(data = "a"),
    list(a = datasets::iris, b = datasets::mtcars)
  )

  expect_identical(res$status, "ok")
  expect_identical(res$expr, quote(utils::tail(a, n = 3L)))
  expect_identical(res$result, utils::tail(datasets::iris, n = 3L))
  expect_identical(res$summary, "<data.frame> [3 \u00d7 5]")
  expect_identical(res$conditions, list())

  res <- evaluate_block(
    rbind_class,
    list(),
    list(... = list("a", b2 = "b")),
    list(a = datasets::BOD, b = datasets::BOD)
  )

  expect_identical(res$expr, quote(rbind(a, b2 = b)))
  expect_identical(nrow(res$result), 12L)
})

test_that("a registered class is evaluated by its registry id", {

  withr::defer(unregister_blocks("head_block"))

  register_block(new_class_head_block, "Head", "First rows")

  res <- evaluate_block("head_block", list(n = 2L), list(data = "a"),
                        list(a = datasets::iris))

  expect_identical(res$status, "ok")
  expect_identical(nrow(res$result), 2L)

  expect_error(evaluate_block(1, list()), class = "block_class_invalid")
})

test_that("a state that cannot run makes R run nothing and report invalid", {

  res <- evaluate_block(merge_class, list(), list(x = "a", y = "b"),
                        list(a = datasets::BOD, b = datasets::BOD))

  expect_identical(res$status, "invalid")
  expect_null(res$expr)
  expect_null(res$result)
  expect_length(res$conditions, 1L)
  expect_identical(res$conditions[[1L]]$phase, "expr")
  # The status already says that the state cannot run, so why is a message.
  expect_identical(res$conditions[[1L]]$severity, "message")
  expect_match(res$conditions[[1L]]$message, "by")

  picky <- new_block_class(
    "picky_block",
    inputs = "data",
    expr = function(cols = character()) {
      if (!length(cols)) blockr_invalid("Choose at least one column.")
      bbquote(.(data)[, .(cols), drop = FALSE])
    },
    ui = js_module("picky.js")
  )

  res <- evaluate_block(picky, list(cols = character()), list(data = "a"),
                        list(a = datasets::iris))

  expect_identical(res$status, "invalid")
  expect_identical(res$conditions[[1L]]$message, "Choose at least one column.")
  expect_identical(res$conditions[[1L]]$severity, "message")
  expect_true("blockr_invalid" %in% res$conditions[[1L]]$class)

  unset_fun <- new_block_class(
    "unset_fun_block",
    inputs = "data",
    expr = function(n) function(data) utils::head(data, n = .(n)),
    ui = js_module("unset.js")
  )

  res <- evaluate_block(unset_fun, list(), list(data = "a"),
                        list(a = datasets::iris))

  expect_identical(res$status, "invalid")
  expect_identical(res$conditions[[1L]]$phase, "expr")
})

test_that("failed checks and errors report error, by phase", {

  res <- evaluate_block(head_class, list(), list(data = "a"),
                        list(a = 1:3))

  expect_identical(res$status, "error")
  expect_identical(res$conditions[[1L]]$phase, "inputs")
  expect_null(res$expr)

  res <- evaluate_block(head_class, list(m = 1L), list(data = "a"),
                        list(a = datasets::iris))

  expect_identical(res$status, "error")
  expect_identical(res$conditions[[1L]]$phase, "expr")

  broken <- new_block_class(
    "broken_block",
    inputs = "data",
    expr = function() bbquote(stop("boom")),
    ui = js_module("broken.js")
  )

  res <- evaluate_block(broken, list(), list(data = "a"),
                        list(a = datasets::iris))

  expect_identical(res$status, "error")
  expect_identical(res$expr, quote(stop("boom")))
  expect_identical(res$conditions[[1L]]$phase, "block_eval")
  expect_identical(res$conditions[[1L]]$message, "boom")

  expect_error(
    evaluate_block(head_class, list(), list(), list()),
    class = "block_inputs_unlinked"
  )

  expect_error(
    evaluate_block(head_class, list(), list(data = "a"), list()),
    class = "block_results_missing"
  )
})

test_that("an inputs check returns NULL, and any other value fails it", {

  predicate <- new_block_class(
    "predicate_block",
    inputs = function(data) is.data.frame(data),
    expr = function() bbquote(.(data)),
    ui = js_module("predicate.js")
  )

  for (input in list(1:3, datasets::BOD)) {

    res <- evaluate_block(predicate, list(), list(data = "a"),
                          list(a = input))

    expect_identical(res$status, "error")
    expect_identical(res$conditions[[1L]]$phase, "inputs")
    expect_contains(res$conditions[[1L]]$class, "block_inputs_check_invalid")
  }

  res <- evaluate_block(head_class, list(), list(data = "a"),
                        list(a = datasets::BOD))

  expect_identical(res$status, "ok")
})

test_that("a summary that is no string fails, and the block stays ok", {

  odd <- new_block_class(
    "odd_block",
    expr = function() quote(structure(1, class = "odd_result")),
    ui = js_module("odd.js")
  )

  methods <- list(
    function(result) c("a", "b"),
    function(result) 1,
    function(result) NULL,
    function(result) stop("No summary.")
  )

  for (method in methods) {

    local_mocked_s3_method("result_summary", "odd_result", method)

    res <- evaluate_block(odd)

    expect_identical(res$status, "ok")
    expect_identical(res$result, structure(1, class = "odd_result"))
    expect_null(res$summary)
    expect_length(res$conditions, 1L)
    expect_identical(res$conditions[[1L]]$severity, "error")
    expect_identical(res$conditions[[1L]]$phase, "result_summary")
  }

  local_mocked_s3_method(
    "result_summary",
    "odd_result",
    function(result) c("a", "b")
  )

  res <- evaluate_block(odd)

  expect_contains(res$conditions[[1L]]$class, "result_summary_invalid")
  expect_identical(
    res$conditions[[1L]]$message,
    paste(
      "The `result_summary()` method for a result of class odd_result is",
      "expected to return a string, but it returned an object of class",
      "character and length 2."
    )
  )
})

test_that("warnings and messages are recorded with their phase", {

  noisy <- new_block_class(
    "noisy_block",
    inputs = "data",
    expr = function() {
      message("building")
      bbquote({
        warning("careful")
        .(data)
      })
    },
    ui = js_module("noisy.js")
  )

  expect_silent(
    res <- evaluate_block(noisy, list(), list(data = "a"),
                          list(a = datasets::BOD))
  )

  expect_identical(res$status, "ok")
  expect_identical(res$result, datasets::BOD)
  expect_identical(chr_xtr(res$conditions, "severity"), c("message", "warning"))
  expect_identical(chr_xtr(res$conditions, "phase"), c("expr", "block_eval"))
  expect_identical(chr_xtr(res$conditions, "message"), c("building", "careful"))
})

test_that("a phase keeps every condition that it raises, in order", {

  res <- run_phase("expr", {
    for (i in seq_len(500L)) warning("w", i)
    message("done")
    "value"
  })

  expect_identical(res[["value"]], "value")
  expect_null(res[["error"]])
  expect_identical(
    chr_xtr(res[["conditions"]], "message"),
    c(paste0("w", seq_len(500L)), "done")
  )
})

test_that("a condition whose message is not valid UTF-8 is kept", {

  res <- run_phase("expr", {
    warning("bad \xff\n")
    1
  })

  expect_identical(res[["value"]], 1)
  expect_identical(chr_xtr(res[["conditions"]], "severity"), "warning")
  expect_identical(
    condition_reply(res[["conditions"]][[1L]])[["message"]],
    "bad ?"
  )
})

test_that("a block_eval() method decides whether the expression runs", {

  saver <- new_block_class(
    "saver_block",
    inputs = "data",
    expr = function(auto = FALSE) bbquote(.(data)),
    ui = js_module("saver.js")
  )

  registerS3method(
    "block_eval",
    "saver_block",
    function(x, expr, env, save = FALSE, ...) {
      if (!isTRUE(x$auto) && !save) blockr_invalid("Save to write the file.")
      NextMethod()
    },
    envir = asNamespace("blockr.core")
  )

  res <- evaluate_block(saver, list(auto = FALSE), list(data = "a"),
                        list(a = datasets::BOD))

  expect_identical(res$status, "invalid")
  expect_identical(res$expr, quote(a))
  expect_identical(res$conditions[[1L]]$phase, "block_eval")
  expect_identical(res$conditions[[1L]]$severity, "message")

  res <- evaluate_block(saver, list(auto = FALSE), list(data = "a"),
                        list(a = datasets::BOD), save = TRUE)

  expect_identical(res$status, "ok")
})

test_that("core's block_eval() method evaluates the block expression", {

  env <- eval_env(list(a = 2L))

  x <- structure(list(), class = c("x_block", "block"))

  expect_identical(block_eval(x, quote(a + 1L), env), 3L)
})

test_that("an evaluation is a classed object that prints", {

  res <- evaluate_block(head_class, list(n = 2L), list(data = "a"),
                        list(a = datasets::iris))

  expect_s3_class(res, "block_evaluation")
  expect_snapshot(print(res))

  res <- evaluate_block(merge_class, list(), list(x = "a", y = "b"),
                        list(a = datasets::BOD, b = datasets::BOD))

  expect_snapshot(print(res))

  expect_error(new_block_evaluation("done"),
               class = "block_evaluation_status_invalid")
  expect_error(new_block_evaluation("ok", expr = 1),
               class = "block_evaluation_expr_invalid")
  expect_error(new_block_evaluation("ok", summary = 1),
               class = "block_evaluation_summary_invalid")
  expect_error(new_block_evaluation("ok", conditions = "a"),
               class = "block_evaluation_conditions_invalid")
})

test_that("result_summary() gives a result's class and dimensions", {

  expect_identical(
    result_summary(datasets::mtcars),
    "<data.frame> [32 \u00d7 11]"
  )
  expect_identical(result_summary(1:10), "<integer> [10]")
  expect_identical(result_summary(factor(c("a", "b"))), "<factor> [2]")
  expect_identical(result_summary(NULL), "<NULL>")
  expect_identical(result_summary(sum), "<function>")
  expect_identical(result_summary(function(x) x), "<function>")
  expect_identical(result_summary(new.env()), "<environment>")
  expect_identical(
    result_summary(stats::lm(dist ~ speed, datasets::cars)),
    "<lm>"
  )
  expect_identical(result_summary(structure(list(), class = "my_plot")),
                   "<my_plot>")
  expect_identical(
    result_summary(matrix(1, 1000, 2)),
    "<double> [1,000 \u00d7 2]"
  )

  skip_if_not_installed("tibble")

  expect_identical(
    result_summary(tibble::as_tibble(datasets::mtcars)),
    "<tibble> [32 \u00d7 11]"
  )
})
