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
  expect_identical(res$conditions[[1L]]$severity, "error")
  expect_match(res$conditions[[1L]]$message, "by")

  picky <- new_block_class(
    "picky_block",
    inputs = "data",
    expr = function(cols = character()) {
      if (!length(cols)) invalid("Choose at least one column.")
      bbquote(.(data)[, .(cols), drop = FALSE])
    },
    ui = js_module("picky.js")
  )

  res <- evaluate_block(picky, list(cols = character()), list(data = "a"),
                        list(a = datasets::iris))

  expect_identical(res$status, "invalid")
  expect_identical(res$conditions[[1L]]$message, "Choose at least one column.")
  expect_true("invalid" %in% res$conditions[[1L]]$class)

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
      if (!isTRUE(x$auto) && !save) invalid("Save to write the file.")
      NextMethod()
    },
    envir = asNamespace("blockr.core")
  )

  res <- evaluate_block(saver, list(auto = FALSE), list(data = "a"),
                        list(a = datasets::BOD))

  expect_identical(res$status, "invalid")
  expect_identical(res$expr, quote(a))
  expect_identical(res$conditions[[1L]]$phase, "block_eval")

  res <- evaluate_block(saver, list(auto = FALSE), list(data = "a"),
                        list(a = datasets::BOD), save = TRUE)

  expect_identical(res$status, "ok")
})

test_that("core's block_eval() method keeps what an expression draws", {

  plotter <- new_block_class(
    "plotter_block",
    inputs = "data",
    expr = function() bbquote(graphics::plot(.(data))),
    ui = js_module("plotter.js")
  )

  dev <- grDevices::dev.cur()

  res <- evaluate_block(plotter, list(), list(data = "a"),
                        list(a = datasets::BOD))

  expect_identical(grDevices::dev.cur(), dev)
  expect_identical(res$status, "ok")
  expect_s3_class(res$result, "recordedplot")
  expect_identical(res$summary, "<recordedplot>")

  rerun <- attr(res$result, "rerun")

  expect_type(rerun, "closure")

  grDevices::pdf(NULL)
  grDevices::dev.control(displaylist = "enable")
  rerun()
  expect_gt(length(grDevices::recordPlot()[[1L]]), 0L)
  grDevices::dev.off()

  res <- evaluate_block(head_class, list(), list(data = "a"),
                        list(a = datasets::BOD))

  expect_null(attr(res$result, "rerun"))
})

test_that("an evaluation is a classed object that prints", {

  res <- evaluate_block(head_class, list(n = 2L), list(data = "a"),
                        list(a = datasets::iris))

  expect_s3_class(res, "block_evaluation")
  expect_snapshot(print(res))

  res <- evaluate_block(merge_class, list(), list(x = "a", y = "b"),
                        list(a = datasets::BOD, b = datasets::BOD))

  expect_snapshot(print(res))

  expect_error(new_block_evaluation("done"))
  expect_error(new_block_evaluation("ok", expr = 1))
})

test_that("block_summary() gives a result's class and dimensions", {

  expect_identical(
    block_summary(datasets::mtcars),
    "<data.frame> [32 \u00d7 11]"
  )
  expect_identical(block_summary(1:10), "<integer> [10]")
  expect_identical(block_summary(factor(c("a", "b"))), "<factor> [2]")
  expect_identical(block_summary(NULL), "<NULL>")
  expect_identical(block_summary(sum), "<builtin>")
  expect_identical(block_summary(new.env()), "<environment>")
  expect_identical(
    block_summary(matrix(1, 1000, 2)),
    "<double> [1,000 \u00d7 2]"
  )
})

test_that("block_migrate() leaves a state as it is", {

  state <- list(n = 3L, direction = "tail")

  expect_identical(
    block_migrate(structure(state, class = head_class$class), "0.1.0"),
    state
  )
})
