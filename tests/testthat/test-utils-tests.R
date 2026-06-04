test_that("snapshot_reactives detaches from the source reactives", {

  src <- reactiveValues(
    val = 1L,
    block = list(
      server = list(
        expr = reactive("E"),
        cond = reactiveValues(eval = "c")
      )
    )
  )

  snap <- snapshot_reactives(src)

  expect_true(is.reactivevalues(snap))

  isolate({

    expect_identical(snap$val, 1L)
    expect_identical(snap$block$server$expr(), "E")

    expect_true(is.reactivevalues(snap$block$server$cond))
    expect_identical(
      reactiveValuesToList(snap$block$server$cond),
      list(eval = "c")
    )
  })

  isolate(src$val <- 2L)
  isolate(src$block$server$cond$eval <- "d")

  expect_identical(isolate(snap$val), 1L)
  expect_identical(isolate(snap$block$server$cond$eval), "c")
})

test_that("snapshot_reactives defers evaluation errors to read time", {

  snap <- snapshot_reactives(list(thrower = reactive(stop("boom"))))

  expect_error(isolate(snap$thrower()), "boom")
})

test_that("generate_plugin_args returns session-independent args", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("ChickWeight"),
      c = new_merge_block(by = "Time")
    ),
    links = links(
      from = c("a", "b"),
      to = c("c", "c"),
      input = c("x", "y")
    )
  )

  args <- generate_plugin_args(board)

  expect_true(is.reactivevalues(args$board))

  isolate({

    exprs <- lst_xtr_reval(args$board$blocks, "server", "expr")
    expect_named(exprs, c("a", "b", "c"), ignore.order = TRUE)

    cond <- args$board$blocks[["a"]]$server$cond
    expect_true(is.reactivevalues(cond))
    expect_type(reactiveValuesToList(cond), "list")
  })
})
