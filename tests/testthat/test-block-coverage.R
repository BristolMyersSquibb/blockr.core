test_that("tracing block classes does nothing outside covr", {

  withr::local_envvar(R_COVR = "")

  expect_null(trace_block_classes("stats"))
})

test_that("tracing block classes under covr fails on a sealed namespace", {

  withr::local_envvar(R_COVR = "true")

  expect_error(
    trace_block_classes("stats"),
    class = "trace_block_classes_late"
  )
})

test_that("every copy of a block class's functions is bound", {

  env <- new.env()
  env$head_class <- head_class
  env$new_head_block <- new_class_head_block
  env$merge_class <- merge_class

  bind_class_funs(env)

  expect_setequal(
    grep("^\\.__covr\\.", ls(env, all.names = TRUE), value = TRUE),
    paste0(
      ".__covr.",
      c("head_class.inputs", "head_class.expr", "head_class.requests.n_max",
        "new_head_block.attr.inputs", "new_head_block.attr.expr",
        "new_head_block.attr.requests.n_max", "new_head_block.env.inputs",
        "new_head_block.env.expr", "new_head_block.env.requests.n_max",
        "merge_class.expr", "merge_class.requests.shared")
    )
  )

  # Bound are the functions the classes hold, which covr rewrites in place
  expect_identical(
    rlang::obj_address(env[[".__covr.head_class.expr"]]),
    rlang::obj_address(head_class[["expr"]])
  )

  ctor_cls <- environment(new_class_head_block)[["cls"]]

  expect_identical(
    rlang::obj_address(env[[".__covr.new_head_block.env.requests.n_max"]]),
    rlang::obj_address(ctor_cls[["requests"]][["n_max"]][["fun"]])
  )

  bind_class_funs(env)

  expect_length(grep("^\\.__covr\\.", ls(env, all.names = TRUE)), 11L)
})

test_that("a namespace without block classes gets no bindings", {

  env <- new.env()
  env$f <- function() 1

  bind_class_funs(env)

  expect_identical(ls(env, all.names = TRUE), "f")
})
