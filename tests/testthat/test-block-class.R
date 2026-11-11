test_that("a block class holds its properties", {

  expect_true(is_block_class(head_class))
  expect_false(is_block_class(new_class_head_block()))

  # R identifies the class by its id, which leads the class vector for S3
  # dispatch
  expect_identical(head_class$id, "head_block")
  expect_identical(head_class$class, c("head_block", "block"))
  expect_identical(class_inputs(head_class), "data")
  expect_identical(class_state_args(head_class), c("n", "direction"))
  expect_identical(class_inputs(merge_class), c("x", "y"))
  expect_identical(class_inputs(rbind_class), "...")

  expect_null(head_class$output)
  expect_identical(head_class$version, 1L)

  lst <- new_block_class("lst_block", inputs = alist(x = , y = ),
                         expr = function() bbquote(c(.(x), .(y))),
                         ui = js_module("lst.js"))

  expect_identical(class_inputs(lst), c("x", "y"))

  none <- new_block_class("none_block", expr = function() "datasets::iris",
                          ui = js_module("none.js"))

  expect_identical(class_inputs(none), character())

  expect_snapshot(print(head_class))
})

test_that("new_block_class() checks a class once", {

  ui <- js_module("x.js")
  expr <- function() bbquote(.(data))

  expect_identical(
    new_block_class("x_block", "data", expr, ui, version = 3L)$version,
    3L
  )

  for (version in list(0L, 1.5, "2", c(1L, 2L), NA_integer_)) {
    expect_error(
      new_block_class("x_block", "data", expr, ui, version = version),
      class = "block_class_version_invalid"
    )
  }

  expect_error(
    new_block_class(1, "data", expr, ui),
    class = "block_class_id_invalid"
  )

  expect_error(
    new_block_class("", "data", expr, ui),
    class = "block_class_id_invalid"
  )

  # A registry id splits at its first `-`, so a class's id holds none
  expect_error(
    new_block_class("my-block", "data", expr, ui),
    class = "block_class_id_invalid",
    regexp = "my-block"
  )

  expect_error(
    new_block_class("x_block", 1, expr, ui),
    class = "block_class_inputs_invalid"
  )

  expect_error(
    new_block_class("x_block", c("data", "data"), expr, ui),
    class = "block_class_inputs_invalid"
  )

  expect_error(
    new_block_class("x_block", alist(data = 1), expr, ui),
    class = "block_class_inputs_invalid"
  )

  expect_error(
    new_block_class("x_block", function(data = NULL) NULL, expr, ui),
    class = "block_class_inputs_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", "expr", ui),
    class = "block_class_expr_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", expr, "x.js"),
    class = "block_class_ui_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", expr, ui, output = "as_text"),
    class = "block_class_output_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", function(...) bbquote(.(data)), ui),
    class = "block_class_args_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", function(data = 1) "data", ui),
    class = "block_class_args_invalid"
  )

  # A constructor takes `block_name` and `locked` beside the state args.
  for (arg in c("block_name", "locked")) {
    expect_error(
      new_block_class(
        "x_block", "data",
        as.function(c(set_names(list(1), arg), quote(bbquote(.(data))))),
        ui
      ),
      paste("cannot have state arg", arg),
      class = "block_class_args_invalid"
    )
  }

  expect_error(
    new_block_class(
      "x_block", "data", function(options = 1) "data", ui,
      requests = list(
        page = new_block_request(function(options) options, new_n_rows_option)
      )
    ),
    class = "block_class_requests_invalid"
  )

  expect_error(
    new_block_class(
      "x_block", "data", expr, ui,
      requests = list(
        page = new_block_request(
          function(data) data,
          options = list(data = new_n_rows_option)
        )
      )
    ),
    class = "block_class_requests_invalid"
  )

  expect_error(
    new_block_class(
      "x_block", "data", expr, ui,
      requests = list(
        upload = new_block_request(
          function(file) file,
          options = list(file = new_n_rows_option),
          action = TRUE
        )
      )
    ),
    class = "block_class_requests_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", expr, ui, requests = list(page = "x")),
    class = "block_class_requests_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", expr, ui,
                    requests = list(function(data) data)),
    class = "block_class_requests_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", expr, ui,
                    requests = list(cols = function(dat) names(dat))),
    class = "block_class_requests_invalid"
  )

  expect_s3_class(
    new_block_class(
      "x_block", "data", function(n = 1L) bbquote(utils::head(.(data), .(n))),
      ui,
      requests = list(
        cols = new_block_request(
          function(data, n, rows, limit = 10L) names(data),
          options = list(rows = new_n_rows_option)
        ),
        upload = new_block_request(function(file, n) file, action = TRUE)
      )
    ),
    "block_class"
  )

  expect_s3_class(
    new_block_class("x_block", "data", function(options = 1) "data", ui),
    "block_class"
  )

  expect_error(
    new_block_class("x_block", "data", function() 1, ui),
    class = "block_class_expr_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", function() function(x) x, ui),
    class = "block_class_expr_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", function() bquote(f(.(sum))), ui),
    class = "block_class_expr_invalid"
  )

  expect_error(
    new_block_class("x_block", "data", function() stop("boom"), ui),
    "boom"
  )

  expect_s3_class(
    new_block_class("x_block", "data", function() blockr_invalid("Not yet."),
                    ui),
    "block_class"
  )

  expect_s3_class(
    new_block_class("x_block", "data", function(by) bbquote(f(.(by))), ui),
    "block_class"
  )
})

test_that("the constructor keeps the state the checks in expr leave", {

  blk <- new_class_head_block(n = 10L)

  expect_s3_class(blk, c("head_block", "block"), exact = TRUE)
  expect_identical(block_state(blk), list(n = 10L, direction = "head"))
  expect_identical(block_name(blk), "Head")
  expect_identical(block_class(blk), head_class)

  blk <- new_class_head_block(direction = "tail", block_name = "Last")

  expect_identical(block_state(blk), list(n = 6L, direction = "tail"))
  expect_identical(block_name(blk), "Last")

  expect_error(block_name(list()), class = "block_class_invalid")

  expect_error(new_class_head_block(m = 10L), "unused argument")
  expect_error(new_class_head_block(direction = "middle"), "should be one of")
  expect_error(
    new_class_head_block(block_name = 1),
    class = "block_name_invalid"
  )

  expect_identical(
    names(formals(new_class_head_block)),
    c("n", "direction", "block_name", "locked")
  )

  # A block's lock is NA unless a script gives it.
  expect_identical(block_locked(new_class_head_block()), NA)
  expect_false(block_locked(new_class_head_block(locked = FALSE)))
  expect_true(block_locked(new_class_head_block(n = 3L, locked = TRUE)))

  for (locked in list("yes", c(TRUE, FALSE), NULL, NA_character_)) {
    expect_error(
      new_class_head_block(locked = locked),
      class = "block_locked_invalid"
    )
  }

  expect_identical(constructor_class(new_class_head_block), head_class)
  expect_error(constructor_class(function() NULL),
               class = "block_constructor_invalid")

  expect_error(block_constructor(new_class_head_block),
               class = "block_class_invalid")
})

test_that("an unset arg stays out of the state until something sets it", {

  expect_identical(
    block_state(new_class_merge_block()),
    list(all_x = FALSE, all_y = FALSE)
  )

  expect_identical(
    block_state(new_class_merge_block(by = "Time")),
    list(by = "Time", all_x = FALSE, all_y = FALSE)
  )

  derived <- block_constructor(
    new_block_class(
      "derived_block",
      inputs = "data",
      expr = function(cols) {
        if (missing(cols)) cols <- "Species"
        bbquote(.(data)[, .(cols)])
      },
      ui = js_module("derived.js")
    )
  )

  expect_identical(block_state(derived()), list(cols = "Species"))

  picky <- block_constructor(
    new_block_class(
      "picky_block",
      inputs = "data",
      expr = function(cols = character()) {
        if (!length(cols)) blockr_invalid("Choose at least one column.")
        bbquote(.(data)[, .(cols)])
      },
      ui = js_module("picky.js")
    )
  )

  expect_identical(block_state(picky()), list(cols = character()))
})

test_that("js_module() keeps the path and the package", {

  mod <- js_module("js/head-block.js", package = "blockr.core")

  expect_s3_class(mod, "js_module")
  expect_identical(mod$path, "js/head-block.js")
  expect_identical(mod$package, "blockr.core")

  expect_identical(js_module("js/head-block.js")$package, "blockr.core")

  withr::with_dir(
    withr::local_tempdir(),
    {
      file.create("mod.js")
      expect_identical(
        js_module("mod.js", package = NULL)$path,
        normalizePath("mod.js")
      )
    }
  )

  expect_error(js_module(1), class = "js_module_path_invalid")
  expect_error(js_module("x.js", package = 1),
               class = "js_module_package_invalid")
})

test_that("blockr_invalid() signals a classed error", {

  n <- 2L

  cnd <- tryCatch(
    blockr_invalid("Choose {n} column{?s}.", class = "my_block_unset"),
    error = identity
  )

  expect_s3_class(cnd, c("my_block_unset", "blockr_invalid", "blockr_error"))
  expect_identical(conditionMessage(cnd), "Choose 2 columns.")
  expect_true(is_invalid_cnd(cnd))
})

test_that("missing args are told apart from other errors", {

  cnd <- tryCatch((function(x) x)(), error = identity)

  expect_true(is_missing_arg_error(cnd))
  expect_true(is_invalid_cnd(cnd))

  old <- simpleError(
    sprintf(
      gettext("argument \"%s\" is missing, with no default", domain = "R"),
      "by"
    )
  )

  expect_true(is_missing_arg_error(old))
  expect_false(is_missing_arg_error(simpleError("argument by")))
  expect_false(is_invalid_cnd(simpleError("boom")))
})

test_that("blocks of a class print, combine and report their state args", {

  blk <- new_class_head_block()

  expect_true(is_block(blk))
  expect_identical(as_block(blk), blk)

  expect_snapshot(print(blk))
  expect_snapshot(print(new_class_rbind_block()))
  expect_snapshot(print(new_class_head_block))

  expect_identical(str_value(blk), "<head_block> n, direction")
  expect_identical(str_value(new_class_rbind_block()), "<rbind_block>")

  expect_identical(block_arity(blk), 1L)
  expect_identical(block_arity(new_class_merge_block()), 2L)
  expect_identical(block_arity(new_class_rbind_block()), NA_integer_)

  blks <- c(a = blk, b = new_class_merge_block())

  expect_s3_class(blks, "blocks")
  expect_named(blks, c("a", "b"))

  expect_error(
    validate_block(structure(list(), block_class = "head_block",
                             class = c("head_block", "block"))),
    class = "block_class_invalid"
  )
})

test_that("str_value() is extensible via NextMethod()", {

  registerS3method(
    "str_value", "test_subblock",
    function(x, ...) paste0(NextMethod(), " [extra]"),
    envir = asNamespace("blockr.core")
  )

  new_test_subblock <- block_constructor(
    new_block_class(
      "test_subblock",
      inputs = "data",
      expr = function(n = 6L) bbquote(utils::head(.(data), n = .(n))),
      ui = js_module("head-block.js", package = "blockr.core")
    )
  )

  blk <- new_test_subblock()

  expect_identical(str_value(blk), "<test_subblock> n [extra]")
  expect_identical(capture.output(str(blk)), " <test_subblock> n [extra]")
})

test_that("a block's board options are those its requests read", {

  with_opts <- block_constructor(
    new_block_class(
      "opts_block",
      inputs = "data",
      expr = function() bbquote(.(data)),
      ui = js_module("opts.js"),
      requests = list(
        page = new_block_request(
          function(data, options) utils::head(data, options),
          options = new_n_rows_option
        ),
        rows = new_block_request(
          function(data, n, dark) min(nrow(data), n),
          options = list(n = new_n_rows_option, dark = new_dark_mode_option)
        )
      )
    )
  )

  expect_setequal(names(board_options(with_opts())), c("n_rows", "dark_mode"))
  expect_length(board_options(new_class_head_block()), 0L)
})

test_that("a block class records the package whose code defines it", {

  expect_identical(head_block$package, "blockr.core")

  expect_identical(
    test_block_class(
      "x_block",
      expr = function() "datasets::iris",
      ui = js_module("x.js", package = NULL),
      package = "blockr.pkg"
    )$package,
    "blockr.pkg"
  )

  # An app script's class records none, as where Shiny sources an app.R into
  # an environment of its own
  cls <- evalq(
    blockr.core::new_block_class(
      "x_block",
      expr = function() "datasets::iris",
      ui = blockr.core::js_module("x.js", package = NULL)
    ),
    new.env(parent = globalenv())
  )

  expect_null(cls$package)
  expect_identical(registry_id_from_block(cls), "x_block")
})

test_that("each of core's block classes gives its id from its name", {

  ns <- asNamespace("blockr.core")
  classes <- Filter(is_block_class, mget(ls(ns, pattern = "_block$"), ns))

  # Each is named as its constructor without `new_`
  for (name in names(classes)) {
    ctor <- get(paste0("new_", name), ns)
    expect_identical(constructor_class(ctor), classes[[name]])
    expect_identical(
      registry_id_from_block(classes[[name]]),
      paste0("blockr.core-", name)
    )
  }
})

test_that("each of core's block classes ships its UI module and its schema", {

  ns <- asNamespace("blockr.core")
  classes <- Filter(is_block_class, mget(ls(ns, pattern = "_block$"), ns))

  expect_length(classes, 12L)

  for (cls in classes) {
    expect_true(nzchar(module_file(cls[["ui"]])), info = cls[["class"]][1L])
    expect_setequal(
      names(class_schema(cls)[["properties"]]),
      class_state_args(cls)
    )
  }
})
