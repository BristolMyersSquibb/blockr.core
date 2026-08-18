test_that("generate code", {

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

  plugin_args <- generate_plugin_args(board)
  plugin_args$update <- reactiveVal()

  testServer(
    generate_code_server,
    {
      expect_identical(code_export_state(board), "ready")

      expect_match(as.character(output$code_out), "merge", all = FALSE)

      session$setInputs(code_mod = 1)

      # Showing the code asks for construction only: nothing joins the eval set
      expect_identical(update(), list(construct = c("a", "b", "c")))

      session$setInputs(code_eval = 1)

      expect_identical(update(), list(evaluate = c("a", "b", "c")))
    },
    args = plugin_args
  )
})

test_that("the code modal offers a one-off evaluation", {

  modal <- as.character(code_modal(NS("gen")))

  expect_match(modal, "id=\"gen-code_eval\"", fixed = TRUE)
  expect_match(modal, "action-button", fixed = TRUE)
})

test_that("export would emit `NA` for an eval-complete but unbuilt board", {

  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  board <- new_board(
    blocks = c(a = new_dataset_block("BOD"), b = new_dataset_block("BOD"))
  )

  # Only `a` carries an expression: `b` is reported ready but was never built.
  # Handing this partial set to the exporter indexes `b` out of the block list
  # and assigns to a variable literally named `NA` -- the junk this fix guards
  # against.
  junk <- export_wrapped_code(list(a = quote(datasets::BOD)), board)

  expect_true(grepl("`NA` <-", junk, fixed = TRUE))

  # code_export_state gates on the built set, so the plugin never reaches the
  # exporter with `b` missing: it reports pending instead of exporting junk.
  ro <- list(
    blocks = list(a = list(server = list(expr = reactive(quote(1))))),
    conditions = empty_conditions_frame(),
    board = board
  )

  expect_identical(code_export_state(ro), "pending")
})

test_that("code_export_state distinguishes ready, blocked and pending", {

  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  board <- new_board(
    blocks = c(a = new_dataset_block("BOD"), b = new_dataset_block("BOD"))
  )

  blk <- function(state_ready = TRUE) {
    list(server = list(state_ready = state_ready))
  }

  errored <- function(id) {
    rbind(
      empty_conditions_frame(),
      data.frame(
        block = id,
        phase = "eval",
        severity = "error",
        message = "boom",
        id = "cnd"
      )
    )
  }

  make_ro <- function(blocks, conditions = empty_conditions_frame()) {
    list(blocks = blocks, conditions = conditions, board = board)
  }

  expect_identical(
    code_export_state(make_ro(list(a = blk(), b = blk()))),
    "ready"
  )

  # A block whose user inputs were never set holds the export back, whether or
  # not it has ever been evaluated
  expect_identical(
    code_export_state(make_ro(list(a = blk(), b = blk(FALSE)))),
    "blocked"
  )

  # So does an error a block reported the last time it ran, which outlives the
  # evaluation that raised it
  expect_identical(
    code_export_state(make_ro(list(a = blk(), b = blk()), errored("b"))),
    "blocked"
  )

  # A block still missing from the built set is pending, not blocked
  expect_identical(code_export_state(make_ro(list(a = blk()))), "pending")
})

test_that("code modal body shows a script, a preparing or a not-ready note", {

  ready <- code_modal_body("ready", "y <- 1")

  expect_match(as.character(ready), "<pre>")
  expect_match(as.character(ready), "y &lt;- 1")

  pending <- code_modal_body("pending")

  expect_s3_class(pending, "shiny.tag")
  expect_match(as.character(pending), "Preparing")

  blocked <- code_modal_body("blocked")

  expect_s3_class(blocked, "shiny.tag")
  expect_match(as.character(blocked), "not ready")
})

test_that("show code builds the board without evaluating or gating it", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_dataset_block("BOD")
    )
  )

  withr::local_options(blockr.background_construction_delay = Inf)

  testServer(
    get_s3_method("board_server", board),
    {
      vis$gate("front-end")
      board_update(
        list(sustain = list(`front-end` = list(set = "a")), construct = "b")
      )
      vis$visible[["a"]](TRUE)
      session$flushReact()

      expect_identical(reval_if(rv$eval[["b"]]), "dormant")
      expect_false("c" %in% names(rv$blocks))

      session$setInputs(`generate_code-code_mod` = 1)
      session$flushReact()

      # The deferred block is built for its expression, and stays dormant: the
      # export needs blocks present, not run
      expect_true("c" %in% names(rv$blocks))
      expect_identical(reval_if(rv$eval[["c"]]), "dormant")
      expect_identical(reval_if(rv$eval[["b"]]), "dormant")

      # The front-end's claim is left exactly as it was, and the export adds
      # none of its own
      expect_identical(rv$claims(), list(`front-end` = "a"))

      session$setInputs(`generate_code-code_eval` = 1)
      session$flushReact()

      # The one-off runs them and hands them back, leaving nothing held beyond
      # the front-end's own claim
      expect_length(rv$evaluating(), 0L)
      expect_identical(rv$claims(), list(`front-end` = "a"))
      expect_identical(reval_if(rv$eval[["c"]]), "dormant")
    },
    args = list(x = board, plugins = board_plugins(board, "generate_code"))
  )
})

test_that("show code requires the whole board, gating export on config", {

  drive <- function(m) {

    board <- new_board(
      blocks = c(
        a = new_dataset_block("BOD"),
        b = new_dataset_block("BOD"),
        m = m
      ),
      links = links(new_link("a", "m", "x"), new_link("b", "m", "y"))
    )

    withr::local_options(blockr.background_construction_delay = Inf)

    out <- NULL

    testServer(
      get_s3_method("board_server", board),
      {
        vis$gate("front-end")
        board_update(
          list(sustain = list(`front-end` = list(set = c("a", "b"))))
        )

        for (id in c("a", "b")) {
          vis$visible[[id]](TRUE)
        }

        session$flushReact()

        read_only <- function() {
          list(blocks = rv$blocks, conditions = rv$conditions, board = rv$board)
        }

        pending <- code_export_state(read_only())
        pending_body <- as.character(code_modal_body(pending))

        session$setInputs(`generate_code-code_mod` = 1)
        session$flushReact()

        out <<- list(
          pending = pending,
          pending_body = pending_body,
          state = code_export_state(read_only()),
          status = reval_if(rv$eval[["m"]]),
          script = export_wrapped_code(
            lst_xtr_reval(rv$blocks, "server", "expr"),
            rv$board
          )
        )
      },
      args = list(x = board, plugins = board_plugins(board, "generate_code"))
    )

    out
  }

  configured <- drive(new_merge_block(by = "Time"))

  # before "Show code", `m` is unbuilt: pending, and never junk
  expect_identical(configured$pending, "pending")
  expect_false(grepl("`NA` <-", configured$pending_body, fixed = TRUE))

  # After "Show code", the whole board is built and exports cleanly -- while
  # `m` stays dormant, since building it is all the export needed
  expect_identical(configured$state, "ready")
  expect_identical(configured$status, "dormant")
  expect_match(configured$script, "merge")
  expect_false(grepl("`NA` <-", configured$script, fixed = TRUE))

  # an unconfigured off-screen block holds the export back rather than
  # exporting broken code, without having to be evaluated to say so
  unconfigured <- drive(new_merge_block())

  expect_identical(unconfigured$state, "blocked")
  expect_identical(unconfigured$status, "dormant")
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(generate_code_ui("gen", new_board()), "shiny.tag.list")
})

test_that("showing code builds a deferred block in the browser", {

  skip_on_cran()

  app_path <- system.file("examples", "board", "code", "app.R",
                          package = "blockr.core")

  app <- try(
    shinytest2::AppDriver$new(
      app_path,
      name = "code",
      seed = 42,
      load_timeout = 30 * 1000
    )
  )

  testthat::skip_if(
    inherits(app, "try-error"),
    "Cannot start shinytest2 code board app."
  )

  on.exit(app$stop())

  # The `b` block is off screen on a board that defers construction, so it
  # starts out unbuilt
  expect_identical(app$get_value(export = "my_board-built"), "a")

  app$click(selector = "#my_board-generate_code-code_mod")

  expect_setequal(
    app$wait_for_value(export = "my_board-built", ignore = list("a")),
    c("a", "b")
  )

  # Built for its expression, and left dormant rather than evaluated
  expect_identical(app$get_value(export = "my_board-status_b"), "dormant")

  # The construct request lands before the modal output is rendered and sent,
  # so wait on the script itself rather than on the block having been built
  app$wait_for_js(
    paste0(
      "document.querySelector('#my_board-generate_code-code_out')",
      ".textContent.includes('ChickWeight')"
    )
  )

  expect_match(
    app$get_html("#my_board-generate_code-code_out"),
    "ChickWeight",
    fixed = TRUE
  )
})
