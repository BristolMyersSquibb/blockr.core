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

  testServer(
    generate_code_server,
    {
      expect_identical(code_export_state(board), "ready")

      expect_match(as.character(output$code_out), "merge", all = FALSE)

      session$setInputs(code_mod = 1)
    },
    args = generate_plugin_args(board)
  )
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
    eval = reactiveValues(a = reactive("ready"), b = reactive("ready")),
    blocks = list(a = list(server = list(expr = reactive(quote(1))))),
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

  make_ro <- function(...) {
    status <- list(...)
    list(
      eval = do.call(reactiveValues, status),
      blocks = set_names(vector("list", length(status)), names(status)),
      board = board
    )
  }

  expect_identical(
    code_export_state(make_ro(a = reactive("ready"), b = reactive("ready"))),
    "ready"
  )
  expect_identical(
    code_export_state(make_ro(a = reactive("ready"), b = reactive("dormant"))),
    "ready"
  )

  expect_identical(
    code_export_state(make_ro(a = reactive("ready"), b = reactive("waiting"))),
    "blocked"
  )
  expect_identical(
    code_export_state(make_ro(a = reactive("ready"), b = reactive("unset"))),
    "blocked"
  )
  expect_identical(
    code_export_state(make_ro(a = reactive("ready"), b = reactive("failed"))),
    "blocked"
  )

  # a block still missing from the built set is pending, not blocked
  expect_identical(
    code_export_state(make_ro(a = reactive("ready"))),
    "pending"
  )
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

test_that("require_all_blocks marks every block required on a gated board", {

  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  board <- list(
    board = new_board(
      blocks = c(
        a = new_dataset_block("BOD"),
        b = new_dataset_block("BOD"),
        c = new_dataset_block("BOD")
      )
    )
  )

  vis <- list(
    required = new.env(parent = emptyenv()),
    visible = new.env(parent = emptyenv())
  )
  add_vis_slots(vis, c("a", "b", "c"))
  vis$required[["a"]](TRUE)

  require_all_blocks(board, vis)

  expect_true(vis$required[["a"]]())
  expect_true(vis$required[["b"]]())
  expect_true(vis$required[["c"]]())
})

test_that("require_all_blocks is a no-op when ungated or standalone", {

  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  board <- list(board = new_board(blocks = c(a = new_dataset_block("BOD"))))

  vis <- list(
    required = new.env(parent = emptyenv()),
    visible = new.env(parent = emptyenv())
  )
  add_vis_slots(vis, "a")

  expect_null(require_all_blocks(board, vis))
  expect_true(is.na(vis$required[["a"]]()))

  expect_null(require_all_blocks(board, NULL))
})

test_that("require_all_blocks tolerates a block without a vis slot", {

  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  board <- list(
    board = new_board(
      blocks = c(a = new_dataset_block("BOD"), b = new_dataset_block("BOD"))
    )
  )

  vis <- list(
    required = new.env(parent = emptyenv()),
    visible = new.env(parent = emptyenv())
  )
  add_vis_slots(vis, "a")
  vis$required[["a"]](FALSE)

  expect_no_error(require_all_blocks(board, vis))
  expect_true(vis$required[["a"]]())
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
        for (id in c("a", "b")) {
          vis$required[[id]](TRUE)
          vis$visible[[id]]("main")
        }
        session$flushReact()

        ro <- list(eval = rv$eval, blocks = rv$blocks, board = rv$board)

        pending <- code_export_state(ro)
        pending_body <- as.character(code_modal_body(pending))

        require_all_blocks(list(board = rv$board), vis)
        session$flushReact()

        ro <- list(eval = rv$eval, blocks = rv$blocks, board = rv$board)

        out <<- list(
          pending = pending,
          pending_body = pending_body,
          state = code_export_state(ro),
          status = reval_if(rv$eval[["m"]]),
          script = export_wrapped_code(
            lst_xtr_reval(rv$blocks, "server", "expr"),
            rv$board
          )
        )
      },
      args = list(x = board, plugins = list())
    )

    out
  }

  configured <- drive(new_merge_block(by = "Time"))

  # before "Show code", `m` is unbuilt: pending, and never junk
  expect_identical(configured$pending, "pending")
  expect_false(grepl("`NA` <-", configured$pending_body, fixed = TRUE))

  # after "Show code", the whole board is built and exports cleanly
  expect_identical(configured$state, "ready")
  expect_identical(configured$status, "ready")
  expect_match(configured$script, "merge")
  expect_false(grepl("`NA` <-", configured$script, fixed = TRUE))

  # an unconfigured off-screen block holds the export back rather than
  # exporting broken code
  unconfigured <- drive(new_merge_block())

  expect_identical(unconfigured$state, "blocked")
  expect_identical(unconfigured$status, "unset")
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(generate_code_ui("gen", new_board()), "shiny.tag.list")
})
