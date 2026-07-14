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
      res <- code()

      expect_type(res, "character")
      expect_length(res, 1L)

      session$setInputs(code_mod = 1)
    },
    args = generate_plugin_args(board)
  )
})

test_that("code export is gated on all blocks being ready", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      c = new_merge_block(by = "Time")
    )
  )

  testServer(
    generate_code_server,
    {
      expect_false(code_export_ready(board))
      expect_null(code())

      session$setInputs(code_mod = 1)
    },
    args = generate_plugin_args(board)
  )
})

test_that("code_export_ready settles on ready or dormant", {

  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  board <- new_board(
    blocks = c(a = new_dataset_block("BOD"), b = new_dataset_block("BOD"))
  )

  make_rv <- function(...) {
    list(eval = do.call(reactiveValues, list(...)), board = board)
  }

  expect_true(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("ready")))
  )
  expect_true(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("dormant")))
  )

  expect_false(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("waiting")))
  )
  expect_false(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("unset")))
  )
  expect_false(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("failed")))
  )

  expect_false(
    code_export_ready(make_rv(a = reactive("ready")))
  )
})

test_that("code modal body shows script or a not-ready note", {

  note <- code_modal_body(NULL)

  expect_s3_class(note, "shiny.tag")
  expect_match(as.character(note), "not ready")

  body <- code_modal_body("y <- 1")

  expect_match(as.character(body), "<pre>")
  expect_match(as.character(body), "y &lt;- 1")
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

        built <- "m" %in% names(reactiveValuesToList(rv$eval))

        require_all_blocks(list(board = rv$board), vis)
        session$flushReact()

        out <<- list(
          built = built,
          status = reval_if(rv$eval[["m"]]),
          ready = isTRUE(
            code_export_ready(list(eval = rv$eval, board = rv$board))
          )
        )
      },
      args = list(x = board, plugins = list())
    )

    out
  }

  configured <- drive(new_merge_block(by = "Time"))

  expect_false(configured$built)
  expect_identical(configured$status, "ready")
  expect_true(configured$ready)

  unconfigured <- drive(new_merge_block())

  expect_identical(unconfigured$status, "unset")
  expect_false(unconfigured$ready)
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(generate_code_ui("gen", new_board()), "shiny.tag.list")
})
