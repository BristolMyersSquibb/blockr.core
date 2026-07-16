make_vis <- function(ids, frozen = character()) {

  vis <- list(
    required = new.env(parent = emptyenv()),
    visible = new.env(parent = emptyenv()),
    frozen = new.env(parent = emptyenv())
  )

  add_vis_slots(vis, ids)

  for (id in frozen) {
    vis$frozen[[id]](TRUE)
  }

  vis
}

new_state_probe <- function(v = "published") {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          val <- reactiveVal(v)
          observeEvent(input$v, val(input$v))
          list(expr = reactive(quote(identity(data))), state = list(v = val))
        }
      )
    },
    function(id) shiny::tagList(),
    class = "state_probe",
    block_metadata = FALSE
  )
}

new_multi_state_probe <- function(first = FALSE, second = FALSE) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          first_rv <- reactiveVal(first)
          second_rv <- reactiveVal(second)
          observeEvent(input$first, first_rv(input$first))
          observeEvent(input$second, second_rv(input$second))
          list(
            expr = reactive(quote(identity(data))),
            state = list(first = first_rv, second = second_rv)
          )
        }
      )
    },
    function(id) shiny::tagList(),
    class = "multi_state_probe",
    block_metadata = FALSE
  )
}

test_that("each exposed read-only state field tracks its own value", {

  # Regression: freeze_readonly_field() took `live`/`nm` as promises that
  # reactive() only forced on first read, by which time the caller's
  # `for (nm in readonly)` loop had finished -- so every field bound to the
  # LAST one. Every state field then reported the last field's value, and that
  # is what got serialized.

  blk <- new_multi_state_probe()

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      scope <- session$makeScope("expr")

      scope$setInputs(first = TRUE)
      session$flushReact()

      expect_true(reval_if(session$returned$state$first))
      expect_false(reval_if(session$returned$state$second))

      scope$setInputs(first = FALSE, second = TRUE)
      session$flushReact()

      expect_false(reval_if(session$returned$state$first))
      expect_true(reval_if(session$returned$state$second))
    },
    args = list(
      x = blk,
      data = list(data = reactiveVal(datasets::BOD)),
      block_id = "p",
      visibility = make_vis("p")
    )
  )
})

test_that("the board exposes a per-block frozen channel", {

  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true("a" %in% ls(vis$frozen))
      expect_false(block_frozen("a", vis))

      vis$frozen[["a"]](TRUE)
      session$flushReact()

      expect_true(block_frozen("a", vis))
    },
    args = list(x = board, plugins = list())
  )
})

test_that("valid_frozen accepts logical scalars only", {

  expect_true(valid_frozen(TRUE))
  expect_true(valid_frozen(FALSE))
  expect_false(valid_frozen(NA))
  expect_false(valid_frozen("yes"))
  expect_false(valid_frozen(c(TRUE, FALSE)))
})

test_that("a frozen block ignores a forged input", {

  blk <- new_dataset_block("iris")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_equal(session$returned$result(), iris)

      session$makeScope("expr")$setInputs(dataset = "mtcars")
      session$flushReact()

      expect_equal(session$returned$result(), iris)
    },
    args = list(
      x = blk,
      data = list(),
      block_id = "data",
      visibility = make_vis("data", frozen = "data")
    )
  )
})

test_that("freezing and unfreezing toggles a block's input handling", {

  blk <- new_dataset_block("iris")

  vis <- make_vis("data")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      session$makeScope("expr")$setInputs(dataset = "mtcars")
      session$flushReact()

      expect_equal(session$returned$result(), mtcars)

      vis$frozen[["data"]](TRUE)
      session$flushReact()

      session$makeScope("expr")$setInputs(dataset = "iris")
      session$flushReact()

      expect_equal(session$returned$result(), mtcars)

      vis$frozen[["data"]](FALSE)
      session$flushReact()

      session$makeScope("expr")$setInputs(dataset = "iris")
      session$flushReact()

      expect_equal(session$returned$result(), iris)
    },
    args = list(
      x = blk,
      data = list(),
      block_id = "data",
      visibility = vis
    )
  )
})

test_that("a frozen block still re-evaluates on upstream data changes", {

  blk <- new_subset_block("Sepal.Width > 3")

  data <- reactiveVal(iris)

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_equal(
        session$returned$result(),
        subset(iris, Sepal.Width > 3)
      )

      session$makeScope("expr")$setInputs(
        subset = "Sepal.Width > 99",
        select = "",
        eval = 1L
      )
      session$flushReact()

      expect_equal(
        session$returned$result(),
        subset(iris, Sepal.Width > 3)
      )

      data(head(iris, 20L))
      session$flushReact()

      expect_equal(
        session$returned$result(),
        subset(head(iris, 20L), Sepal.Width > 3)
      )
    },
    args = list(
      x = blk,
      data = list(data = data),
      block_id = "sub",
      visibility = make_vis("sub", frozen = "sub")
    )
  )
})

test_that("a frozen block exposes its published state, not a forged one", {

  blk <- new_state_probe("published")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_equal(reval_if(session$returned$state$v), "published")

      session$makeScope("expr")$setInputs(v = "forged")
      session$flushReact()

      expect_equal(reval_if(session$returned$state$v), "published")
    },
    args = list(
      x = blk,
      data = list(data = reactiveVal(datasets::BOD)),
      block_id = "p",
      visibility = make_vis("p", frozen = "p")
    )
  )
})

test_that("an unfrozen block exposes its live state", {

  blk <- new_state_probe("published")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      session$makeScope("expr")$setInputs(v = "forged")
      session$flushReact()

      expect_equal(reval_if(session$returned$state$v), "forged")
    },
    args = list(
      x = blk,
      data = list(data = reactiveVal(datasets::BOD)),
      block_id = "p",
      visibility = make_vis("p")
    )
  )
})

test_that("a frozen block reverts writes to externally controllable state", {

  blk <- new_subset_block("Sepal.Width > 3")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_equal(reval_if(session$returned$state$subset), "Sepal.Width > 3")

      session$makeScope("expr")$setInputs(
        subset = "forged",
        select = "",
        eval = 1L
      )
      session$flushReact()

      expect_equal(reval_if(session$returned$state$subset), "Sepal.Width > 3")
    },
    args = list(
      x = blk,
      data = list(data = reactiveVal(iris)),
      block_id = "sub",
      visibility = make_vis("sub", frozen = "sub")
    )
  )
})

test_that("an unfrozen block keeps writes to externally controllable state", {

  blk <- new_subset_block("Sepal.Width > 3")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      session$makeScope("expr")$setInputs(
        subset = "Sepal.Width > 9",
        select = "",
        eval = 1L
      )
      session$flushReact()

      expect_equal(reval_if(session$returned$state$subset), "Sepal.Width > 9")
    },
    args = list(
      x = blk,
      data = list(data = reactiveVal(iris)),
      block_id = "sub",
      visibility = make_vis("sub")
    )
  )
})
