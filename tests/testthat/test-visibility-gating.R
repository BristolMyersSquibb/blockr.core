probe_render <- new.env()
probe_render$ids <- character()

registerS3method(
  "block_output", "probe_block",
  function(x, result, session) {
    probe_render$ids <- c(probe_render$ids, session$ns(NULL))
    NULL
  }
)

registerS3method(
  "block_ui", "probe_block",
  function(id, x, ...) shiny::tagList()
)

probe_source <- function() {
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(expr = reactive(quote(datasets::BOD)), state = list())
        }
      )
    },
    function(id) shiny::tagList(),
    class = "probe_block",
    block_metadata = FALSE
  )
}

probe_passthrough <- function() {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(expr = reactive(quote(identity(data))), state = list())
        }
      )
    },
    function(id) shiny::tagList(),
    class = "probe_block",
    block_metadata = FALSE
  )
}

reset_render_log <- function() {
  probe_render$ids <- character()
}

rendered <- function(id) {
  any(endsWith(probe_render$ids, paste0("block_", id)))
}

evaluated <- function(rv, id) {
  !is.null(rv$blocks[[id]]$server$result())
}

test_that("with no producer every block is visible", {

  reset_render_log()

  board <- new_board(
    blocks = c(a = probe_source(), b = probe_passthrough()),
    links = links(new_link(from = "a", to = "b"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(rv$visible)
      expect_true(rv$eval)

      expect_true(evaluated(rv, "a"))
      expect_true(evaluated(rv, "b"))

      expect_true(rendered("a"))
      expect_true(rendered("b"))
    },
    args = list(x = board)
  )
})

test_that("a producer gates evaluation and rendering on visibility", {

  reset_render_log()

  board <- new_board(
    blocks = c(
      a = probe_source(),
      b = probe_passthrough(),
      c = probe_passthrough(),
      d = probe_passthrough()
    ),
    links = links(
      new_link(from = "a", to = "b"),
      new_link(from = "b", to = "c"),
      new_link(from = "a", to = "d")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_setequal(rv$visible, "b")
      expect_setequal(rv$eval, c("a", "b"))

      expect_true(evaluated(rv, "b"))
      expect_true(rendered("b"))

      expect_true(evaluated(rv, "a"))
      expect_false(rendered("a"))

      expect_false(evaluated(rv, "c"))
      expect_false(rendered("c"))

      expect_false(evaluated(rv, "d"))
      expect_false(rendered("d"))

      board_visible(c("b", "c", "d"))
      session$flushReact()

      expect_true(evaluated(rv, "c"))
      expect_true(rendered("c"))

      expect_true(evaluated(rv, "d"))
      expect_true(rendered("d"))
    },
    args = list(
      x = board,
      callbacks = function(visible, ...) {
        visible("b")
        NULL
      }
    )
  )
})

test_that("the gate_visibility option disables gating", {

  reset_render_log()

  withr::local_options(blockr.gate_visibility = FALSE)

  board <- new_board(
    blocks = c(
      a = probe_source(),
      b = probe_passthrough(),
      c = probe_passthrough()
    ),
    links = links(
      new_link(from = "a", to = "b"),
      new_link(from = "b", to = "c")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      for (id in c("a", "b", "c")) {
        expect_true(evaluated(rv, id))
        expect_true(rendered(id))
      }
    },
    args = list(
      x = board,
      callbacks = function(visible, ...) {
        visible("b")
        NULL
      }
    )
  )
})
