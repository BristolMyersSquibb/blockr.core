probe_render <- new.env()
probe_render$ids <- character()

probe_eval <- new.env()
probe_eval$ids <- character()

probe_args <- new.env()
probe_args$entry_classes <- NULL

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

registerS3method(
  "block_eval", "probe_block",
  function(x, expr, env, ...) {
    probe_eval$ids <- c(probe_eval$ids, attr(x, "probe_id"))
    NextMethod()
  }
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

probe_data_observer <- function() {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          observeEvent(data(), NULL)
          list(expr = reactive(quote(identity(data))), state = list())
        }
      )
    },
    function(id) shiny::tagList(),
    class = "probe_block",
    block_metadata = FALSE
  )
}

probe_variadic <- function() {
  new_transform_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {

          observe(
            {
              ks <- names(...args)
              req(length(ks) > 0)
              probe_args$entry_classes <- chr_ply(
                ks, function(k) class(...args[[k]])[1L]
              )
            }
          )

          list(expr = reactive(quote(datasets::BOD)), state = list())
        }
      )
    },
    function(id) shiny::tagList(),
    class = "probe_block",
    block_metadata = FALSE
  )
}

with_id <- function(blk, id) {
  attr(blk, "probe_id") <- id
  blk
}

reset_probes <- function() {
  probe_render$ids <- character()
  probe_eval$ids <- character()
}

rendered <- function(id) {
  any(endsWith(probe_render$ids, paste0("block_", id)))
}

evaluated <- function(id) {
  id %in% probe_eval$ids
}

test_that("with no producer every block is visible", {

  reset_probes()

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_passthrough(), "b")
    ),
    links = links(new_link(from = "a", to = "b"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(rv$visible)

      expect_true(evaluated("a"))
      expect_true(evaluated("b"))

      expect_true(rendered("a"))
      expect_true(rendered("b"))
    },
    args = list(x = board, plugins = list())
  )
})

test_that("a producer gates evaluation and rendering on visibility", {

  reset_probes()

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_passthrough(), "b"),
      c = with_id(probe_passthrough(), "c"),
      d = with_id(probe_passthrough(), "d")
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

      expect_true(evaluated("b"))
      expect_true(rendered("b"))

      expect_true(evaluated("a"))
      expect_false(rendered("a"))

      expect_false(evaluated("c"))
      expect_false(rendered("c"))

      expect_false(evaluated("d"))
      expect_false(rendered("d"))

      board_visible(c("b", "c", "d"))
      session$flushReact()

      expect_true(evaluated("c"))
      expect_true(rendered("c"))

      expect_true(evaluated("d"))
      expect_true(rendered("d"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visible, ...) {
        visible("b")
        NULL
      }
    )
  )
})

test_that("the gate_visibility option disables gating", {

  reset_probes()

  withr::local_options(blockr.gate_visibility = FALSE)

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_passthrough(), "b"),
      c = with_id(probe_passthrough(), "c")
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
        expect_true(evaluated(id))
        expect_true(rendered(id))
      }
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visible, ...) {
        visible("b")
        NULL
      }
    )
  )
})

test_that("a link change re-routes the pulled upstream", {

  reset_probes()

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      c = with_id(probe_source(), "c"),
      b = with_id(probe_passthrough(), "b")
    ),
    links = links(ab = new_link("a", "b", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("a"))
      expect_true(evaluated("b"))
      expect_false(evaluated("c"))

      reset_probes()

      board_update(
        list(
          links = list(rm = "ab", add = links(cb = new_link("c", "b", "data")))
        )
      )
      session$flushReact()

      expect_true(evaluated("c"))
      expect_true(evaluated("b"))
      expect_false(evaluated("a"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visible, ...) {
        visible("b")
        NULL
      }
    )
  )
})

test_that("an off-screen data-observing block does not pull its upstream", {

  reset_probes()

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_data_observer(), "b"),
      c = with_id(probe_source(), "c")
    ),
    links = links(new_link("a", "b", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("c"))

      expect_false(evaluated("a"))
      expect_false(evaluated("b"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visible, ...) {
        visible("c")
        NULL
      }
    )
  )
})

test_that("an unrelated structural edit does not re-evaluate needed blocks", {

  reset_probes()

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_passthrough(), "b"),
      x = with_id(probe_source(), "x")
    ),
    links = links(new_link("a", "b", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("a"))
      expect_true(evaluated("b"))
      expect_false(evaluated("x"))

      reset_probes()

      board_update(
        list(blocks = list(mod = list(x = list(block_name = "renamed"))))
      )
      session$flushReact()

      expect_false(evaluated("a"))
      expect_false(evaluated("b"))
      expect_false(evaluated("x"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visible, ...) {
        visible("b")
        NULL
      }
    )
  )
})

test_that("adding a block does not re-evaluate existing needed blocks", {

  reset_probes()

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_passthrough(), "b")
    ),
    links = links(new_link("a", "b", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("a"))
      expect_true(evaluated("b"))

      reset_probes()

      board_update(
        list(blocks = list(add = blocks(d = with_id(probe_source(), "d"))))
      )
      session$flushReact()

      expect_false(evaluated("a"))
      expect_false(evaluated("b"))
      expect_false(evaluated("d"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visible, ...) {
        visible("b")
        NULL
      }
    )
  )
})

test_that("a variadic block receives its inputs as values, not reactives", {

  reset_probes()
  probe_args$entry_classes <- NULL

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_source(), "b"),
      c = with_id(probe_variadic(), "c")
    ),
    links = links(new_link("a", "c", "1"), new_link("b", "c", "2"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_length(probe_args$entry_classes, 2)
      expect_setequal(probe_args$entry_classes, "data.frame")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visible, ...) {
        visible("c")
        NULL
      }
    )
  )
})

test_that("an off-screen variadic block does not pull its inputs", {

  reset_probes()

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_source(), "b"),
      c = with_id(probe_variadic(), "c"),
      e = with_id(probe_source(), "e")
    ),
    links = links(new_link("a", "c", "1"), new_link("b", "c", "2"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("e"))

      expect_false(evaluated("a"))
      expect_false(evaluated("b"))
      expect_false(evaluated("c"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visible, ...) {
        visible("e")
        NULL
      }
    )
  )
})
