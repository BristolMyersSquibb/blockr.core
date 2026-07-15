probe_render <- new.env()
probe_render$ids <- character()

probe_eval <- new.env()
probe_eval$ids <- character()

probe_args <- new.env()
probe_args$entry_classes <- NULL

probe_construct <- new.env()
probe_construct$ids <- character()

registerS3method(
  "block_output", "probe_block",
  function(x, result, session) {
    probe_render$ids <- c(probe_render$ids, session$ns(NULL))
    NULL
  }
)

registerS3method(
  "expr_server", "probe_block",
  function(x, data, ...) {
    probe_construct$ids <- c(probe_construct$ids, attr(x, "probe_id"))
    NextMethod()
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
  probe_construct$ids <- character()
}

# Drives the background builder synchronously: the production scheduler paces
# the next tick behind a post-flush `later::later()`, which a mock session does
# not run, so bump the pace channel directly to re-run the ticker on the next
# flush.
drive_construction <- function(pace, session) {
  pace(isolate(pace()) + 1L)
}

rendered <- function(id) {
  any(endsWith(probe_render$ids, paste0("block_", id)))
}

evaluated <- function(id) {
  id %in% probe_eval$ids
}

constructed <- function(id) {
  id %in% probe_construct$ids
}

require_blocks <- function(vis, ...) {

  for (id in c(...)) {
    vis$required[[id]](TRUE)
  }

  invisible()
}

render_blocks <- function(vis, ...) {

  for (id in c(...)) {
    vis$visible[[id]]("main")
  }

  invisible()
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

      expect_true(rv$needed())

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

  withr::local_options(blockr.background_construction_delay = 0)

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

      expect_setequal(required_now(vis$required), "b")

      expect_true(evaluated("b"))
      expect_true(rendered("b"))

      expect_true(evaluated("a"))
      expect_false(rendered("a"))

      expect_false(evaluated("c"))
      expect_false(rendered("c"))

      expect_false(evaluated("d"))
      expect_false(rendered("d"))

      require_blocks(vis, "c", "d")
      render_blocks(vis, "c", "d")
      session$flushReact()

      expect_true(evaluated("c"))
      expect_true(rendered("c"))

      expect_true(evaluated("d"))
      expect_true(rendered("d"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "b")
        render_blocks(visibility, "b")
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
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "b")
        render_blocks(visibility, "b")
      }
    )
  )
})

test_that("a link change re-routes the pulled upstream", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

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
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "b")
        render_blocks(visibility, "b")
      }
    )
  )
})

test_that("an off-screen data-observing block does not pull its upstream", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

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
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "c")
        render_blocks(visibility, "c")
      }
    )
  )
})

test_that("an unrelated structural edit does not re-evaluate needed blocks", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

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
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "b")
        render_blocks(visibility, "b")
      }
    )
  )
})

test_that("adding a block does not re-evaluate existing needed blocks", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

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
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "b")
        render_blocks(visibility, "b")
      }
    )
  )
})

test_that("a variadic block receives its inputs as values, not reactives", {

  reset_probes()
  probe_args$entry_classes <- NULL

  withr::local_options(blockr.background_construction_delay = 0)

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
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "c")
        render_blocks(visibility, "c")
      }
    )
  )
})

test_that("an off-screen variadic block does not pull its inputs", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

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
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "e")
        render_blocks(visibility, "e")
      }
    )
  )
})

ordered_board <- function() {
  new_board(
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
}

visible_b <- function(visibility, ...) {
  require_blocks(visibility, "b")
  render_blocks(visibility, "b")
}

test_that("the priority lane builds the needed set ahead of the backlog", {

  reset_probes()

  local_mocked_bindings(schedule_construction = drive_construction)

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      built <- probe_construct$ids

      expect_setequal(built, c("a", "b", "c", "d"))

      # d is off the needed path; it builds last, after the needed set a, b, c,
      # even though topo order (a, d, b, c) would otherwise place it second
      expect_identical(built[[length(built)]], "d")
    },
    args = list(
      x = ordered_board(),
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "c")
        render_blocks(visibility, "c")
      }
    )
  )
})

test_that("opening a view pulls its blocks ahead of a gated backlog", {

  reset_probes()

  local_mocked_bindings(schedule_construction = drive_construction)

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      expect_true(constructed("b"))
      expect_false(constructed("c"))
      expect_false(constructed("d"))

      require_blocks(vis, "c")
      render_blocks(vis, "c")
      session$flushReact()

      expect_true(constructed("c"))
      expect_false(constructed("d"))
    },
    args = list(
      x = ordered_board(),
      plugins = list(),
      callbacks = function(visibility, ...) require_blocks(visibility, "b")
    )
  )
})

test_that("the background constructs every block exactly once", {

  reset_probes()

  local_mocked_bindings(schedule_construction = drive_construction)

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      built <- probe_construct$ids

      expect_setequal(built, c("a", "b", "c", "d"))
      expect_length(built, 4L)

      session$flushReact()

      expect_identical(probe_construct$ids, built)
    },
    args = list(x = ordered_board(), plugins = list(), callbacks = visible_b)
  )
})

test_that("an infinite background delay never fills in the background", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = Inf)

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      expect_true(constructed("a"))
      expect_true(constructed("b"))

      expect_false(constructed("c"))
      expect_false(constructed("d"))

      session$elapse(5000)
      session$flushReact()

      expect_false(constructed("c"))
      expect_false(constructed("d"))

      require_blocks(vis, "c")
      render_blocks(vis, "c")
      session$flushReact()

      expect_true(constructed("c"))
      expect_false(constructed("d"))
    },
    args = list(x = ordered_board(), plugins = list(), callbacks = visible_b)
  )
})

test_that("an infinite background delay never arms the scheduler", {

  reset_probes()

  armed <- new.env(parent = emptyenv())
  armed$called <- FALSE

  local_mocked_bindings(
    schedule_construction = function(pace, session) {
      armed$called <- TRUE
      invisible()
    }
  )

  withr::local_options(blockr.background_construction_delay = Inf)

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      expect_false(armed$called)
    },
    args = list(x = ordered_board(), plugins = list(), callbacks = visible_b)
  )
})

test_that("is_visible is a non-NA check on the slot value", {

  expect_true(is_visible("main"))
  expect_false(is_visible(NA_character_))
})

test_that("channel validators enforce the required and visible contracts", {

  expect_true(valid_required(TRUE))
  expect_true(valid_required(FALSE))
  expect_true(valid_required(NA))
  expect_false(valid_required("x"))
  expect_false(valid_required(NA_character_))

  expect_true(valid_visible("main"))
  expect_true(valid_visible(NA_character_))
  expect_false(valid_visible(""))
  expect_false(valid_visible(NA))
  expect_false(valid_visible(c("a", "b")))
})

test_that("validate_vis hard-errors on an off-contract slot", {

  isolate({
    vis <- list(
      required = new.env(parent = emptyenv()),
      visible = new.env(parent = emptyenv())
    )
    add_vis_slots(vis, "a")

    vis$required[["a"]](1L)
    expect_error(validate_vis(vis), class = "invalid_required")

    vis$required[["a"]](TRUE)
    vis$visible[["a"]]("")
    expect_error(validate_vis(vis), class = "invalid_visible")
  })
})

test_that("required_now returns the TRUE-required blocks", {

  isolate({
    req <- new.env(parent = emptyenv())
    req$a <- reactiveVal(TRUE)
    req$b <- reactiveVal(FALSE)
    req$c <- reactiveVal(NA)
    req$d <- reactiveVal(TRUE)

    expect_setequal(required_now(req), c("a", "d"))
    expect_length(required_now(new.env(parent = emptyenv())), 0L)
  })
})

test_that("ever_required and has_required track declared (non-NA) slots", {

  isolate({
    req <- new.env(parent = emptyenv())
    req$a <- reactiveVal(TRUE)
    req$b <- reactiveVal(FALSE)
    req$c <- reactiveVal(NA)

    expect_setequal(ever_required(req), c("a", "b"))
    expect_true(has_required(req))
    expect_false(has_required(new.env(parent = emptyenv())))
  })
})

test_that("required_fulfilled holds only when every required block is shown", {

  isolate({
    vis <- list(
      required = new.env(parent = emptyenv()),
      visible = new.env(parent = emptyenv())
    )
    add_vis_slots(vis, c("a", "b"))
    vis$required[["a"]](TRUE)
    vis$required[["b"]](TRUE)
    vis$visible[["a"]]("main")
    vis$visible[["b"]]("main")

    expect_true(required_fulfilled(vis))

    vis$visible[["b"]](NA_character_)
    expect_false(required_fulfilled(vis))

    empty <- list(
      required = new.env(parent = emptyenv()),
      visible = new.env(parent = emptyenv())
    )
    expect_true(required_fulfilled(empty))
  })
})

test_that("the background waits for the front-end's rendered report", {

  reset_probes()

  local_mocked_bindings(schedule_construction = drive_construction)

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      expect_true(constructed("a"))
      expect_true(constructed("b"))

      expect_false(constructed("c"))
      expect_false(constructed("d"))

      render_blocks(vis, "b")
      session$flushReact()

      expect_true(constructed("c"))
      expect_true(constructed("d"))
    },
    args = list(
      x = ordered_board(),
      plugins = list(),
      callbacks = function(visibility, ...) require_blocks(visibility, "b")
    )
  )
})

test_that("a zero background delay builds every block up front", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      expect_true(constructed("a"))
      expect_true(constructed("b"))
      expect_true(constructed("c"))
      expect_true(constructed("d"))
    },
    args = list(x = ordered_board(), plugins = list(), callbacks = visible_b)
  )
})
