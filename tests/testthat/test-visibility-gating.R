probe_render <- new.env()
probe_render$ids <- character()

probe_eval <- new.env()
probe_eval$ids <- character()

probe_args <- new.env()
probe_args$entry_classes <- NULL

probe_construct <- new.env()
probe_construct$ids <- character()

probe_push <- new.env()
probe_push$ids <- character()

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

# Same probe source, different dataset (zero-arg on purpose: constructor
# arguments double as block state). For tests where a re-routed input must
# resolve to a different object -- an input that is the same object as before
# is skipped by the unchanged-inputs guard in block_server().
probe_source_alt <- function() {
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(expr = reactive(quote(datasets::ChickWeight)), state = list())
        }
      )
    },
    function(id) shiny::tagList(),
    class = "probe_block",
    block_metadata = FALSE
  )
}

probe_ui_ready <- function() {
  new_data_block(
    function(id, ui_ready) {
      moduleServer(
        id,
        function(input, output, session) {

          observe(
            {
              req(ui_ready())
              probe_push$ids <- c(probe_push$ids, session$ns(NULL))
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

# Externally controllable, so a board update `mod` delta can edit its
# expression -- standing in for a consumer editing a block that sits off screen.
probe_select <- function(col = "demand") {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          sel <- reactiveVal(col)

          list(
            expr = reactive(bquote(subset(data, select = .(as.name(sel()))))),
            state = list(col = sel)
          )
        }
      )
    },
    function(id) shiny::tagList(),
    class = "probe_block",
    external_ctrl = TRUE,
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
  probe_push$ids <- character()
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

pushed <- function(id) {
  any(endsWith(probe_push$ids, paste0("block_", id, "-expr")))
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
    vis$visible[[id]](TRUE)
  }

  invisible()
}

park_blocks <- function(vis, ...) {

  for (id in c(...)) {
    vis$required[[id]](FALSE)
    vis$visible[[id]](FALSE)
  }

  invisible()
}

block_conditions <- function(rv, id, severity) {
  cnd <- rv$conditions()
  cnd[cnd$block == id & cnd$severity == severity, ]
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

test_that("ui_ready follows the visible channel of a needed block", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      a = with_id(probe_ui_ready(), "a"),
      b = with_id(probe_passthrough(), "b")
    ),
    links = links(new_link(from = "a", to = "b"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("a"))
      expect_false(rendered("a"))
      expect_false(pushed("a"))

      render_blocks(vis, "a")
      session$flushReact()

      expect_true(pushed("a"))
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

test_that("ui_ready is always set with nothing gating visibility", {

  reset_probes()

  board <- new_board(blocks = c(a = with_id(probe_ui_ready(), "a")))

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(pushed("a"))
    },
    args = list(x = board, plugins = list())
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
      # A different dataset than a's: the re-routed input must actually change
      # for b to re-evaluate -- an input that is the same object as before is
      # skipped (see the unchanged-inputs test below).
      c = with_id(probe_source_alt(), "c"),
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

test_that("a needed round trip with unchanged inputs does not re-evaluate", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  vis_env <- NULL

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
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

      reset_probes()

      # Park the chain, as a view switch whose visibility updates land across
      # several flushes does: b goes un-needed, taking a with it ...
      vis_env$required[["b"]](FALSE)
      session$flushReact()

      # ... and comes back. Nothing upstream changed, so nothing re-evaluates:
      # the unchanged-inputs guard returns the cached results instead of
      # re-running the block expressions.
      vis_env$required[["b"]](TRUE)
      session$flushReact()

      expect_false(evaluated("a"))
      expect_false(evaluated("b"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        vis_env <<- visibility
        require_blocks(visibility, "b")
        render_blocks(visibility, "b")
      }
    )
  )
})

test_that("a dormant block reports stale when an upstream re-evaluates", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s1 = with_id(probe_source(), "s1"),
      s2 = with_id(probe_source_alt(), "s2"),
      a = with_id(probe_passthrough(), "a"),
      r = with_id(probe_passthrough(), "r")
    ),
    links = links(
      sa = new_link("s1", "a", "data"),
      ar = new_link("a", "r", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("a"))
      expect_true(evaluated("r"))
      expect_identical(rv$eval[["r"]](), "ready")

      # Park r off-screen: it drops out of the eval set and goes dormant, while
      # a stays required (its panel is still open).
      vis$required[["r"]](FALSE)
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "dormant")

      # Re-route a from s1 to s2 (a different dataset): a re-evaluates to a new
      # result, breaking r's cached input -- but r is dormant and never re-runs.
      reset_probes()

      board_update(
        list(
          links = list(
            rm = "sa",
            add = links(s2a = new_link("s2", "a", "data"))
          )
        )
      )
      session$flushReact()

      expect_true(evaluated("a"))
      expect_false(evaluated("r"))

      # r's reported status now reflects that its last-known result is stale.
      expect_identical(rv$eval[["r"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "a", "r")
        render_blocks(visibility, "a", "r")
      }
    )
  )
})

test_that("a dormant block whose upstreams are unchanged stays dormant", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      a = with_id(probe_passthrough(), "a"),
      r = with_id(probe_passthrough(), "r")
    ),
    links = links(
      sa = new_link("s", "a", "data"),
      ar = new_link("a", "r", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

      # Park the whole chain, as a view switch does: r and its upstream a both
      # go dormant. a's last result survives dormancy, so r's cached input still
      # matches and r is not stale.
      vis$required[["a"]](FALSE)
      vis$required[["r"]](FALSE)
      session$flushReact()

      expect_identical(rv$eval[["a"]](), "dormant")
      expect_identical(rv$eval[["r"]](), "dormant")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "a", "r")
        render_blocks(visibility, "a", "r")
      }
    )
  )
})

test_that("staleness propagates to the whole dormant downstream cone", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s1 = with_id(probe_source(), "s1"),
      s2 = with_id(probe_source_alt(), "s2"),
      a = with_id(probe_passthrough(), "a"),
      b = with_id(probe_passthrough(), "b"),
      r = with_id(probe_passthrough(), "r")
    ),
    links = links(
      s1a = new_link("s1", "a", "data"),
      ab = new_link("a", "b", "data"),
      br = new_link("b", "r", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

      # Park b and r off-screen; a stays required so it re-evaluates below.
      vis$required[["b"]](FALSE)
      vis$required[["r"]](FALSE)
      session$flushReact()

      expect_identical(rv$eval[["b"]](), "dormant")
      expect_identical(rv$eval[["r"]](), "dormant")

      # Re-route a to a new dataset: a re-evaluates. b (a's direct downstream)
      # is stale from the changed input; r is stale transitively via b, even
      # though b -- being dormant -- never re-evaluated.
      board_update(
        list(
          links = list(
            rm = "s1a",
            add = links(s2a = new_link("s2", "a", "data"))
          )
        )
      )
      session$flushReact()

      expect_identical(rv$eval[["b"]](), "stale")
      expect_identical(rv$eval[["r"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "a", "b", "r")
        render_blocks(visibility, "a", "b", "r")
      }
    )
  )
})

test_that("re-routing a dormant block's input marks it stale", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_source_alt(), "b"),
      r = with_id(probe_passthrough(), "r")
    ),
    links = links(ar = new_link("a", "r", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

      # Park r; a and b stay required (both ready).
      vis$required[["r"]](FALSE)
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "dormant")

      reset_probes()

      # Swap r's input from a to b. r never re-evaluates, but its consumed input
      # (a's result) is no longer what feeds it, so it is stale.
      board_update(
        list(
          links = list(rm = "ar", add = links(br = new_link("b", "r", "data")))
        )
      )
      session$flushReact()

      expect_false(evaluated("r"))
      expect_identical(rv$eval[["r"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "a", "b", "r")
        render_blocks(visibility, "a", "b", "r")
      }
    )
  )
})

test_that("a stale block that re-evaluates is dormant when parked again", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_source_alt(), "b"),
      r = with_id(probe_passthrough(), "r")
    ),
    links = links(ar = new_link("a", "r", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      vis$required[["r"]](FALSE)
      session$flushReact()

      board_update(
        list(
          links = list(rm = "ar", add = links(br = new_link("b", "r", "data")))
        )
      )
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "stale")

      # Put r back on screen: it evaluates against its new input and is current
      # again, so parking it a second time leaves it dormant. Neither b's result
      # nor its status changed in between, so the verdict has to be recomputed
      # off r's own last evaluation.
      require_blocks(vis, "r")
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

      vis$required[["r"]](FALSE)
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "dormant")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "a", "b", "r")
        render_blocks(visibility, "a", "b", "r")
      }
    )
  )
})

test_that("an evaluation request brings a stale block current", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  upd_channel <- NULL

  board <- new_board(
    blocks = c(
      s1 = with_id(probe_source(), "s1"),
      s2 = with_id(probe_source_alt(), "s2"),
      a = with_id(probe_passthrough(), "a"),
      r = with_id(probe_passthrough(), "r")
    ),
    links = links(
      s1a = new_link("s1", "a", "data"),
      ar = new_link("a", "r", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

      # Park the whole a -> r chain off screen, then re-route a to a different
      # dataset: neither re-evaluates, both report the break as stale.
      park_blocks(vis, "a", "r")
      session$flushReact()

      board_update(
        list(
          links = list(
            rm = "s1a",
            add = links(s2a = new_link("s2", "a", "data"))
          )
        )
      )
      session$flushReact()

      expect_identical(rv$eval[["a"]](), "stale")
      expect_identical(rv$eval[["r"]](), "stale")

      reset_probes()

      upd_channel(list(evaluate = "r"))
      session$flushReact()

      # The request pulls in a, the unevaluated upstream r needs for a result,
      # and both are current afterwards -- reported as dormant, not stale.
      expect_true(evaluated("a"))
      expect_true(evaluated("r"))

      expect_identical(rv$eval[["a"]](), "dormant")
      expect_identical(rv$eval[["r"]](), "dormant")

      # The request is spent, and nothing about what is on screen changed.
      expect_length(rv$evaluating(), 0L)

      expect_false(vis$required[["r"]]())
      expect_false(vis$visible[["r"]]())
      expect_false(rendered("r"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, update, ...) {
        upd_channel <<- update
        require_blocks(visibility, "s1", "s2", "a", "r")
        render_blocks(visibility, "s1", "s2", "a", "r")
      }
    )
  )
})

select_board <- function() {
  new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      r = with_id(probe_select(), "r")
    ),
    links = links(sr = new_link("s", "r", "data"))
  )
}

edit_col <- function(value) {
  list(blocks = list(mod = list(r = list(col = value))))
}

test_that("an evaluation request evaluates a block edited while dormant", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- select_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")
      expect_equal(nrow(block_conditions(rv, "r", "error")), 0L)

      park_blocks(vis, "r")
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "dormant")

      reset_probes()

      # Break r by editing r itself. Nothing upstream changed, so it is not
      # stale, and its conditions still report the last (clean) run.
      board_update(edit_col("nope"))
      session$flushReact()

      expect_false(evaluated("r"))
      expect_identical(rv$eval[["r"]](), "dormant")
      expect_equal(nrow(block_conditions(rv, "r", "error")), 0L)

      board_update(list(evaluate = "r"))
      session$flushReact()

      # The request runs r off screen: the error it now raises is reported,
      # and r drops back out of the eval set.
      expect_true(evaluated("r"))
      expect_equal(nrow(block_conditions(rv, "r", "error")), 1L)

      expect_identical(rv$eval[["r"]](), "dormant")
      expect_length(rv$evaluating(), 0L)
      expect_false(rendered("r"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "s", "r")
        render_blocks(visibility, "s", "r")
      }
    )
  )
})

test_that("an edit and a request in one payload evaluate the edit", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- select_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      park_blocks(vis, "r")
      session$flushReact()

      reset_probes()

      # Requests apply after the state delta, so the block evaluates what the
      # same payload just made of it, not what it replaced.
      board_update(c(edit_col("nope"), list(evaluate = "r")))
      session$flushReact()

      expect_true(evaluated("r"))
      expect_equal(nrow(block_conditions(rv, "r", "error")), 1L)

      # Repairing it the same way clears the report again.
      board_update(c(edit_col("Time"), list(evaluate = "r")))
      session$flushReact()

      expect_equal(nrow(block_conditions(rv, "r", "error")), 0L)
      expect_identical(rv$eval[["r"]](), "dormant")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "s", "r")
        render_blocks(visibility, "s", "r")
      }
    )
  )
})

test_that("an evaluation request builds the blocks it needs", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = Inf)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      a = with_id(probe_passthrough(), "a"),
      r = with_id(probe_passthrough(), "r")
    ),
    links = links(
      sa = new_link("s", "a", "data"),
      ar = new_link("a", "r", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(constructed("s"))
      expect_false(constructed("a"))
      expect_false(constructed("r"))

      # An unbuilt block holds the request open until it has been built and has
      # run, rather than the request being spent on a block that cannot report.
      board_update(list(evaluate = "r"))
      session$flushReact()

      expect_true(constructed("a"))
      expect_true(constructed("r"))

      expect_true(evaluated("r"))
      expect_identical(rv$eval[["r"]](), "dormant")
      expect_length(rv$evaluating(), 0L)
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "s")
        render_blocks(visibility, "s")
      }
    )
  )
})

test_that("a required claim holds a block until it is released", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      r = with_id(probe_passthrough(), "r")
    ),
    links = links(sr = new_link("s", "r", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      park_blocks(vis, "r")
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "dormant")

      reset_probes()

      # A claim, unlike a one-off request, survives evaluation.
      board_update(list(require = list(add = "r")))
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

      for (i in 1:3) session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")
      expect_setequal(rv$required_blocks(), "r")

      # Releasing it hands the block back to the front-end's gating, which
      # parked it.
      board_update(list(require = list(rm = "r")))
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "dormant")
      expect_length(rv$required_blocks(), 0L)
      expect_false(rendered("r"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "s", "r")
        render_blocks(visibility, "s", "r")
      }
    )
  )
})

test_that("a request for a block added in the same payload is honoured", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(s = with_id(probe_source(), "s")),
    links = links()
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      new <- as_blocks(list(r = with_id(probe_passthrough(), "r")))

      board_update(
        list(
          blocks = list(add = new),
          links = list(add = links(sr = new_link("s", "r", "data"))),
          evaluate = "r"
        )
      )
      session$flushReact()

      expect_true(evaluated("r"))
      expect_length(rv$evaluating(), 0L)
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "s")
        render_blocks(visibility, "s")
      }
    )
  )
})

test_that("a request naming an unknown block is rejected", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(a = with_id(probe_source(), "a")),
    links = links()
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      board_update(list(evaluate = "nope"))
      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_identical(rv$last_update$phase, "validate")
      expect_length(rv$evaluating(), 0L)

      board_update(list(require = list(add = "nope")))
      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_length(rv$required_blocks(), 0L)

      expect_setequal(rv$needed(), "a")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "a")
        render_blocks(visibility, "a")
      }
    )
  )
})

test_that("a view switch does not re-evaluate shared upstream left needed", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      src = with_id(probe_source(), "src"),
      mid = with_id(probe_passthrough(), "mid"),
      t1 = with_id(probe_passthrough(), "t1"),
      t2 = with_id(probe_passthrough(), "t2")
    ),
    links = links(
      new_link("src", "mid", "data"),
      new_link("mid", "t1", "data"),
      new_link("mid", "t2", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("src"))
      expect_true(evaluated("mid"))
      expect_true(evaluated("t1"))

      reset_probes()

      # Switch to the sibling view: t1 leaves the needed set and t2 enters, but
      # the shared upstream (src, mid) stays needed throughout. Only the newly
      # visible leaf evaluates -- the upstream slots never flip, so nothing
      # pulls the shared pipeline again.
      vis$required[["t1"]](FALSE)
      vis$required[["t2"]](TRUE)
      render_blocks(vis, "t2")
      session$flushReact()

      expect_true(evaluated("t2"))
      expect_false(evaluated("src"))
      expect_false(evaluated("mid"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "t1")
        render_blocks(visibility, "t1")
      }
    )
  )
})

test_that("a variadic block skips re-evaluation on unchanged inputs", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_source_alt(), "b"),
      v = with_id(probe_variadic(), "v")
    ),
    links = links(new_link("a", "v", "1"), new_link("b", "v", "2"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_true(evaluated("a"))
      expect_true(evaluated("b"))
      expect_true(evaluated("v"))

      reset_probes()

      # A variadic block's `...args` are repackaged into a fresh list on every
      # pull, but the element objects are the cached upstream results. Park the
      # block across separate flushes and bring it back: the by-reference skip
      # sees the same objects and nothing re-evaluates.
      vis$required[["v"]](FALSE)
      session$flushReact()

      vis$required[["v"]](TRUE)
      session$flushReact()

      expect_false(evaluated("a"))
      expect_false(evaluated("b"))
      expect_false(evaluated("v"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        require_blocks(visibility, "v")
        render_blocks(visibility, "v")
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

test_that("is_visible is an isTRUE check on the slot value", {

  expect_true(is_visible(TRUE))
  expect_false(is_visible(FALSE))
  expect_false(is_visible(NA))
})

test_that("channel validators enforce the required and visible contracts", {

  expect_true(valid_required(TRUE))
  expect_true(valid_required(FALSE))
  expect_true(valid_required(NA))
  expect_false(valid_required("x"))
  expect_false(valid_required(NA_character_))

  expect_true(valid_visible(TRUE))
  expect_true(valid_visible(FALSE))
  expect_true(valid_visible(NA))
  expect_false(valid_visible("main"))
  expect_false(valid_visible(NA_character_))
  expect_false(valid_visible(c(TRUE, FALSE)))
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
    vis$visible[["a"]]("main")
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
    vis$visible[["a"]](TRUE)
    vis$visible[["b"]](TRUE)

    expect_true(required_fulfilled(vis))

    vis$visible[["b"]](FALSE)
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

test_that("a downstream input recovers an upstream built after it ran", {

  # Regression for the finite background_construction_delay race: an input
  # reactive that runs before its upstream is registered in rv$blocks must
  # re-resolve the server once that upstream is constructed, instead of latching
  # the NULL it first saw. The wake rides the upstream's rv$eval slot, installed
  # at its construction -- the same per-key signal input_ready() depends on.

  latch_probe <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {

        rv <- reactiveValues(blocks = list())
        rv$eval <- reactiveValues()
        rv$needed <- reactiveVal(TRUE)
        rv$needed_slots <- new.env(parent = emptyenv())

        src_rv <- reactiveValues()
        src_rv[["data"]] <- "up"

        input_res <- upstream_result("data", src_rv, rv, to = "down")

        captured <- new.env(parent = emptyenv())
        captured$val <- "unset"

        observe(captured$val <- input_res())
      }
    )
  }

  testServer(
    latch_probe,
    {
      session$flushReact()

      expect_null(captured$val)

      # Build the upstream, mirroring construct_block's install order: rebind
      # rv$blocks, then install the eval slot through a local binding.
      rv$blocks[["up"]] <- list(server = list(result = reactive("UPSTREAM")))
      ev <- isolate(rv$eval)
      ev[["up"]] <- reactive("ready")

      session$flushReact()

      expect_identical(captured$val, "UPSTREAM")
    }
  )
})
