probe_render <- new.env()
probe_render$ids <- character()

probe_eval <- new.env()
probe_eval$ids <- character()

probe_args <- new.env()
probe_args$entry_reactive <- NULL
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

# Builds its expression from its input data, as some extension blocks do, so
# the expression cannot be rebuilt while the block is out of the eval set.
probe_data_expr <- function(n = 1L) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          rows <- reactiveVal(n)

          list(
            expr = reactive(
              bquote(utils::head(data, .(min(rows(), nrow(data())))))
            ),
            state = list(n = rows)
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

probe_valid <- new.env()
probe_valid$runs <- 0L

# Validated, with a user input that may be left unset.
probe_validated <- function(n = integer()) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          rows <- reactiveVal(n)

          list(
            expr = reactive(bquote(utils::head(data, .(rows())))),
            state = list(n = rows)
          )
        }
      )
    },
    function(id) shiny::tagList(),
    dat_valid = function(data) {
      probe_valid$runs <- probe_valid$runs + 1L
    },
    class = "probe_block",
    block_metadata = FALSE
  )
}

probe_trigger <- new.env()
probe_trigger$value <- reactiveVal(1L)

registerS3method(
  "block_eval_trigger", "probe_trigger_block",
  function(x, session = get_session()) probe_trigger$value()
)

probe_triggered <- function() {
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
    class = c("probe_trigger_block", "probe_block"),
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
              probe_args$entry_reactive <- lgl_ply(
                ks, function(k) is.reactive(...args[[k]])
              )
              probe_args$entry_classes <- chr_ply(
                ks, function(k) class(...args[[k]]())[1L]
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

# The front-end under test: its callback makes the board lazy by returning its
# opening eager set, and it states demand from then on as an
# `eager` component under that label, exactly as any other consumer would. Each
# payload helper writes the channel once, since a second write before the next
# flush would clobber the first.
front_end <- "front-end"

front_delta <- function(...) {
  set_names(list(list(...)), front_end)
}

declare_eager <- function(...) {
  eager(front_end, c(...))
}

require_blocks <- function(update, ...) {

  update(list(eager = front_delta(add = c(...))))

  invisible()
}

release_blocks <- function(update, ...) {

  update(list(eager = front_delta(rm = c(...))))

  invisible()
}

render_blocks <- function(vis, ...) {

  for (id in c(...)) {
    vis$visible[[id]](TRUE)
  }

  invisible()
}

park_blocks <- function(update, vis, ...) {

  release_blocks(update, ...)

  for (id in c(...)) {
    vis$visible[[id]](FALSE)
  }

  invisible()
}

front_eager <- function(rv) {
  rv$eager_blocks()[[front_end]]
}

# The front-end holds an eager set like any other owner, so a test about what
# consumers hold reads past its entry rather than the whole set.
consumer_eager <- function(rv) {
  held <- rv$eager_blocks()
  held[setdiff(names(held), front_end)]
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

      expect_setequal(front_eager(rv), "b")

      expect_true(evaluated("b"))
      expect_true(rendered("b"))

      expect_true(evaluated("a"))
      expect_false(rendered("a"))

      expect_false(evaluated("c"))
      expect_false(rendered("c"))

      expect_false(evaluated("d"))
      expect_false(rendered("d"))

      require_blocks(board_update, "c", "d")
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
        render_blocks(visibility, "b")
        declare_eager("b")
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
        render_blocks(visibility, "b")
        declare_eager("b")
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
        render_blocks(visibility, "b")
        declare_eager("b")
      }
    )
  )
})

test_that("a needed round trip with unchanged inputs does not re-evaluate", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

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
      release_blocks(board_update, "b")
      session$flushReact()

      # ... and comes back. Nothing upstream changed, so nothing re-evaluates:
      # the unchanged-inputs guard returns the cached results instead of
      # re-running the block expressions.
      require_blocks(board_update, "b")
      session$flushReact()

      expect_false(evaluated("a"))
      expect_false(evaluated("b"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "b")
        declare_eager("b")
      }
    )
  )
})

test_that("a parked block reports stale when an upstream re-evaluates", {

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

      # Park r off-screen: it drops out of the eval set, while a stays eager
      # (its panel is still open). What r's last run found still holds.
      release_blocks(board_update, "r")
      session$flushReact()

      expect_false(block_needed(rv, "r"))
      expect_identical(rv$eval[["r"]](), "ready")

      # Re-route a from s1 to s2 (a different dataset): a re-evaluates to a new
      # result, breaking r's cached input -- but r is parked and never re-runs.
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
        render_blocks(visibility, "a", "r")
        declare_eager("a", "r")
      }
    )
  )
})

test_that("a parked block whose upstreams are unchanged stays current", {

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
      # leave the eval set, and the last result of a survives parking, so r's
      # cached input still matches and r is not stale.
      release_blocks(board_update, "a", "r")
      session$flushReact()

      expect_false(block_needed(rv, "a"))
      expect_false(block_needed(rv, "r"))

      expect_identical(rv$eval[["a"]](), "ready")
      expect_identical(rv$eval[["r"]](), "ready")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "a", "r")
        declare_eager("a", "r")
      }
    )
  )
})

test_that("staleness propagates to the whole parked downstream cone", {

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
      release_blocks(board_update, "b", "r")
      session$flushReact()

      expect_identical(rv$eval[["b"]](), "ready")
      expect_identical(rv$eval[["r"]](), "ready")

      # Re-route a to a new dataset: a re-evaluates. b (a's direct downstream)
      # is stale from the changed input; r is stale transitively via b, even
      # though b -- being parked -- never re-evaluated.
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
        render_blocks(visibility, "a", "b", "r")
        declare_eager("a", "b", "r")
      }
    )
  )
})

test_that("re-routing a parked block's input marks it stale", {

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

      # Park r; a and b stay eager (both ready).
      release_blocks(board_update, "r")
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

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
        render_blocks(visibility, "a", "b", "r")
        declare_eager("a", "b", "r")
      }
    )
  )
})

test_that("a stale block that re-evaluates is current when parked again", {

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

      release_blocks(board_update, "r")
      session$flushReact()

      board_update(
        list(
          links = list(rm = "ar", add = links(br = new_link("b", "r", "data")))
        )
      )
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "stale")

      # Put r back on screen: it evaluates against its new input and is current
      # again, so parking it a second time leaves it reading ready. Neither b's
      # result nor its status changed in between, so the verdict has to be
      # recomputed off r's own last evaluation.
      require_blocks(board_update, "r")
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

      release_blocks(board_update, "r")
      session$flushReact()

      expect_false(block_needed(rv, "r"))
      expect_identical(rv$eval[["r"]](), "ready")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "a", "b", "r")
        declare_eager("a", "b", "r")
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
      park_blocks(board_update, vis, "a", "r")
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
      # and both are current afterwards -- reported as ready, not stale, while
      # parked again.
      expect_true(evaluated("a"))
      expect_true(evaluated("r"))

      expect_false(block_needed(rv, "a"))
      expect_false(block_needed(rv, "r"))

      expect_identical(rv$eval[["a"]](), "ready")
      expect_identical(rv$eval[["r"]](), "ready")

      # The request is spent, and nothing about what is on screen changed.
      expect_length(rv$evaluating(), 0L)

      expect_false("r" %in% front_eager(rv))
      expect_false(vis$visible[["r"]]())
      expect_false(rendered("r"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, update, ...) {
        upd_channel <<- update
        render_blocks(visibility, "s1", "s2", "a", "r")
        declare_eager("s1", "s2", "a", "r")
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

test_that("an evaluation request evaluates a block edited while parked", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- select_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")
      expect_equal(nrow(block_conditions(rv, "r", "error")), 0L)

      park_blocks(board_update, vis, "r")
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")

      reset_probes()

      # Break r by editing r itself. It does not re-run, so its conditions
      # still report the last (clean) run, but it reads stale: that run did
      # not see the edit.
      board_update(edit_col("nope"))
      session$flushReact()

      expect_false(evaluated("r"))
      expect_identical(rv$eval[["r"]](), "stale")
      expect_equal(nrow(block_conditions(rv, "r", "error")), 0L)

      board_update(list(evaluate = "r"))
      session$flushReact()

      # The request runs r off screen: the error it now raises is reported,
      # and r drops back out of the eval set still reading failed.
      expect_true(evaluated("r"))
      expect_equal(nrow(block_conditions(rv, "r", "error")), 1L)

      expect_false(block_needed(rv, "r"))
      expect_identical(rv$eval[["r"]](), "failed")
      expect_length(rv$evaluating(), 0L)
      expect_false(rendered("r"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s", "r")
        declare_eager("s", "r")
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

      park_blocks(board_update, vis, "r")
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
      expect_identical(rv$eval[["r"]](), "ready")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s", "r")
        declare_eager("s", "r")
      }
    )
  )
})

test_that("an edit to a parked block marks its downstream cone stale", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      r = with_id(probe_select(), "r"),
      d = with_id(probe_passthrough(), "d")
    ),
    links = links(
      sr = new_link("s", "r", "data"),
      rd = new_link("r", "d", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      park_blocks(board_update, vis, "r", "d")
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "ready")
      expect_identical(rv$eval[["d"]](), "ready")

      reset_probes()

      board_update(edit_col("Time"))
      session$flushReact()

      # Neither re-runs, and nothing upstream of r changed, but d consumed what
      # r's last run produced, which the edit has overtaken.
      expect_false(evaluated("r"))
      expect_false(evaluated("d"))

      expect_identical(rv$eval[["r"]](), "stale")
      expect_identical(rv$eval[["d"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s", "r", "d")
        declare_eager("s", "r", "d")
      }
    )
  )
})

test_that("a block that was never needed is unevaluated until it runs", {

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

      # Built by the background pass, but never needed, so there is no run
      # for either to be current with.
      expect_true(constructed("r"))
      expect_false(evaluated("a"))
      expect_false(evaluated("r"))

      expect_identical(rv$eval[["a"]](), "unevaluated")
      expect_identical(rv$eval[["r"]](), "unevaluated")

      # Declared parked, which changes nothing about either block, but re-runs
      # the eval-set observer, so a request is read before it joins the eval
      # set. It must be held open then, not dropped as though r had run.
      park_blocks(board_update, vis, "a", "r")
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "unevaluated")

      board_update(list(evaluate = "r"))
      session$flushReact()

      expect_true(evaluated("a"))
      expect_true(evaluated("r"))

      expect_identical(rv$eval[["a"]](), "ready")
      expect_identical(rv$eval[["r"]](), "ready")
      expect_length(rv$evaluating(), 0L)
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s")
        declare_eager("s")
      }
    )
  )
})

test_that("finding that a block cannot run counts as a check", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      t = with_id(probe_source_alt(), "t"),
      w = with_id(probe_passthrough(), "w"),
      m = new_merge_block()
    ),
    links = links(
      sm = new_link("s", "m", "x"),
      tm = new_link("t", "m", "y")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["w"]](), "waiting")
      expect_identical(rv$eval[["m"]](), "unset")

      park_blocks(board_update, vis, "w", "m")
      session$flushReact()

      # Neither has ever run, but each was checked and found unable to, which
      # still holds.
      expect_false(evaluated("w"))

      expect_identical(rv$eval[["w"]](), "waiting")
      expect_identical(rv$eval[["m"]](), "unset")

      # A request settles on that same verdict rather than holding out for a
      # run that cannot happen.
      board_update(list(evaluate = "w"))
      session$flushReact()

      expect_length(rv$evaluating(), 0L)
      expect_identical(rv$eval[["w"]](), "waiting")

      # Connecting the missing input is a change the check did not see.
      board_update(
        list(links = list(add = links(sw = new_link("s", "w", "data"))))
      )
      session$flushReact()

      expect_false(evaluated("w"))
      expect_identical(rv$eval[["w"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s", "t", "w", "m")
        declare_eager("s", "t", "w", "m")
      }
    )
  )
})

test_that("checking a block with an unset user input does not validate it", {

  reset_probes()

  probe_valid$runs <- 0L

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      v = with_id(probe_validated(), "v")
    ),
    links = links(sv = new_link("s", "v", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["v"]](), "unset")
      expect_identical(probe_valid$runs, 0L)
    },
    args = list(x = board, plugins = list())
  )
})

test_that("rewiring a parked block marks it stale", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_source_alt(), "b"),
      r1 = with_id(probe_passthrough(), "r1"),
      r2 = with_id(probe_passthrough(), "r2")
    ),
    links = links(
      ar1 = new_link("a", "r1", "data"),
      ar2 = new_link("a", "r2", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      park_blocks(board_update, vis, "b", "r1", "r2")
      session$flushReact()

      expect_identical(rv$eval[["b"]](), "ready")
      expect_identical(rv$eval[["r1"]](), "ready")
      expect_identical(rv$eval[["r2"]](), "ready")

      reset_probes()

      # One loses its input, the other is fed by a block that is itself parked,
      # so has no fresh result to compare. Neither is what the last run saw.
      board_update(
        list(
          links = list(
            rm = c("ar1", "ar2"),
            add = links(br2 = new_link("b", "r2", "data"))
          )
        )
      )
      session$flushReact()

      expect_false(evaluated("r1"))
      expect_false(evaluated("r2"))

      expect_identical(rv$eval[["r1"]](), "stale")
      expect_identical(rv$eval[["r2"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "a", "b", "r1", "r2")
        declare_eager("a", "b", "r1", "r2")
      }
    )
  )
})

test_that("a block stays stale once its changed upstream is parked", {

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
      s1a = new_link("s1", "a", "data"),
      ar = new_link("a", "r", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      park_blocks(board_update, vis, "r")
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

      expect_identical(rv$eval[["r"]](), "stale")

      # Parking a leaves it current, but r has still not seen what a now holds.
      park_blocks(board_update, vis, "a")
      session$flushReact()

      expect_identical(rv$eval[["a"]](), "ready")
      expect_identical(rv$eval[["r"]](), "stale")

      reset_probes()

      board_update(list(evaluate = "r"))
      session$flushReact()

      expect_true(evaluated("r"))
      expect_identical(rv$eval[["r"]](), "ready")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s1", "s2", "a", "r")
        declare_eager("s1", "s2", "a", "r")
      }
    )
  )
})

test_that("a request for an upstream alone leaves its downstream stale", {

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
      s1a = new_link("s1", "a", "data"),
      ar = new_link("a", "r", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      park_blocks(board_update, vis, "a", "r")
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

      board_update(list(evaluate = "a"))
      session$flushReact()

      expect_true(evaluated("a"))
      expect_false(evaluated("r"))

      expect_identical(rv$eval[["a"]](), "ready")
      expect_identical(rv$eval[["r"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s1", "s2", "a", "r")
        declare_eager("s1", "s2", "a", "r")
      }
    )
  )
})

test_that("a block built from its input data is compared on its state", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      p = with_id(probe_data_expr(), "p")
    ),
    links = links(sp = new_link("s", "p", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["p"]](), "ready")

      # Parked, p's expression cannot be rebuilt, as its data is withheld.
      # That is no edit, so p still reads ready, and a request leaves it there.
      park_blocks(board_update, vis, "p")
      session$flushReact()

      expect_identical(rv$eval[["p"]](), "ready")

      board_update(list(evaluate = "p"))
      session$flushReact()

      expect_length(rv$evaluating(), 0L)
      expect_identical(rv$eval[["p"]](), "ready")

      reset_probes()

      board_update(list(blocks = list(mod = list(p = list(n = 2L)))))
      session$flushReact()

      expect_false(evaluated("p"))
      expect_identical(rv$eval[["p"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s", "p")
        declare_eager("s", "p")
      }
    )
  )
})

test_that("an eval trigger that moves while a block is parked marks it stale", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  probe_trigger$value(1L)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      t = with_id(probe_triggered(), "t")
    ),
    links = links(st = new_link("s", "t", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      park_blocks(board_update, vis, "t")
      session$flushReact()

      expect_identical(rv$eval[["t"]](), "ready")

      probe_trigger$value(2L)
      session$flushReact()

      expect_identical(rv$eval[["t"]](), "stale")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s", "t")
        declare_eager("s", "t")
      }
    )
  )
})

test_that("a block checked off screen reports why it cannot run", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      w = with_id(probe_passthrough(), "w")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval[["w"]](), "unevaluated")

      # Never on screen, so nothing renders it: the reason comes from the check
      # the request runs, and stays once w is parked again.
      board_update(list(evaluate = "w"))
      session$flushReact()

      expect_length(rv$evaluating(), 0L)
      expect_false(block_needed(rv, "w"))
      expect_identical(rv$eval[["w"]](), "waiting")

      reason <- block_conditions(rv, "w", "warning")

      expect_identical(reason$phase, "status")
      expect_match(reason$message, "waiting for its data input")

      # Connected and run off screen, it can run, which clears the reason.
      board_update(
        list(
          links = list(add = links(sw = new_link("s", "w", "data"))),
          evaluate = "w"
        )
      )
      session$flushReact()

      expect_true(evaluated("w"))
      expect_false(rendered("w"))

      expect_identical(rv$eval[["w"]](), "ready")
      expect_equal(nrow(block_conditions(rv, "w", "warning")), 0L)
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s")
        declare_eager("s")
      }
    )
  )
})

test_that("a parked block returns the result its last check left", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s1 = with_id(probe_source(), "s1"),
      s2 = with_id(probe_source_alt(), "s2"),
      r = with_id(probe_passthrough(), "r"),
      w = with_id(probe_passthrough(), "w")
    ),
    links = links(s1r = new_link("s1", "r", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      held <- rv$blocks[["r"]]$server$result()

      expect_identical(held, datasets::BOD)

      park_blocks(board_update, vis, "r", "w")
      session$flushReact()

      reset_probes()

      # Reading it runs nothing: it is what the status reports on.
      expect_identical(rv$eval[["r"]](), "ready")
      expect_identical(rv$blocks[["r"]]$server$result(), held)
      expect_false(evaluated("r"))

      # Once stale it still returns what it last found, while its status says
      # that is out of date.
      board_update(
        list(
          links = list(
            rm = "s1r",
            add = links(s2r = new_link("s2", "r", "data"))
          )
        )
      )
      session$flushReact()

      expect_identical(rv$eval[["r"]](), "stale")
      expect_identical(rv$blocks[["r"]]$server$result(), held)
      expect_false(evaluated("r"))

      # A block last found unable to run holds no result.
      expect_identical(rv$eval[["w"]](), "waiting")
      expect_null(rv$blocks[["w"]]$server$result())
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s1", "s2", "r", "w")
        declare_eager("s1", "s2", "r", "w")
      }
    )
  )
})

test_that("a block that is not built yet reads unevaluated", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = Inf)

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
      seen <- new.env()
      seen$status <- character()

      observe(
        seen$status <- c(seen$status, reval_if(rv$eval[["r"]]))
      )

      session$flushReact()

      expect_false(constructed("r"))
      expect_identical(seen$status, "unevaluated")

      # Building the block replaces the placeholder, which is what wakes a
      # reader of it.
      board_update(list(eager = list(consumer = list(set = "r"))))
      session$flushReact()

      expect_true(constructed("r"))
      expect_identical(seen$status, c("unevaluated", "ready"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s")
        declare_eager("s")
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
      expect_identical(rv$eval[["r"]](), "ready")
      expect_length(rv$evaluating(), 0L)
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s")
        declare_eager("s")
      }
    )
  )
})

test_that("an eager block stays evaluated until it is released", {

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

      park_blocks(board_update, vis, "r")
      session$flushReact()

      expect_false(block_needed(rv, "r"))

      reset_probes()

      # An eager block, unlike a one-off request, survives evaluation.
      board_update(list(eager = list(consumer = list(set = "r"))))
      session$flushReact()

      expect_true(block_needed(rv, "r"))

      for (i in 1:3) session$flushReact()

      expect_true(block_needed(rv, "r"))
      expect_identical(rv$eval[["r"]](), "ready")
      expect_identical(consumer_eager(rv), list(consumer = "r"))

      # Releasing it hands the block back to the front-end's gating, which
      # parked it.
      board_update(list(eager = list(consumer = list(set = character()))))
      session$flushReact()

      expect_false(block_needed(rv, "r"))
      expect_length(consumer_eager(rv), 0L)
      expect_false(rendered("r"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s", "r")
        declare_eager("s", "r")
      }
    )
  )
})

test_that("one owner's release leaves another owner's eager set standing", {

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

      park_blocks(board_update, vis, "r")
      session$flushReact()

      expect_false(block_needed(rv, "r"))

      board_update(list(eager = list(one = list(set = "r"))))
      session$flushReact()

      board_update(list(eager = list(two = list(add = "r"))))
      session$flushReact()

      expect_identical(consumer_eager(rv), list(one = "r", two = "r"))
      expect_true(block_needed(rv, "r"))

      # The block is held by two owners, so the first letting go does not
      # release the second's hold.
      board_update(list(eager = list(one = list(set = character()))))
      session$flushReact()

      expect_identical(consumer_eager(rv), list(two = "r"))
      expect_true(block_needed(rv, "r"))

      # Releasing the last block an owner holds drops the owner, whether it
      # says so with `rm` or by setting an empty set.
      board_update(list(eager = list(two = list(rm = "r"))))
      session$flushReact()

      expect_length(consumer_eager(rv), 0L)
      expect_false(block_needed(rv, "r"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s", "r")
        declare_eager("s", "r")
      }
    )
  )
})

test_that("a consumer's eager block does not make an eager board lazy", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- new_board(
    blocks = c(
      s = with_id(probe_source(), "s"),
      a = with_id(probe_passthrough(), "a"),
      b = with_id(probe_passthrough(), "b")
    ),
    links = links(
      sa = new_link("s", "a", "data"),
      sb = new_link("s", "b", "data")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      board_update(list(eager = list(consumer = list(set = "a"))))
      session$flushReact()

      # No callback made the board lazy, so an eager block changes nothing: it
      # says what one consumer wants evaluated, never that everything else may
      # be parked. Turning the board lazy on it instead would leave b -- which
      # nobody asked for -- unevaluated and blank on a board that has no
      # front-end.
      expect_true(rv$needed())
      expect_identical(rv$eval[["b"]](), "ready")
      expect_true(rendered("b"))
    },
    args = list(x = board, plugins = list())
  )
})

test_that("a consumer cannot release what the front-end holds", {

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

      expect_identical(front_eager(rv), "r")

      # A consumer holds the block the front-end is showing and then lets go.
      # Sharing one channel, its write landed on the front-end's own state and
      # its release took the front-end's demand with it; as one owner among
      # several it can do neither.
      board_update(list(eager = list(consumer = list(set = "r"))))
      session$flushReact()

      board_update(list(eager = list(consumer = list(set = character()))))
      session$flushReact()

      expect_identical(front_eager(rv), "r")
      expect_length(consumer_eager(rv), 0L)
      expect_setequal(rv$needed(), c("s", "r"))
      expect_identical(rv$eval[["r"]](), "ready")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "r")
        declare_eager("r")
      }
    )
  )
})

test_that("removing an eager block prunes it from every owner", {

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

      board_update(
        list(
          eager = list(
            one = list(set = c("s", "r")),
            two = list(set = "r")
          )
        )
      )
      session$flushReact()

      board_update(list(blocks = list(rm = "r")))
      session$flushReact()

      # An owner left holding nothing is dropped, so a stale set cannot
      # outlive the block it named.
      expect_identical(consumer_eager(rv), list(one = "s"))
      expect_setequal(rv$needed(), "s")

      # The owner that lost its block still releases cleanly: `rm` names a
      # block the board no longer has, and that must not reject the payload.
      board_update(list(eager = list(two = list(rm = "r"))))
      session$flushReact()

      expect_true(rv$last_update$ok)
      expect_identical(consumer_eager(rv), list(one = "s"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s")
        declare_eager("s")
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
        render_blocks(visibility, "s")
        declare_eager("s")
      }
    )
  )
})

test_that("a construction request builds a block without evaluating it", {

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
      expect_false(constructed("r"))

      board_update(list(construct = c("s", "r")))
      session$flushReact()

      expect_true(constructed("r"))
      expect_false(evaluated("r"))
      expect_identical(rv$eval[["r"]](), "unevaluated")

      # Construction is not demand: `r` stays out of the eval set, `a` -- which
      # it would need for a result -- stays unbuilt, and `s` is not rebuilt.
      expect_setequal(rv$needed(), "s")
      expect_identical(probe_construct$ids, c("s", "r"))

      # Nor does the request join the eager set the front-end holds, which is
      # what keeps it from parking what is on screen.
      expect_identical(front_eager(rv), "s")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s")
        declare_eager("s")
      }
    )
  )
})

test_that("overlapping requests union rather than clash", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = Inf)

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

      board_update(
        list(
          construct = "r",
          evaluate = "r",
          eager = list(one = list(set = "r"))
        )
      )
      session$flushReact()

      expect_true(rv$last_update$ok)
      expect_true(constructed("r"))
      expect_identical(rv$eval[["r"]](), "ready")
      expect_identical(consumer_eager(rv), list(one = "r"))
      expect_length(rv$evaluating(), 0L)

      # A second consumer cannot know what the first holds, so a one-off over
      # a block someone else holds eager must not be rejected either.
      board_update(list(evaluate = "r"))
      session$flushReact()

      expect_true(rv$last_update$ok)
      expect_identical(consumer_eager(rv), list(one = "r"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "s")
        declare_eager("s")
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

      board_update(list(construct = "nope"))
      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_identical(rv$last_update$phase, "validate")

      board_update(list(eager = list(consumer = list(set = "nope"))))
      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_length(consumer_eager(rv), 0L)

      # An eager set with no owner to release it is refused as well.
      board_update(list(eager = list(list(set = "a"))))
      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_length(consumer_eager(rv), 0L)

      expect_setequal(rv$needed(), "a")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "a")
        declare_eager("a")
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
      board_update(list(eager = front_delta(add = "t2", rm = "t1")))
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
        render_blocks(visibility, "t1")
        declare_eager("t1")
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
      release_blocks(board_update, "v")
      session$flushReact()

      require_blocks(board_update, "v")
      session$flushReact()

      expect_false(evaluated("a"))
      expect_false(evaluated("b"))
      expect_false(evaluated("v"))
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "v")
        declare_eager("v")
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
        render_blocks(visibility, "c")
        declare_eager("c")
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
        render_blocks(visibility, "b")
        declare_eager("b")
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
        render_blocks(visibility, "b")
        declare_eager("b")
      }
    )
  )
})

test_that("a variadic block receives reactives that return its inputs", {

  reset_probes()
  probe_args$entry_reactive <- NULL
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

      expect_identical(probe_args$entry_reactive, c(TRUE, TRUE))
      expect_length(probe_args$entry_classes, 2)
      expect_setequal(probe_args$entry_classes, "data.frame")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(visibility, ...) {
        render_blocks(visibility, "c")
        declare_eager("c")
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
        render_blocks(visibility, "e")
        declare_eager("e")
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
  render_blocks(visibility, "b")
  declare_eager("b")
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
        render_blocks(visibility, "c")
        declare_eager("c")
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

      require_blocks(board_update, "c")
      render_blocks(vis, "c")
      session$flushReact()

      expect_true(constructed("c"))
      expect_false(constructed("d"))
    },
    args = list(
      x = ordered_board(),
      plugins = list(),
      callbacks = function(...) {
        declare_eager("b")
      }
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

      require_blocks(board_update, "c")
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

test_that("channel validators enforce the gate and visible contracts", {

  expect_true(valid_gate(NULL))
  expect_true(valid_gate("dock"))
  expect_false(valid_gate(NA_character_))
  expect_false(valid_gate(""))
  expect_false(valid_gate(c("a", "b")))
  expect_false(valid_gate(TRUE))

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
      gate = reactiveVal(NULL),
      visible = new.env(parent = emptyenv())
    )
    add_vis_slots(vis, "a")

    vis$gate(1L)
    expect_error(validate_vis(vis), class = "invalid_gate")

    vis$gate("dock")
    vis$visible[["a"]]("main")
    expect_error(validate_vis(vis), class = "invalid_visible")
  })
})

test_that("a board turns lazy on the declaration, not on an eager block", {

  isolate({
    vis <- list(gate = reactiveVal(NULL))

    expect_false(gating_active(vis))

    vis$gate("dock")
    expect_true(gating_active(vis))

    withr::local_options(blockr.gate_visibility = FALSE)
    expect_false(gating_active(vis))
  })
})

test_that("gate_fulfilled tracks the front-end's eager set alone", {

  isolate({
    vis <- list(
      gate = reactiveVal("dock"),
      visible = new.env(parent = emptyenv())
    )
    add_vis_slots(vis, c("a", "b", "c"))

    rv <- reactiveValues(eager_blocks = reactiveVal(list(dock = c("a", "b"))))
    vis$visible[["a"]](TRUE)
    vis$visible[["b"]](TRUE)

    expect_true(gate_fulfilled(vis, rv))

    vis$visible[["b"]](FALSE)
    expect_false(gate_fulfilled(vis, rv))

    # Another owner's eager block off screen never lands on screen, so
    # holding the backlog for it would stall it for good.
    vis$visible[["b"]](TRUE)
    rv$eager_blocks(list(dock = c("a", "b"), consumer = "c"))
    expect_true(gate_fulfilled(vis, rv))

    rv$eager_blocks(list())
    expect_true(gate_fulfilled(vis, rv))
  })
})

test_that("a declared eager set is in place before the first flush", {

  reset_probes()

  local_mocked_bindings(schedule_construction = drive_construction)

  eager_at_first_flush <- NULL

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      # Seeded as the callbacks run, so the first construction pass already
      # has it: only the eager set and its upstream are built ahead of the
      # backlog, and nothing outside it evaluates.
      expect_identical(eager_at_first_flush, list(`front-end` = "b"))
      expect_identical(probe_construct$ids[1:2], c("a", "b"))

      expect_true(evaluated("b"))
      expect_false(evaluated("c"))
      expect_false(evaluated("d"))
    },
    args = list(
      x = ordered_board(),
      plugins = list(),
      callbacks = list(
        function(visibility, ...) {
          render_blocks(visibility, "b")
          declare_eager("b")
        },
        function(board, ...) {
          observe(eager_at_first_flush <<- board$eager_blocks(), priority = Inf)
          NULL
        }
      )
    )
  )
})

test_that("a declaration travels alongside a callback's plugin arguments", {

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      expect_identical(rv$eager_blocks(), list(`front-end` = "b"))

      expect_identical(session$returned$extra, 42)
      expect_false(any(c("owner", "blocks") %in% names(session$returned)))
      expect_false(any(lgl_ply(session$returned, is_eager_blocks)))
    },
    args = list(
      x = ordered_board(),
      plugins = list(),
      callbacks = function(...) list(extra = 42, declare_eager("b")),
      callback_location = "start"
    )
  )
})

test_that("callbacks cannot write the gate, only declare it", {

  seen <- NULL

  testServer(
    get_s3_method("board_server", ordered_board()),
    {
      session$flushReact()

      expect_setequal(seen, c("visible", "frozen"))
      expect_false(gating_active(vis))
    },
    args = list(
      x = ordered_board(),
      plugins = list(),
      callbacks = function(visibility, ...) {
        seen <<- names(visibility)
        NULL
      }
    )
  )
})

test_that("at most one callback declares itself the gating front-end", {

  expect_error(
    testServer(
      get_s3_method("board_server", ordered_board()),
      session$flushReact(),
      args = list(
        x = ordered_board(),
        plugins = list(),
        callbacks = list(
          function(...) eager("one", "b"),
          function(...) eager("two", "c")
        )
      )
    ),
    class = "eager_declaration_ambiguous"
  )
})

test_that("a declared eager set is validated as any eager delta is", {

  expect_error(
    testServer(
      get_s3_method("board_server", ordered_board()),
      session$flushReact(),
      args = list(
        x = ordered_board(),
        plugins = list(),
        callbacks = function(...) eager("front-end", "nope")
      )
    ),
    class = "board_update_eager_unknown_id"
  )

  expect_error(eager(""), class = "eager_owner_invalid")
  expect_error(eager(NA_character_), class = "eager_owner_invalid")
  expect_error(eager("fe", 1L), class = "eager_blocks_invalid")
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
      callbacks = function(...) {
        declare_eager("b")
      }
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
        rv$eval <- reactives()
        rv$needed <- reactiveVal(TRUE)
        rv$needed_slots <- reactive_vals()

        src_rv <- reactive_vals(data = "up")

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

stacked_board <- function() {
  new_board(
    blocks = c(
      a = with_id(probe_source(), "a"),
      b = with_id(probe_passthrough(), "b"),
      c = with_id(probe_source(), "c"),
      d = with_id(probe_passthrough(), "d"),
      e = with_id(probe_source(), "e")
    ),
    links = links(
      new_link(from = "a", to = "b"),
      new_link(from = "c", to = "d")
    ),
    stacks = list(s1 = c("a", "b"), s2 = c("c", "d"))
  )
}

# Mirrors what bslib's accordion input reports: the panel values of the open
# stacks, and NULL rather than an empty vector once none are open.
# What core's own stack-gating callback holds eager, like any other owner,
# under the label gate_stacks() takes from the board session.
stack_eager <- function(rv, session) {
  rv$eager_blocks()[[stack_gate_owner(session)]]
}

report_open_stacks <- function(session, ...) {

  ids <- c(...)

  session$setInputs(
    stacks = if (length(ids)) chr_ply(paste0("stack_", ids), session$ns)
  )
}

test_that("core requires the open stacks and every unstacked block", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- stacked_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      # Declared from what core renders open, before the client has reported
      # anything: with no gate in place every block is needed, and the
      # collapsed stack's would evaluate once in that window.
      expect_true(gating_active(vis))
      expect_setequal(stack_eager(rv, session), c("a", "b", "e"))

      expect_false(evaluated("c"))
      expect_false(rendered("c"))

      report_open_stacks(session, "s1")
      session$flushReact()

      expect_setequal(stack_eager(rv, session), c("a", "b", "e"))
      expect_setequal(rv$needed(), c("a", "b", "e"))

      expect_true(block_visible("b", vis))
      expect_false(block_visible("c", vis))
      expect_false(block_visible("d", vis))

      # Paint is the client's to report, so the open stack's blocks run only
      # once it has; the collapsed stack's never do.
      expect_true(evaluated("b"))
      expect_true(rendered("b"))

      expect_false(evaluated("c"))
      expect_false(rendered("c"))

      expect_false(evaluated("d"))
      expect_false(rendered("d"))
    },
    args = list(x = board, plugins = list())
  )
})

test_that("expanding a stack requires its blocks and collapsing parks them", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- stacked_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      report_open_stacks(session, "s1", "s2")
      session$flushReact()

      expect_setequal(
        stack_eager(rv, session),
        c("a", "b", "c", "d", "e")
      )

      expect_true(evaluated("c"))
      expect_true(rendered("d"))

      report_open_stacks(session, "s2")
      session$flushReact()

      expect_setequal(stack_eager(rv, session), c("c", "d", "e"))
      expect_setequal(rv$needed(), c("c", "d", "e"))

      # Parked rather than dropped: still built, so re-expanding shows them
      # without a rebuild.
      expect_setequal(names(rv$blocks), board_block_ids(rv$board))
      expect_true(constructed("a"))

      expect_false(block_visible("a", vis))
      expect_false(block_visible("b", vis))
    },
    args = list(x = board, plugins = list())
  )
})

test_that("a parked block stays quiescent when its result is read", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- stacked_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      report_open_stacks(session, "s1")
      session$flushReact()

      reset_probes()

      # The needed set otherwise reaches a block through its data reads, which
      # a source block has none of: reading `c` evaluated it, where `d` settled
      # on NULL through its unfulfilled inputs. Neither has been checked, so
      # neither holds a result.
      expect_null(isolate(rv$blocks[["c"]]$server$result()))
      expect_null(isolate(rv$blocks[["d"]]$server$result()))

      expect_false(evaluated("c"))
      expect_false(evaluated("d"))
    },
    args = list(x = board, plugins = list())
  )
})

test_that("a fully collapsed accordion is not read as one yet to report", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- stacked_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      # Both states read as a NULL input: the accordion yet to report, and the
      # user having collapsed everything. What separates them is that the
      # first stands on what core rendered open.
      expect_setequal(stack_eager(rv, session), c("a", "b", "e"))

      report_open_stacks(session)
      session$flushReact()

      expect_setequal(stack_eager(rv, session), "e")
      expect_setequal(rv$needed(), "e")
    },
    args = list(x = board, plugins = list())
  )
})

test_that("a board without stacks requires every block", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

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

      # An accordion with no panels binds no input, so there would be nothing
      # to refine a declaration made here -- it is left ungated instead, which
      # costs nothing on a board where every block is unstacked anyway.
      expect_false(gating_active(vis))
      expect_true(rv$needed())

      report_open_stacks(session)
      session$flushReact()

      expect_setequal(stack_eager(rv, session), c("a", "b"))

      expect_true(evaluated("b"))
      expect_true(rendered("b"))
    },
    args = list(x = board, plugins = list())
  )
})

test_that("the gate_visibility option disables stack gating", {

  reset_probes()

  withr::local_options(
    blockr.gate_visibility = FALSE,
    blockr.background_construction_delay = 0
  )

  board <- stacked_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      report_open_stacks(session, "s1")
      session$flushReact()

      expect_false(gating_active(vis))
      expect_true(rv$needed())

      for (id in c("a", "b", "c", "d", "e")) {
        expect_true(evaluated(id))
        expect_true(rendered(id))
      }
    },
    args = list(x = board, plugins = list())
  )
})

test_that("collapsing a stack parks its blocks in the browser", {

  skip_on_cran()

  app_path <- system.file("examples", "board", "gate", "app.R",
                          package = "blockr.core")

  app <- try(
    shinytest2::AppDriver$new(
      app_path,
      name = "gate",
      seed = 42,
      load_timeout = 30 * 1000
    )
  )

  testthat::skip_if(
    inherits(app, "try-error"),
    "Cannot start shinytest2 stack gating app."
  )

  on.exit(app$stop())

  # The collapsed stack's data block never runs, not even once: what core
  # renders open is declared before the first flush rather than waited for.
  expect_identical(
    app$get_value(export = "my_board-evaluated"),
    "datasets::BOD"
  )

  # What bslib reports is the panel `data-value`, which core rebuilds from the
  # stack IDs it holds -- a round trip no mock session exercises.
  expect_identical(app$get_value(export = "my_board-status_b"), "ready")
  expect_identical(app$get_value(export = "my_board-status_d"), "unevaluated")

  expect_identical(app$get_text("#my_board-block_d-result"), "")

  app$click(
    selector = "#stack-accordion-item-my_board-stack_s2 .accordion-button"
  )

  expect_identical(
    app$wait_for_value(
      export = "my_board-status_d",
      ignore = list("unevaluated")
    ),
    "ready"
  )

  expect_match(app$get_text("#my_board-block_d-result"), "Chick", fixed = TRUE)

  app$click(
    selector = "#stack-accordion-item-my_board-stack_s1 .accordion-button"
  )

  # Collapsing s1 parks its blocks, and b goes on reporting what its last run
  # found rather than that nothing needs it.
  expect_identical(
    app$wait_for_value(export = "my_board-needed", ignore = list("a b c d")),
    "c d"
  )

  expect_identical(app$get_value(export = "my_board-status_b"), "ready")
})

test_that("a front-end's own callbacks displace core's stack tracking", {

  reset_probes()

  withr::local_options(blockr.background_construction_delay = 0)

  board <- stacked_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      # The board renders stacks and the option is on, yet core tracks nothing:
      # what gates is whatever the supplied callbacks do.
      expect_false(gating_active(vis))
      expect_true(rv$needed())

      for (id in c("a", "b", "c", "d", "e")) {
        expect_true(evaluated(id))
      }
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(...) NULL
    )
  )
})
