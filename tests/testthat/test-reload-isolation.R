# Guards the cross-session reload handoff.
#
# A board restore stages the next board in `serve_obj`, then calls
# session$reload(). The returning session picks the staged board back up. The
# staged board is keyed per reload handoff (`reload-<token>`, where the token
# rides across the boundary in the URL) rather than in a single shared "reload"
# slot, so under one shared process (Posit Connect) an unrelated session can no
# longer read or consume a board staged for someone else.
#
# `staged_board_key()` is the resolution both the request-phase UI and the
# server use: a query carrying a live reload token resolves that token's slot,
# anything else resolves a request-phase preload slot (falling back to the
# initial board).

test_that("a reload board is keyed per token, hidden from other tabs", {

  update_serve_obj(
    "initial",
    new_board(blocks = c(initmark = new_dataset_block("iris")))
  )

  update_serve_obj(
    "reload-owner",
    new_board(blocks = c(restored_a = new_dataset_block("BOD")))
  )

  withr::defer(
    if (is_reloading("reload-owner")) finalize_reload("reload-owner")
  )

  owner     <- list(`__blockr_reload__` = "owner")
  bystander <- list()
  intruder  <- list(`__blockr_reload__` = "intruder")

  resolved <- function(q) board_block_ids(get_serve_obj(staged_board_key(q)))

  expect_identical(staged_board_key(owner), "reload-owner")
  expect_identical(resolved(owner), "restored_a")

  expect_false("restored_a" %in% resolved(bystander))
  expect_false("restored_a" %in% resolved(intruder))
})

test_that("a bystander session does not strand the owner's staged board", {

  update_serve_obj(
    "initial",
    new_board(blocks = c(initmark = new_dataset_block("iris")))
  )

  update_serve_obj(
    "reload-owner",
    new_board(blocks = c(restored_a = new_dataset_block("BOD")))
  )

  withr::defer(
    if (is_reloading("reload-owner")) finalize_reload("reload-owner")
  )

  finalize_reload(staged_board_key(list()))

  expect_true(is_reloading("reload-owner"))
  expect_identical(board_block_ids(get_serve_obj("reload-owner")), "restored_a")
})

test_that("a stale reload token falls through to the request-phase preload", {

  expect_identical(staged_board_key(list()), "preload-")
  expect_identical(
    staged_board_key(list(board_name = "Foo")),
    "preload-board_name=Foo"
  )

  # a token whose slot was already consumed resolves the preload key, not a
  # dead reload slot
  expect_false(is_reloading("reload-gone"))
  expect_identical(
    staged_board_key(list(board_name = "Foo", `__blockr_reload__` = "gone")),
    "preload-board_name=Foo"
  )
})

test_that("the reload token round-trips through the URL and strips cleanly", {

  expect_null(reload_token(list()))
  expect_null(reload_token(list(board_name = "Foo")))
  expect_identical(reload_token(list(`__blockr_reload__` = "tok")), "tok")

  query <- list(board_name = "My Board", `__blockr_reload__` = "tok")
  parsed <- parseQueryString(query_to_string(query))

  expect_identical(parsed$board_name, "My Board")
  expect_identical(reload_token(parsed), "tok")

  query[[reload_query_param]] <- NULL
  expect_false(grepl("__blockr_reload__", query_to_string(query), fixed = TRUE))
  expect_match(query_to_string(query), "board_name=My%20Board", fixed = TRUE)
})

test_that("staging a handoff slot sweeps stale ones, keeps fresh and initial", {

  update_serve_obj(
    "initial",
    new_board(blocks = c(initmark = new_dataset_block("iris")))
  )

  serve_obj[["reload-stale"]] <- list(
    board = new_board(blocks = c(b = new_dataset_block("BOD"))),
    meta = NULL,
    stamp = Sys.time() - 10 * reload_handoff_ttl
  )
  withr::defer(
    if (is_reloading("reload-stale")) finalize_reload("reload-stale")
  )

  # staging a fresh slot triggers the TTL sweep of expired ones
  update_serve_obj(
    "reload-fresh",
    new_board(blocks = c(a = new_dataset_block("BOD")))
  )
  withr::defer(
    if (is_reloading("reload-fresh")) finalize_reload("reload-fresh")
  )

  expect_false(is_reloading("reload-stale"))
  expect_true(is_reloading("reload-fresh"))
  expect_true(is_reloading("initial"))
})

test_that("stage_reload_handoff stages a token slot carrying board and meta", {

  session <- new_mock_session()
  withr::defer(if (!session$isClosed()) session$close())

  board <- new_board(blocks = c(restored = new_dataset_block("BOD")))
  token <- stage_reload_handoff(board, list(url = "?board_name=Foo"), session)

  slot <- paste0("reload-", token)
  withr::defer(if (is_reloading(slot)) finalize_reload(slot))

  expect_true(is_reloading(slot))
  expect_identical(board_block_ids(get_serve_obj(slot)), "restored")
  expect_identical(finalize_reload(slot)$url, "?board_name=Foo")
})

rendered_block_ids <- function(app, ns = "brd") {

  html <- xml2::read_html(app$get_html("body"))
  ids <- xml2::xml_attr(xml2::xml_find_all(html, "//*[@id]"), "id")

  hits <- grep(paste0("^", ns, "-block_[^-]+-"), ids, value = TRUE)

  unique(sub(paste0("^", ns, "-block_([^-]+)-.*$"), "\\1", hits))
}

test_that("a legacy shared 'reload' slot is never read by a fresh session", {

  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("xml2")

  plant <- withr::local_tempfile(fileext = ".cmd")
  withr::local_envvar(REPRO_PLANT = plant)

  owner <- try(
    shinytest2::AppDriver$new(
      test_path("apps", "reload-repro"),
      name = "reload-legacy-owner",
      load_timeout = 30 * 1000
    )
  )

  skip_if(inherits(owner, "try-error"), "Cannot start reload-repro app.")
  withr::defer(owner$stop())

  expect_setequal(rendered_block_ids(owner), "initmark")

  writeLines("BOD", plant)
  Sys.sleep(1)

  bystander <- shinytest2::AppDriver$new(
    owner$get_url(),
    name = "reload-legacy-bystander",
    load_timeout = 30 * 1000
  )

  withr::defer(bystander$stop())

  expect_false("stolen" %in% rendered_block_ids(bystander))
})

test_that("a returning session lands on its own board, a fresh one does not", {

  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("xml2")

  plant <- withr::local_tempfile(fileext = ".cmd")
  withr::local_envvar(REPRO_PLANT = plant)

  app <- try(
    shinytest2::AppDriver$new(
      test_path("apps", "reload-handoff"),
      name = "reload-handoff-app",
      load_timeout = 30 * 1000
    )
  )

  skip_if(inherits(app, "try-error"), "Cannot start reload-handoff app.")
  withr::defer(app$stop())

  expect_setequal(rendered_block_ids(app), "initmark")
  base <- app$get_url()

  # the producer stages a board under this tab's token just before reloading
  writeLines("ownsess", plant)
  Sys.sleep(1)

  # a concurrent fresh session (no token) ignores the staged slot
  fresh <- shinytest2::AppDriver$new(
    base,
    name = "reload-handoff-fresh",
    load_timeout = 30 * 1000
  )
  withr::defer(fresh$stop())
  expect_setequal(rendered_block_ids(fresh), "initmark")

  # the returning session (its token in the URL) lands on its own board
  returning <- shinytest2::AppDriver$new(
    paste0(base, "?__blockr_reload__=ownsess"),
    name = "reload-handoff-return",
    load_timeout = 30 * 1000
  )
  withr::defer(returning$stop())
  expect_setequal(rendered_block_ids(returning), "restored")
})
