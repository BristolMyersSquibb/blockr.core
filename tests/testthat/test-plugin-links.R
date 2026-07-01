test_that("add/rm links", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    )
  )

  testServer(
    manage_links_server,
    {
      expect_null(session$returned)
      expect_null(update())

      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())
      expect_null(upd$edit)

      expect_null(update())

      session$setInputs(add_link = 1)

      expect_s3_class(upd$add, "links")
      expect_length(upd$add, 1L)

      lnk <- names(upd$add)
      expect_identical(upd$add, as_links(set_names(list(new_link()), lnk)))

      session$setInputs(add_link = 2)

      expect_identical(upd$add, as_links(set_names(list(new_link()), lnk)))

      upd$edit <- list(row = names(upd$add), col = "from", val = "a")
      session$flushReact()
      expect_identical(upd$add, as_links(set_names(list(new_link("a")), lnk)))

      upd$edit <- list(row = names(upd$add), col = "to", val = "b")
      session$flushReact()
      expect_identical(
        upd$add,
        as_links(set_names(list(new_link("a", "b")), lnk))
      )

      upd$edit <- list(row = names(upd$add), col = "input", val = "data")
      session$flushReact()
      expect_identical(
        upd$add,
        as_links(set_names(list(new_link("a", "b", "data")), lnk))
      )

      session$setInputs(modify_links = 1)

      expect_identical(
        update()$links,
        list(
          add = as_links(set_names(list(new_link("a", "b", "data")), lnk)),
          rm = NULL
        )
      )

      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )

  testServer(
    manage_links_server,
    {
      expect_null(upd$edit)
      expect_length(upd$add, 0L)

      session$flushReact()

      expect_length(upd$obs, 1L)
      expect_named(upd$obs, "ac")
      expect_type(upd$obs, "list")

      expect_length(upd$obs[["ac"]], 3L)
      expect_named(upd$obs[["ac"]], c("from", "to", "input"))
      expect_type(upd$obs[["ac"]], "list")

      for (i in c("from", "to", "input")) {
        expect_s3_class(upd$obs[["ac"]][[i]], "Observer")
      }

      session$setInputs(ac_input = "data")
      expect_null(upd$edit)
      expect_length(upd$add, 0L)

      session$setInputs(ac_to = "d")
      expect_identical(upd$edit, list(row = "ac", col = "to", val = "d"))
      expect_length(upd$add, 1L)

      session$setInputs(ac_from = "b")
      expect_identical(upd$edit, list(row = "ac", col = "from", val = "b"))
      expect_length(upd$add, 1L)
    },
    args = list(
      board = list(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_dataset_block("mtcars"),
            c = new_subset_block(),
            d = new_subset_block()
          ),
          links = links(ac = new_link(from = "a", to = "c"))
        )
      ),
      update = reactiveVal()
    )
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(aa = new_link(from = "a", to = "b"))
  )

  testServer(
    manage_links_server,
    {
      expect_null(session$returned)
      expect_null(update())

      expect_s3_class(upd$curr, "links")
      expect_length(upd$curr, 1L)
      expect_named(upd$curr, "aa")

      expect_identical(upd$rm, character())

      session$setInputs(links_dt_rows_selected = 1, rm_link = 1)

      expect_identical(upd$rm, "aa")

      session$setInputs(modify_links = 1)

      expect_identical(
        update()$links,
        list(add = NULL, rm = "aa")
      )

      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())
      expect_identical(upd$curr, links())

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("add/rm links return validation", {

  brd <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(ab = new_link(from = "a", to = "b"))
  )

  pay <- list(
    links = list(
      add = links(new_link(from = "a", to = "b", input = "data")),
      rm = "ab"
    )
  )

  expect_identical(
    validate_board_update(pay, brd),
    pay
  )

  expect_error(
    validate_board_update(list(links = "a"), new_board()),
    class = "board_update_component_type_invalid"
  )

  expect_error(
    validate_board_update(list(links = list(abc = NULL)), new_board()),
    class = "board_update_component_components_invalid"
  )

  expect_error(
    validate_board_update(list(links = list(add = "a")), new_board()),
    class = "board_update_add_component_invalid"
  )

  expect_error(
    validate_board_update(
      list(links = list(add = links(a = new_link()))),
      new_board(
        blocks = c(a = new_dataset_block(), b = new_subset_block()),
        links = links(a = new_link("a", "b"))
      )
    ),
    class = "board_update_links_add_invalid"
  )

  expect_error(
    validate_board_update(list(links = list(rm = 1)), new_board()),
    class = "board_update_rm_component_invalid"
  )

  expect_error(
    validate_board_update(list(links = list(rm = "a")), new_board()),
    class = "board_update_links_rm_invalid"
  )
})

test_that("merge_staged_links overlays staged edits on refreshed links", {

  applied <- links(
    l1 = new_link(from = "a", to = "c", input = "1"),
    l2 = new_link(from = "b", to = "c", input = "2")
  )

  expect_identical(
    merge_staged_links(applied, links(), character()),
    applied
  )

  added <- links(x = new_link(from = "", to = "", input = ""))
  expect_identical(
    merge_staged_links(applied, added, character()),
    c(applied, added)
  )

  edited <- links(l1 = new_link(from = "a", to = "c", input = "9"))
  in_place <- applied
  in_place["l1"] <- edited
  expect_identical(merge_staged_links(applied, edited, "l1"), in_place)

  expect_named(merge_staged_links(applied, links(), "l2"), "l1")

  expect_named(
    merge_staged_links(applied["l1"], added, character()),
    c("l1", "x")
  )
})

test_that("manage links keeps staged edits when the board re-emits", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block(),
      c = new_subset_block()
    )
  )

  testServer(
    manage_links_server,
    {
      session$flushReact()

      session$setInputs(add_link = 1)
      session$flushReact()

      row <- names(upd$add)
      expect_length(upd$curr, 1L)
      expect_identical(names(upd$curr), row)

      board$board <- new_board(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block(),
          c = new_subset_block()
        )
      )
      session$flushReact()

      expect_length(upd$curr, 1L)
      expect_identical(names(upd$curr), row)
      expect_identical(upd$add, upd$curr[row])

      upd$edit <- list(row = row, col = "from", val = "a")
      session$flushReact()
      upd$edit <- list(row = row, col = "to", val = "c")
      session$flushReact()
      upd$edit <- list(row = row, col = "input", val = "data")
      session$flushReact()

      session$setInputs(modify_links = 1)

      res <- update()$links

      expect_s3_class(res$add, "links")
      expect_length(res$add, 1L)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("manage links redraws only when the id set changes", {

  redraws <- 0L

  local_mocked_bindings(
    replaceData = function(...) redraws <<- redraws + 1L,
    .package = "DT"
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(aa = new_link(from = "a", to = "b"))
  )

  testServer(
    manage_links_server,
    {
      session$flushReact()

      expect_identical(names(upd$curr), "aa")
      expect_setequal(names(upd$obs), "aa")

      base <- redraws

      upd$curr <- links(aa = new_link(from = "a", to = ""))
      session$flushReact()

      expect_identical(names(upd$curr), "aa")
      expect_identical(redraws, base)
      expect_setequal(names(upd$obs), "aa")

      upd$curr <- links(
        aa = new_link(from = "a", to = ""),
        bb = new_link(from = "b", to = "")
      )
      session$flushReact()

      expect_gt(redraws, base)
      expect_setequal(names(upd$obs), c("aa", "bb"))
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("manage links ignores no-op board re-emits", {

  link_syncs <- 0L

  real_merge_links <- merge_staged_links

  local_mocked_bindings(
    merge_staged_links = function(...) {
      link_syncs <<- link_syncs + 1L
      real_merge_links(...)
    }
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(aa = new_link(from = "a", to = "b"))
  )

  testServer(
    manage_links_server,
    {
      session$flushReact()

      expect_gt(link_syncs, 0L)

      link_base <- link_syncs

      board$board <- new_board(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        ),
        links = links(aa = new_link(from = "a", to = "b"))
      )
      session$flushReact()

      expect_identical(link_syncs, link_base)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("dummy ad/rm link ui test", {
  expect_s3_class(manage_links_ui("link", new_board()), "shiny.tag.list")
  expect_s3_class(links_modal(NS("links")), "shiny.tag")
})

test_that("variadic link inputs offer no positional-integer options", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(ac = new_link("a", "c"), bc = new_link("b", "c"))
  )

  dt <- dt_board_link(board_links(board), NS("x"), board)

  expect_false(grepl("<option[^>]*value=\"[0-9]", dt$Input[[1L]]))
  expect_false(grepl("<option[^>]*value=\"[0-9]", dt$Input[[2L]]))
})

test_that("a named variadic link renders its name in the editor", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(l1 = new_link("a", "c", "left"))
  )

  dt <- dt_board_link(board_links(board), NS("x"), board)

  expect_match(dt$Input[[1L]], "value=\"left\" selected", fixed = TRUE)
})
