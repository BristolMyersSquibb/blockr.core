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

  with_mock_session(
    {
      val <- reactiveVal(
        list(links = list(add = links(from = "a", to = "b"), rm = "ab"))
      )

      rv <- list(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_subset_block()
          ),
          links = links(ab = new_link(from = "a", to = "b"))
        )
      )

      res <- validate_board_update(val, rv)

      expect_s3_class(res$links$add, "links")
      expect_length(res$links$add, 1L)

      expect_type(res$links$rm, "character")
      expect_length(res$links$rm, 1L)
    }
  )

  with_mock_session(
    {
      expect_error(
        validate_board_update(
          reactiveVal(list(links = "a")),
          list()
        ),
        class = "board_update_component_type_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(links = list(abc = NULL))),
          list()
        ),
        class = "board_update_component_components_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(links = list(add = "a"))),
          list(board = new_board())
        ),
        class = "board_update_links_add_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(links = list(add = links(a = new_link())))
          ),
          list(
            board = new_board(
              blocks = c(a = new_dataset_block(), b = new_subset_block()),
              links = links(a = new_link("a", "b"))
            )
          )
        ),
        class = "board_update_links_add_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(links = list(rm = 1))),
          list(board = new_board())
        ),
        class = "board_update_links_rm_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(links = list(rm = "a"))),
          list(board = new_board())
        ),
        class = "board_update_links_rm_invalid"
      )
    }
  )
})

test_that("dummy ad/rm link ui test", {
  expect_s3_class(manage_links_ui("link", new_board()), "shiny.tag.list")
  expect_s3_class(links_modal(NS("links")), "shiny.tag")
})
