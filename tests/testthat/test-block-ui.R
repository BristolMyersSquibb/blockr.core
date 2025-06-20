test_that("dummy block ui test", {

  new_identity_block <- function() {
    new_block(
      function(id, data) {
        moduleServer(
          "expression",
          function(input, output, session) {
            list(
              expr = reactive(quote(identity(data))),
              state = list()
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      class = "identity_block"
    )
  }

  expect_null(
    block_ui("identity_block", new_identity_block())
  )

  expect_s3_class(
    expr_ui("identity_block", new_identity_block()),
    "shiny.tag.list"
  )

  expect_error(
    expr_ui("identity_block", new_identity_block(), abc = 1),
    class = "superfluous_expr_ui_args"
  )
})

test_that("block ui with state", {

  has_option <- function(html) {
    grepl(
      "option",
      chr_ply(
        htmltools::tagQuery(html)$find("select")$selectedTags(),
        as.character
      )
    )
  }

  blk <- new_scatter_block()

  ui_no_state <- expr_ui("block", blk)

  expect_false(any(has_option(ui_no_state)))

  ui_with_state <- expr_ui("block", blk, list(x = "a", y = "b"))

  expect_true(all(has_option(ui_with_state)))
})
