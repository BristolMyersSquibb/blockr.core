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
      class = "identity_block",
      block_metadata = list()
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
