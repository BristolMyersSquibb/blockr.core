library(blockr.core)

options(blockr.background_construction_delay = Inf)

serve(
  new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("ChickWeight")
    )
  ),
  "my_board",
  callbacks = function(board, visibility, update, ...) {

    visibility$gate("front-end")
    update(list(sustain = list(`front-end` = list(set = "a"))))
    visibility$visible[["a"]](TRUE)

    shiny::exportTestValues(
      built = names(board$blocks),
      status_b = reval_if(board$eval[["b"]])
    )

    NULL
  }
)
