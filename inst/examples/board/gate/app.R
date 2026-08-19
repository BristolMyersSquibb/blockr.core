library(blockr.core)

options(blockr.gate_stacks = TRUE, blockr.background_construction_delay = 0)

serve(
  new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_head_block(),
      c = new_dataset_block("ChickWeight"),
      d = new_head_block()
    ),
    links = c(
      ab = new_link("a", "b"),
      cd = new_link("c", "d")
    ),
    stacks = list(s1 = c("a", "b"), s2 = c("c", "d"))
  ),
  "my_board",
  callbacks = function(board, ...) {

    shiny::exportTestValues(
      status_b = reval_if(board$eval[["b"]]),
      status_d = reval_if(board$eval[["d"]])
    )

    NULL
  }
)
