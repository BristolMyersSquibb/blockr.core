library(blockr.core)

serve(
  new_rbind_block(),
  data = list(
    data.frame(a = 1),
    data.frame(a = 2)
  )
)
