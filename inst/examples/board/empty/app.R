library(blockr.core)

serve(
  new_board(),
  "my_board",
  plugins = custom_plugins(list(manage_links(), manage_stacks()))
)
