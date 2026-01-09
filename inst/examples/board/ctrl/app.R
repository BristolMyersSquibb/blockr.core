library(blockr.core)

serve(
	new_board(blocks = c(a = new_dataset_block("BOD"))),
	"my_board",
	plugins = custom_plugins(ctrl_block())
)
