# Loading core registers none of its block classes, and an app offers them once
# `serve()` starts it.
# So the tests start with the block registry of an app that offers core's
# classes alone, whatever other packages are available.
fill_block_registry(core_block_rows())
