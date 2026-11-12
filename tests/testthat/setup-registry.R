# Loading core registers none of its block classes, and an app offers them once
# `serve()` builds its block registry.
# So the tests start with core's classes alone in the block registry of the
# process, as an app script would register them, which an evaluator reads
# outside an app, whatever other packages are available.
list2env(as.list(app_block_registry(core_block_rows())), block_registry)
