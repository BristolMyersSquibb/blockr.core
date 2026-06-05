library(blockr.core)

# Fixture for the legacy-slot guard (see test-reload-isolation.R).
#
# The old design staged the next board in a single process-global "reload"
# slot that the next session to boot board_server() consumed unconditionally,
# so under one shared process an unrelated session could read a board staged
# for someone else.
#
# Here we plant a board in that legacy shared "reload" slot on behalf of a
# session that never returns: when REPRO_PLANT names an existing file, its
# contents pick a dataset and we stage a board built from it. The harness then
# connects an unrelated session, which must resolve its own board — the
# per-tab handoff never reads the legacy shared slot.

plant_file <- Sys.getenv("REPRO_PLANT", "")
core_ns <- asNamespace("blockr.core")

stage_pending_board <- function() {

  if (!nzchar(plant_file) || !file.exists(plant_file)) {
    return(invisible())
  }

  dataset <- tryCatch(readLines(plant_file, warn = FALSE)[1L],
                      error = function(e) "")
  unlink(plant_file)

  if (length(dataset) && nzchar(dataset)) {
    board <- new_board(blocks = c(stolen = new_dataset_block(dataset)))
    get("update_serve_obj", core_ns)("reload", board)
  }

  invisible()
}

poll <- function() {

  stage_pending_board()
  later::later(poll, 0.2)
}

poll()

serve(new_board(blocks = c(initmark = new_dataset_block("iris"))), id = "brd")
