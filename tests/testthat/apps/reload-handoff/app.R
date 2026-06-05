library(blockr.core)

# Fixture for the positive per-tab handoff (see test-reload-isolation.R).
#
# Just before reloading, the board server stages the next board under a
# per-tab token (`reload-<token>`) and stamps that token into the URL. Here we
# stage such a slot on demand: when REPRO_PLANT names an existing file, its
# contents are the token, and we stage a "restored" board under
# `reload-<token>` exactly as the producer does. The harness then connects the
# returning session with the token in its URL (it must land on the staged
# board) and a fresh session without it (it must not).

plant_file <- Sys.getenv("REPRO_PLANT", "")
core_ns <- asNamespace("blockr.core")

stage_token_board <- function() {

  if (!nzchar(plant_file) || !file.exists(plant_file)) {
    return(invisible())
  }

  token <- tryCatch(readLines(plant_file, warn = FALSE)[1L],
                    error = function(e) "")
  unlink(plant_file)

  if (length(token) && nzchar(token)) {
    board <- new_board(blocks = c(restored = new_dataset_block("BOD")))
    get("update_serve_obj", core_ns)(paste0("reload-", token), board)
  }

  invisible()
}

poll <- function() {

  stage_token_board()
  later::later(poll, 0.2)
}

poll()

serve(new_board(blocks = c(initmark = new_dataset_block("iris"))), id = "brd")
