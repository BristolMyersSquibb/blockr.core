render_mermaid <- function(mmd_file) {

  png_file <- sub("\\.mmd$", ".png", mmd_file)

  tmp_dir <- tempfile("mermaid_")
  dir.create(tmp_dir)
  qmd_file <- file.path(tmp_dir, "diagram.qmd")

  writeLines(c(
    "---",
    "format:",
    "  html:",
    "    mermaid-format: png",
    "---",
    "",
    "```{mermaid}",
    readLines(mmd_file),
    "```"
  ), qmd_file)

  system2(
    "quarto",
    c("render", shQuote(qmd_file), "--to", "html"),
    stdout = FALSE,
    stderr = FALSE
  )

  pngs <- list.files(
    file.path(tmp_dir, "diagram_files"),
    "\\.png$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(pngs) > 0L) {
    file.copy(pngs[1L], png_file, overwrite = TRUE)
  }

  unlink(tmp_dir, recursive = TRUE)
}
