vct_output <-
  list.files(
    path = file.path(Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR"), "quarto"),
    full.names = TRUE
  )
vct_clean <-
  vct_output |>
  gsub(x = _,
       pattern = "quarto\\/",
       replacement = "")
file.rename(
  from = vct_output,
  to   = vct_clean
)
unlink(file.path(Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR"), "quarto"),
       recursive = TRUE)
