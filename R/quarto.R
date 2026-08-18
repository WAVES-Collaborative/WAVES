move_render <- function(tar_output) {

  # Check to see if it was run on Linux.
  if (reticulate:::is_linux()) {
    return(tar_output[1])
  }

  fnm <-
    tar_output |>
    basename() |>
    tools::file_path_sans_ext() |>
    unique()
  vct_output <- list.files(
    path = file.path("reports", "quarto"),
    pattern = fnm,
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
  unlink(file.path("reports", "quarto"),
         recursive = TRUE)
  vct_clean
}
