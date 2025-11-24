wrapper_GGIR <- function(vct_raw) {

  if (is.null(vct_raw)) return(NULL)

  # Determine average file size. If >= 1GB, its most likely field data.
  dbl_fsize <-
    file.size(vct_raw) |>
    mean() / 2^30

  if (dbl_fsize >= 1) {
    le_mode <-
      1:5
  } else {
    le_mode <-
      1
  }

  GGIR::GGIR(
    mode       = le_mode,
    datadir    = vct_raw,
    outputdir  = "data/GGIR",
    studyname  = "WAVES",
    # fo         = 1,
    # f1         = 2,
    do.report  = c(),
    configfile = "data/GGIR/config_WAVES.csv"
  )
  list.files(
    file.path("data", "GGIR", "output_WAVES", "meta", "basic"),
    pattern =
      basename(vct_raw) |>
      paste0(collapse = "|"),
    full.names = TRUE
  )

}
