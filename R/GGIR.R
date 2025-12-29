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

  # Determine if input is "happily read by GGIR or if user is providing a
  # GENEActiv csv file w/ header or a no header csv.
  # https://github.com/wadpac/GGIR/issues/518
  I <- suppressWarnings(tryCatch(
    GGIR::g.inspectfile(
      datafile = vct_raw[1],
      params_rawdata =
        GGIR::extract_params(params2check = "rawdata")[["params_rawdata"]]
    ),
    error = \(e) e
  ))

  chk_adhoc_csv <-
    I$header[1, 1] == "file does not have header" &&
    I$sf == 0

  if (class(I)[1] == "simpleError") {

    chk_geneactiv_csv <-
      I$message == "The GENEActiv csv reading functionality is deprecated in GGIR from version 2.6-4 onwards. Please, use either the GENEActiv bin files or try to read the csv files with GGIR::read.myacc.csv"

    if (chk_geneactiv_csv) {
      GGIR::GGIR(
        mode       = le_mode,
        datadir    = vct_raw,
        outputdir  = "data/GGIR",
        studyname  = "WAVES",
        # fo         = 1,
        # f1         = 2,
        do.report  = c(),
        configfile = "data/GGIR/config_WAVES_GENEActivHeaderCSV.csv"
      )
    } else {
      stop(
        "Provided csv file is not GENEActiv file or recognized by GGIR. Reach out to WAVES team.",
        call. = FALSE
      )
    }

  } else if (chk_adhoc_csv) {
    GGIR::GGIR(
      mode       = le_mode,
      datadir    = vct_raw,
      outputdir  = "data/GGIR",
      studyname  = "WAVES",
      # fo         = 1,
      # f1         = 2,
      do.report  = c(),
      configfile = "data/GGIR/config_WAVES_GENEActivNoHeaderCSV.csv"
    )
  } else {
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
  }

  list.files(
    file.path("data", "GGIR", "output_WAVES", "meta", "basic"),
    pattern =
      basename(vct_raw) |>
      paste0(collapse = "|"),
    full.names = TRUE
  )

}
