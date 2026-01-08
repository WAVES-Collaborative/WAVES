#' @title Run GGIR in main pipeline.
#' @description Runs GGIR parts 1, 3 and 4 on provided raw data. Requires that
#'  all of vct_raw are the same "raw type". If not, the function will error and
#'  return NULL
#' @param vct_raw Character vector of filepaths to raw data.
#' @param vct_raw_type Character vector containing brand and data format name of
#'  raw data.
#'
#' @returns
#' @export
#'
#' @examples
wrapper_GGIR <- function(vct_raw,
                         vct_raw_type) {

  if (is.null(vct_raw)) return(NULL)

  all_type <- unique(vct_raw_type)

  if (length(all_type) > 1) {

    len_type <- length(all_type)
    cli::cli_abort(c(
      "More than one type of raw data is provided.",
      "i" = "{len_type} types are data were provided.",
      "i" = "Please provide data from one monitor brand, in one format."
    ))

  }

  if (all_type == "GENEACTIV - CSV w/ HEADER") {
    GGIR::GGIR(
      mode       = c(1, 2, 3, 4),
      datadir    = vct_raw,
      outputdir  = "data/GGIR",
      studyname  = "WAVES",
      # fo         = 1,
      # f1         = 2,
      do.report  = c(),
      configfile = "data/GGIR/config_WAVES_GENEActivHeaderCSV.csv"
    )
  } else if (all_type == "GENEACTIV - CSV w/o HEADER") {
    GGIR::GGIR(
      mode       = c(1, 2, 3, 4),
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
      mode       = c(1, 2, 3, 4),
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

#' @title Run GGIR in config pipeline.
#' @description Runs GGIR parts 1, 3 and 4 on provided config data.
#' @param vct_raw Character vector of filepaths to config data.
#' @param vct_raw_type Character vector containing brand and data format name of
#'  config data.
#'
#' @returns
#' @export
#'
#' @examples
wrapper_GGIR_config <- function(vct_raw,
                                vct_raw_type) {

  if (is.null(vct_raw)) return(NULL)

   for (i in seq_along(vct_raw)) {
     fpa_raw <- vct_raw[i]
     le_type <- vct_raw_type[i]

     if (le_type == "GENEACTIV - CSV w/ HEADER") {
       # GGIR::GGIR(
       #   mode = 1:4,
       #   datadir = fpa_raw,
       #   outputdir  = "data/0_CONFIG/GGIR",
       #   studyname  = "config",
       #   do.report  = c(),
       #   rmc.firstrow.acc = 101,
       #   rmc.firstrow.header = 1,
       #   rmc.header.length = 58,
       #   rmc.col.acc = 2:4,
       #   rmc.col.temp = 7,
       #   rmc.col.time = 1,
       #   rmc.unit.acc = "g",
       #   rmc.unit.temp = "C",
       #   rmc.unit.time = "POSIX",
       #   rmc.format.time = "%Y-%m-%d %H:%M:%OS",
       #   rmc.sf = 100, # my_sf
       #   # rmc.headername.sf = "Measurement Frequency", # doesn't get frequency correctly
       #   rmc.headername.sn = "Device Unique Serial Code",
       #   rmc.headername.recordingid = "Subject Code",
       #   configtz = "Canada/Saskatchewan" # my_tz
       # )
       GGIR::GGIR(
         mode       = c(1, 2, 3, 4),
         datadir    = fpa_raw,
         outputdir  = "data/0_CONFIG/GGIR",
         studyname  = "config",
         do.report  = c(),
         configfile = "data/GGIR/config_WAVES_GENEActivHeaderCSV.csv"
       )
     } else if (le_type == "GENEACTIV - CSV w/o HEADER") {
       GGIR::GGIR(
         mode       = c(1, 2, 3, 4),
         datadir    = fpa_raw,
         outputdir  = "data/0_CONFIG/GGIR",
         studyname  = "config",
         do.report  = c(),
         configfile = "data/GGIR/config_WAVES_GENEActivNoHeaderCSV.csv"
       )
     } else {
       GGIR::GGIR(
         mode       = c(1, 2, 3, 4),
         datadir    = fpa_raw,
         outputdir  = "data/0_CONFIG/GGIR",
         studyname  = "config",
         do.report  = c(),
         configfile = "data/GGIR/config_WAVES.csv"
       )
     }
   }

  list.files(
    file.path("data", "0_CONFIG", "GGIR", "output_WAVES", "meta", "basic"),
    pattern =
      basename(vct_raw) |>
      paste0(collapse = "|"),
    full.names = TRUE
  )

}
