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

  chk_csv_type <- any(vct_raw_type %in% c(
    "GENEACTIV - CSV w/ HEADER",
    "ADHOC",
    "UKNOWN"
  ))

  if (chk_csv_type) {
    cli::cli_abort(c(
      "Raw GENEActiv and adhoc csv's are currently not supported.",
      "i" = "Please use raw exported data such as {.value '.bin' '.gt3x' or '.cwa'} data.",
      "i" = "If non-csv data is not available, please reach out to WAVES data team for possible solutions."
    ))
  }

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

     chk_csv_type <- c(
       "GENEACTIV - CSV w/ HEADER",
       "ADHOC",
       "UKNOWN"
     )

     if (le_type %in% chk_csv_type) {
       # These are skipped for now
       next()
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
    file.path("data", "0_CONFIG", "GGIR", "output_config", "meta", "basic"),
    pattern =
      basename(vct_raw) |>
      paste0(collapse = "|"),
    full.names = TRUE
  )

}
