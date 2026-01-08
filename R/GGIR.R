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
