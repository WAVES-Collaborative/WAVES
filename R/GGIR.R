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

  if (length(vct_raw) == 0) return(NULL)

  chk_csv_type <- any(vct_raw_type %in% c(
    "GENEACTIV - CSV w/ HEADER",
    "ADHOC",
    "UNKNOWN"
  ))

  if (chk_csv_type) {
    cli::cli_abort(c(
      "Raw GENEActiv and adhoc csv's are currently not supported.",
      "i" = "Please use raw exported data such as {.value '.bin' '.gt3x' or '.cwa'} data.",
      "i" = "If non-csv data is not available, please reach out to WAVES data team for possible solutions."
    ))
  }

  # Make regex that collates vct_raw and escapes regex characters.
  le_regex <-
    vct_raw |>
    basename() |>
    stringr::str_escape() |>
    paste0(collapse = "|")

  # Check if files were already created from a previous run of the pipeline.
  vct_basic <- list.files(
    file.path("data", "GGIR", "output_WAVES", "meta", "basic"),
    pattern = le_regex,
    full.names = TRUE
  )
  vct_incomplete <- vct_raw[
    !basename(vct_raw) %in%
      (vct_basic |>
         basename() |>
         gsub(x = _,
              pattern = "meta_|\\.RData",
              replacement = ""))
  ]

  if (length(vct_incomplete) == 0) return(vct_basic)

  GGIR::GGIR(
    mode       = c(1, 2, 3, 4),
    datadir    = vct_incomplete,
    outputdir  = "data/GGIR",
    studyname  = "WAVES",
    # fo         = 1,
    # f1         = 2,
    do.report  = c(),
    configfile = "data/GGIR/config_WAVES.csv"
  )

  # Make regex that collates vct_raw and escapes regex characters.
  list.files(
    file.path("data", "GGIR", "output_WAVES", "meta", "basic"),
    pattern = le_regex,
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

  if (length(vct_raw) == 0) return(NULL)

  successful_raw <- character()

  for (i in seq_along(vct_raw)) {
    fpa_raw <- vct_raw[i]
    le_type <- vct_raw_type[i]

    chk_csv_type <- c(
      "GENEACTIV - CSV w/ HEADER",
      "ADHOC",
      "UNKNOWN"
    )

    if (le_type %in% chk_csv_type) {
      # These are skipped for now
      next()
    }

    ggir_result <- tryCatch(
      {
        GGIR::GGIR(
          mode       = c(1, 2, 3, 4),
          datadir    = fpa_raw,
          outputdir  = "data/0_CONFIG/GGIR",
          studyname  = "config",
          do.report  = c(),
          configfile = "data/GGIR/config_WAVES.csv"
        )
        TRUE
      },
      error = function(e) {
        warning(
          sprintf(
            "Skipping config file '%s' because GGIR failed: %s",
            basename(fpa_raw),
            conditionMessage(e)
          ),
          call. = FALSE
        )
        FALSE
      }
    )

    if (ggir_result) {
      successful_raw <- c(successful_raw, fpa_raw)
    }
  }

  if (length(successful_raw) == 0) {
    return(character())
  }

  list.files(
    file.path("data", "0_CONFIG", "GGIR", "output_config", "meta", "basic"),
    pattern =
      basename(successful_raw) |>
      paste0(collapse = "|"),
    full.names = TRUE
  )

}
find_timezone_by_offset <- function(offset_hours,
                                    dttm = Sys.time()) {
  # Validate input
  if (!is.numeric(offset_hours) || length(offset_hours) != 1) {
    stop("offset_hours must be a single numeric value.")
  }

  if (offset_hours == 0) return("UTC")

  # Get all available time zones
  tz_list <- OlsonNames()

  # Filter by matching offset
  matching_tz <- tz_list[
    sapply(tz_list, function(tz) {
      # Get offset in hours for the given date
      tz_offset <- as.numeric(format(as.POSIXct(dttm, tz = tz), "%z")) / 100
      tz_offset == offset_hours
    })
  ]

  grep(
    x = matching_tz,
    pattern = "Etc",
    value = TRUE
  )
}
find_offset <- function(tz) {
  # Validate input: tz should be in Olson name format.
  if(!tz %in% OlsonNames()) stop("tz not in Continent/City format")

  (Sys.time() |>
      as.POSIXct(tz = tz) |>
      format("%z") |>
      as.numeric()) /
    100

}
get_start_tz_df <- function(vct_fpa_basic,
                            my_tz) {

  vct_fnm <-
    vct_fpa_basic |>
    basename() |>
    strip_all_ext() |>
    gsub(
      x = _,
      pattern = "meta_",
      replacement = ""
    )

  lst_start_tz <- vector(mode = "list", length = length(vct_fnm))

  for (i in seq_along(vct_fnm)) {

    fnm <-
      vct_fnm[i]
    load(vct_fpa_basic[i])
    I$header

    if (I$dformn == "gt3x") {

      le_start_dttm <-
        I$header["Start Date", "value"] |>
        strptime(format = "%Y-%m-%d %H:%M:%OS",
                 tz     = "UTC")
      le_offset <-
        I$header["TimeZone", "value"] |>
        as.character() |>
        stri_extract(
          regex = "^[^\\:]+"
        ) |>
        as.numeric()
      le_tz <-
        find_timezone_by_offset(le_offset, le_start_dttm)

    } else if (I$dformn == "cwa") {

      # timezone information not saved in Axivity data. Since I specify
      # "desiredtz" GGIR argument to "UTC", the start time value will already
      # be in "UTC" timezone. Default to my_tz for Axivity files.
      le_start_dttm <- I$header["start", "value"]$start
      le_offset <- find_offset(my_tz)
      le_tz <- my_tz

    } else if (I$dformn == "bin"){

      le_start_dttm <-
        I$header["StarTime", "value"] |>
        strptime(format = "%Y-%m-%d %H:%M:%OS",
                 tz     = "UTC")
      le_offset <-
        (I$header["tzone", "value"] |>
           as.character() |>
           as.numeric()) /
        3600
      le_tz <-
        find_timezone_by_offset(le_offset, le_start_dttm)

    } else if (I$monn == "actigraph" && I$dformn == "csv") {

      # Timezone information not saved in csv header.
      le_start_dttm <-
        paste0(I$header["Start Date", "value"] |> stri_trim(side = "left"),
               I$header["Start Time", "value"]) |>
        strptime(format = "%m/%d/%Y %H:%M:%OS",
                 tz     = "UTC")
      le_offset <- find_offset(my_tz)
      le_tz <- my_tz

    }

    le_start_dttm <- floor_date(le_start_dttm, unit = "seconds")
    lst_start_tz[[i]] <-
      list(
        fnm,
        le_start_dttm,
        as.numeric(le_start_dttm),
        le_offset,
        le_tz
      ) |>
      setNames(c("fnm",
                 "start_dttm",
                 "start_secs",
                 "offset",
                 "tz"))

  }

  bind_rows(lst_start_tz)

}
