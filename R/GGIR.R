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
    mode       = c(1),
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
          mode       = c(1),
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
get_nonwear_sleep <- function(fpa_basic,
                              df_start_tz,
                              fdr_write) {

  le_fnm <-
    fpa_basic |>
    basename() |>
    strip_all_ext() |>
    gsub(
      x = _,
      pattern = "meta_",
      replacement = ""
    )
  lst_start_tz <-
    df_start_tz |>
    dplyr::filter(fnm == le_fnm) |>
    as.list()

  # Load ----
  # From GGIR part 1, the `M` object from g.getmeta() contains a data frame
  # called `metalong`, which contains a variable called `nonwearscore`.
  # The `nonwearscore` variable is how many axes meet brand-specific sd/range
  # nonwear and clipping thresholds.
  #
  # 1) Thresholds are determined from `get_nw_clip_block_params()` https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.getmeta.R#L130
  #    https://github.com/wadpac/GGIR/blob/main/R/get_nw_clip_block_params.R
  #
  # 2) Thresholds are input into detect_nonwear_clipping(), using the "2023"
  #    approach. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.getmeta.R#L435
  #    MODULE 2 - non-wear time & clipping
  #    NWCW = detect_nonwear_clipping(data = data, windowsizes = c(ws3, ws2, ws), sf = sf,
  #                                   clipthres = clipthres, sdcriter = sdcriter, racriter = racriter,
  #                                  nonwear_approach = params_cleaning[["nonwear_approach"]],
  #                                   params_rawdata = params_rawdata)
  #    NWav = NWCW$NWav; CWav = NWCW$CWav; nmin = NWCW$nmin
  #    # metalong
  #    col_mli = 2
  #    metalong[count2:((count2 - 1) + length(NWav)),col_mli] = NWav; col_mli = col_mli + 1
  #    metalong[count2:((count2 - 1) + length(NWav)),col_mli] = CWav; col_mli = col_mli + 1
  load(fpa_basic)

  # g.impute ----
  # In GGIR part 2, the `IMP` object from g.impute() contains a data frame
  # called `rout` which houses all the variables needed to determine the final
  # nonwear calculation, which is named `invalid` for GGIR part 3. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.part2.R#L196
  #
  # 1) A `wearthreshold` object is created, which is the "minimum number of
  #    accelerometer axis needed to meet the criteria for nonwear in order for the
  #    data to be detected as nonwear". https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.impute.R#L35
  #    `wearthreshold` for g.weardec is always 2. So if nonwearscore >= 2, then that
  #    15 minute epoch is considered non-wear
  #
  # 2) `metalong` from GGIR part 1 and `wearthreshold` are input for g.weardec().
  #    So basically if `metalong$nonwearscore` is >= 2 for a 15 minute window
  #    (if using default GGIR window sizes) then its considered non-wear https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.impute.R#L66
  #    Window size determined from the second number from "windowsizes" GGIR
  #    general argument. https://wadpac.github.io/GGIR/articles/GGIRParameters.html#windowsizes
  #
  # 3) A bunch of indicators are output from g.weardec(). https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.impute.R#L67
  #    r1 = #non-wear (1 == nonwear)
  #    r2 = #clipping (1 == clipping)
  #    r3 = #additional non-wear (set first and last 3 hours of recording as nonwear
  #                               based off vanHees 2013) https://wadpac.github.io/GGIR/articles/GGIRParameters.html#nonwearedgecorrection
  #    r4 = #protocol based decisions on data removal (which isn't used for WAVES so its always 0)
  #
  # 4) r5 is r1:r4 summed, with r5long being lengthened to match short windowsize (5 seconds) https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.impute.R#L346
  #    r5 = IMP$rout$r1 + IMP$rout$r2 + IMP$rout$r3 + IMP$rout$r4
  #    r5[which(r5 > 1) ] = 1
  #    r5[which(M$metalong$nonwearscore == -1)] = -1 # expanded data with expand_tail_max_hours
  #    #r5long is the same as r5, but with more values per period of time
  #    r5long = matrix(0,length(r5), 900 / 5)
  #    r5long = replace(r5long, 1:length(r5long), r5)
  #    r5long = t(r5long)
  #    dim(r5long) = c(length(r5) * 900 / 5, 1)
  #
  # 5) This output is saved as `rout` data fame. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.impute.R#L432
  #
  # 6) `IMP` is just saved with the rest of part 2 metadata, which we don't need. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.part2.R#L315

  # First, make additional objects needed for g.impute().
  # https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.part2.R#L141
  hvars = g.extractheadervars(I)
  ID = extractID(
    hvars = hvars,
    idloc = 1, # https://wadpac.github.io/GGIR/articles/GGIRParameters.html#idloc
    fname = I$filename
  )
  IMP = g.impute(
    M,
    I,
    params_cleaning   = GGIR::extract_params()[["params_cleaning"]],
    dayborder         = 0,               # default is 0 (day starts at 12am)
    desiredtz         = lst_start_tz$tz, # generic timezone from df_start_tz
    TimeSegments2Zero = c(),             # default c()
    acc.metric        = "ENMO",          # default "ENMO"
    ID                = ID,
    qwindowImp        = c(0, 24)         # defult c(0, 24) https://wadpac.github.io/GGIR/articles/GGIRParameters.html#qwindow
  )

  # g.sib.det ----
  # In GGIR part 3, `M` and `I` from part 1 and `IMP` from part 2 are used in
  # g.sib.det(), a "sustained inactivity bout" detection function that also includes
  # a sleep period time window estimate. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.part3.R#L87-L92

  # 1) Only r5 is used from `IMP` for nonwear, where any nonwear from the
  #    15-minute windows are lengthened into a `invalid` variable that is
  #    basically nonwear + clipping + additional nonwear for each 5 second window: https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L54-L66
  #
  #    # get indicator of non-wear periods
  #    n_epoch_short <- nrow(IMP$metashort)
  #
  #    if (n_epoch_short < length(IMP$r5long)) {
  #      invalid = IMP$r5long[seq_len(n_epoch_short)]
  #    } else {
  #      invalid = c(
  #        IMP$r5long,
  #        rep(0,
  #            each = (n_epoch_short - length(IMP$r5long)))
  #      )
  #    }
  #
  # 2) A `sleep` object is created from GGIR's HASIB(), which applies a
  #    heuristic algorithm for sustained inactivity bouts detection (SIB). https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L164-L170
  #    `T5A5` is the output, named as "T5A5" for "Time threshold 5 seconds,
  #    angle threshold 5 degrees as the HSAIB function can have multiple time and
  #    angle thresholds https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASIB.R#L79
  #    WAVES configuration will basically set it up like so:
  #
  #    # https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L72
  #    time <- format(IMP$metashort[, "timestamp"])
  #    # https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L73-L80
  #    fix_NA_invector <- function(x){
  #      if (length(which(is.na(x) == TRUE)) > 0) {
  #        x[which(is.na(x) == T)] = 0
  #      }
  #      return(x)
  #    }
  #    anglez <- as.numeric(as.matrix(IMP$metashort[, "anglez"]))
  #    anglez <- fix_NA_invector(anglez)
  #    # https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L104
  #    ACC <- as.numeric(as.matrix(IMP$metashort[, "ENMO"]))
  #    sleep <- HASIB(
  #      HASIB.algo        = "vanHees2015",    # "vanHees2015"
  #      timethreshold     = 5,                # 5 (degrees) https://wadpac.github.io/GGIR/articles/GGIRParameters.html#timethreshold
  #      anglethreshold    = 5,                # 5 (minutes) https://wadpac.github.io/GGIR/articles/GGIRParameters.html#anglethreshold
  #      time              = time,             # `timestamp` from IMP$metashort, which is the same as M$metashort
  #      anglez            = anglez,           # `anglez` w/ NAs converted to 0 from IMP$metashort, which is the same as M$metashort
  #      ws3               = M$windowsizes[1], # 5 seconds (within the GGIR pipeline this is extracted from `M` object)
  #      zeroCrossingCount = c(),              # c() (doesn't matter since we use vanHees2015 method)
  #      NeishabouriCount  = c(),              # c() (doesn't matter since we use vanHees2015 method)
  #      activity          = ACC,              # `ENMO` from IMP$metashoart, which is the same as M$metashort
  #      oakley_threshold  = NULL              # NULL (doesn't matter since we use vanHees2015 method)
  #    )
  #
  # 3) However, the `sleep` object is really just an indicator for sustained
  #    inactivity bouts and not sleep windows. GGIR's HASPT() is used for that,
  #    which applies a heuristic algorithm to estimate sleep period time (SPT)
  #    windows. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L303-L309

  #    - HASPT() is applied each noon -> noon 24hr window, so first GGIR needs to
  #      find the 24hr windows. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L183-L240
  #    - The loop begins and an index vector is created (tSegment) to get temporary
  #      ENMO (tmpACC), z-angle (tmpANGLE), and time (tmpTIME) vectors for HASPT(). https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L241-L301
  #    - HASPT() is finally applied https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L302-L312
  #    - WAVES configuration uses the HDCZA algorithm from vanHees et al. 2018,
  #      where if we were going to follow the steps from Figure 1 then:
  #      - Steps 1-3 are done within GGIR part 1. We don't change the short epoch
  #        length so the it part 1 outputs the 5 second averages of steps 1-2.
  #      - Steps 4-5 an internal function is created to get the rolling median
  #        of the absolute difference between successive values along a 5 minute
  #        window. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASPT.R#L105-L112
  #      - The 10th percentile of of values in a day multiplied by 15 is then
  #        determined, which is constrained between 0.13 and 0.50. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASPT.R#L113-L125
  #        GGIR allows users to change the percentile and multiplier but there
  #        is no prior publications researching how changes in these steps can
  #        result in "better" SPT windows. https://wadpac.github.io/GGIR/articles/GGIRParameters.html#hdcza_threshold
  #      - Step 6 The `threshold` object created from L113-125 is applied, where
  #        nonwear is potentially considered. However, WAVES configuration will
  #        end up with L367 or L369. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASPT.R#L363-L373
  #      - Initialize objects or Steps 7 - 9. Note that HASPT() returns NULL
  #        objects if the entire window is determined to be nonwear. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASPT.R#L374-L387
  #      - Step 7 changes blocks that are below `threshold` and <= 30 minutes
  #        to 0. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASPT.R#L389-L395
  #      - Step 8 Change time gaps < 60 minutes between blocks of 1 to 1. Note
  #        WAVES will always use code from 409 as we leave "spt_max_gap_ratio"
  #        parameter to 1.
  #        https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASPT.R#L409
  #        https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASPT.R#L411-L417
  #      - Step 9 the longest block is indicated with a 2 which is the SPT window,
  #        whereas blocks left with 1 are windows below the threshold. Just use
  #        2 since thats what we care about. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/HASPT.R#L418-L435
  #    - Sometimes there are day sleepers so GGIR will shift the window from
  #      noon -> noon to 6pm -> 6pm to double check. https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L313-L364
  #
  # 4) The SPT estimate is saved in the `output` object as the `spt_crude_estimate`
  #    variable. SLE$output$spt_crude_estimate == 2 is sleep period time window and
  #    is 1 for segments that met HASPT rules up to the final step of selecting the
  #    longest segment/block. In other words, they could've been the final sleep
  #    window but weren't because they weren't the longest.
  # 5) NOTE, actual sleep is estimated to be when `T5A5` == 1 & `spt_crude_estimate` == 2
  #    but since we do not care about quality of sleep such as sleep efficiency
  #    or sleep episodes, just use spt_crude_estimate == 2.

  SLE = g.sib.det(
    M,
    IMP,
    I,
    twd             = c(-12,12),                                # default
    acc.metric      = "ENMO",                                   # default acc.metric is "ENMO" https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L2
    desiredtz       = lst_start_tz$tz, # generic timezone from df_start_tz
    myfun           = c(),                                      # default is c()
    sensor.location = "wrist",                                  # default is "wrist" https://github.com/wadpac/GGIR/blob/9bcb9a9d105fb39a32330d493b5e9c4982a5d70f/R/g.sib.det.R#L3
    params_sleep    = GGIR::extract_params()[["params_sleep"]],
    zc.scale        = 1                                         # default is 1
  )

  if (is.null(SLE$output)) {

    # Will be NULL for files that are from direct observation sessions (less than
    # a day). In that case just fill variables with no non-wear and no sleep.
    SLE$output <- data.frame(
      id                 = le_fnm,
      datetime           = ymd_hms(M$metashort$timestamp, tz = "UTC"),
      sleep              = 0L,
      invalid            = 0L
    )
  } else {
    # Add filename, change time to POSIXct, and have sleep be binary.
    SLE$output <-
      SLE$output |>
      mutate(
        id = le_fnm,
        datetime =
          as.character(time) |>
          ymd_hms(tz = "UTC"),,
        invalid = as.integer(invalid),
        sleep = ifelse(
          spt_crude_estimate == 2,
          yes = 1L,
          no  = 0L
        ),
        .keep = "none",
        .before = 1
      )
  }

  # Write ----
  fpa_write <- file.path(
    fdr_write, paste0(le_fnm, ".parquet")
  )
  write_parquet(
    SLE$output,
    sink = fpa_write
  )

  return(fpa_write)

}
