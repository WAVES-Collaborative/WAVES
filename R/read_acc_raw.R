read_acc_raw <- function(fpa_read,
                         le_type,
                         vct_fpa_basic,
                         dir_cal) {

  if (length(fpa_read) == 0) return(NULL)

  # Read ----
  # Adapted from "readAX_jhm" Sydney group code originally in "features.R".
  # Assuming initials "jhm" stand for jairo migueles, so shout out to him.
  fnm_sans_ext <-
    fpa_read |>
    basename() |>
    strip_all_ext()

  chk_gen <- le_type %in% c(
    "GENEACTIV - CSV w/ HEADER",
    "ADHOC",
    "UNKNOWN"
  )

  if (chk_gen) {
    # Geneactiv and adhoc csv reading not implemented for now.
    return(NULL)
  }

  # Check if file was already created from a previous run of the pipeline.
  fpa_write <- file.path(
    dir_cal,
    paste0(fnm_sans_ext, ".qs2")
  )

  if (file.exists(fpa_write)) return(fpa_write)

  ## GGIR Basic ----
  fpa_basic_match <- grep(
    x       = vct_fpa_basic,
    pattern = stringr::str_escape(fnm_sans_ext),
    value   = TRUE
  )

  if (length(fpa_basic_match) == 0) {
    warning(
      sprintf(
        "Skipping calibration for '%s' because no matching GGIR basic file was found.",
        basename(fpa_read)
      ),
      call. = FALSE
    )
    return(NULL)
  }

  load(fpa_basic_match[1])
  # Don't need output from `g.getmeta`
  rm(M); gc()

  if (is.null(I$sf)) {
    warning(
      sprintf(
        "Skipping calibration for '%s' because file is corrupt.",
        basename(fpa_read)
      ),
      call. = FALSE
    )
    return(NULL)
  }

  ## chk_cal ----
  chk_cal <-
    C$cal.error.end < C$cal.error.start
  if (is.null(C$cal.error.end)) chk_cal <- FALSE

  ## Params ----
  isLastBlock       <- FALSE
  iteration         <- 1
  cols_desired      <- c(
    "x", "y", "z"
  )
  lst_qc <- list()

  # g.readaccfile parameters, supply value for every argument except for filequality
  # which is used in other parts of GGIR but not here.
  blocknumber       <- 1
  PreviousEndPage   <- NULL
  PreviousLastValue <- c(0, 0, 1)
  PreviousLastTime  <- NULL
  header            <- NULL

  # Default raw data parameters.
  params_rawdata <-
    GGIR::extract_params(params2check = "rawdata")[["params_rawdata"]]

  # Although we don't extract the time column from accread$P, we will still
  # set desiredtz to "UTC" just in case.
  params_general <-
    GGIR::extract_params(params2check = "general")[["params_general"]]
  params_general$desiredtz <- "UTC"

  # Default cleaning parameters. Just need "nonwear_approach" from this list, which
  # is the 2023 method
  # nonwear_approach <- "2023"
  # GGIR::extract_params(params2check = "cleaning")[["params_cleaning"]]

  # Default nonwear clip block parameters.
  params_nw.clip.block <- GGIR::get_nw_clip_block_params(
    monc               = I$monc,
    dformat            = I$dformc,
    deviceSerialNumber = g.extractheadervars(I)$deviceSerialNumber,
    sf                 = I$sf,
    params_rawdata     = params_rawdata
  )
  # ws3 = params_general[["windowsizes"]][1]; ws2 = params_general[["windowsizes"]][2]; ws = params_general[["windowsizes"]][3]

  ## While loop ----
  cat("\nReading data chunk:\n")

  while (isLastBlock == FALSE) {

    cat(blocknumber, " ")
    ### 1 - read chunk ----
    accread <- GGIR::g.readaccfile(
      filename          = fpa_read,
      blocksize         = params_nw.clip.block$blocksize,
      blocknumber       = blocknumber,
      filequality       = NULL,
      ws                = 3600,
      PreviousEndPage   = PreviousEndPage,
      inspectfileobject = I,
      PreviousLastValue = PreviousLastValue,
      PreviousLastTime  = PreviousLastTime,
      params_rawdata    = params_rawdata,
      params_general    = params_general
    )

    if (is.null(accread$P)) break # empty block

    isLastBlock     <- accread$isLastBlock
    PreviousEndPage <- accread$endpage

    if ("PreviousLastValue" %in% names(accread$P)) { # output when reading ad-hoc csv
      PreviousLastValue <- accread$P$PreviousLastValue
      PreviousLastTime  <- accread$P$PreviousLastTime
    }

    if (le_type == "ACTIGRAPH - CSV") {

      # add time column
      accread$P$data$time <- seq(
        from       = 1,
        length.out = nrow(accread$P$data),
        by = 1/I$sf
      )
    }

    ## idle-sleep mode ----
    # https://github.com/wadpac/GGIR/blob/167a0159c99ec78192e93b70164e8d50502ee42b/R/g.getmeta.R#L220
    if (le_type %in% c("ACTIGRAPH - GT3X", "ACTIGRAPH - CSV")) {

      lst_impute <- g.imputeTimegaps(
        accread$P$data,
        sf                = I$sf,
        k                 = 0.25,
        impute            = TRUE,
        PreviousLastValue = PreviousLastValue,
        PreviousLastTime  = PreviousLastTime,
        epochsize         = params_general$windowsizes[1:2]
      )
      accread$P$data <- lst_impute$x
      lst_qc[[blocknumber]] <-
        lst_impute$QClog |>
        mutate(blocknumber = blocknumber,
               .before = 1)

      if (blocknumber == 1) {

        # get last time to check if idle-sleep mode occurs between
        # this block and the next. Also get last row for imputation.
        lastblock_endtime <- last(accread$P$data$time)
        lastblock_enddata <-
          last(accread$P$data[, c("x", "y", "z")])

      } else {

        chk_gap <- near(
          x   = accread$P$data$time[1] - lastblock_endtime,
          y   = 1 / I$sf,
          tol = 0.0001
        )

        if (chk_gap) {

          lastblock_endtime <- last(accread$P$data$time)
          lastblock_enddata <-
            last(accread$P$data[, c("x", "y", "z")])

        } else {

          # https://github.com/wadpac/GGIR/blob/388064b707df4fcfb7f9b755c5a43a477d371092/R/g.getmeta.R#L258
          # Impute gap between chunks
          timegap <- accread$P$data$time[1] - lastblock_endtime

          if (timegap > 3600 * I$sf) {

            stop(paste0("Time gap observed of more than 1 hour between data ",
                        "chunks for ", basename(datafile), " . Please contact ",
                        "package maintainer."), call. = FALSE)

          } else if (timegap > (3 / I$sf)) {

            # impute time gap of more than 3 samples and equal to or less than 1 hour
            # normalise last value
            lastblock_enddata <- lastblock_enddata / sqrt(sum(lastblock_enddata^2))

            # get number of rows to replicate last value, should always be a whole number...right? Sometimes
            # there is very very very small decimal left so truncate but double check
            # round would lead to same number.
            n_rep <- timegap  * I$sf

            if (trunc(n_rep) != round(n_rep)) stop("Imputing time between chunks, time gap does not result in whole number when multiplied by sample frequency.") # n_gap <- round(timegap  * I$sf)

            n_rep <- trunc(n_rep)
            accread$P$data <- bind_rows(
              # replicate last row and append to beginning, time column doesn't
              # matter since its not used for calibrating or in future steps.
              lastblock_enddata[rep(1, times = n_rep), ],
              accread$P$data
            )
            lastblock_endtime <- last(accread$P$data$time)
            lastblock_enddata <-
              last(accread$P$data[, c("x", "y", "z")])

          }
        }
      }
    }

    ## calibrate ----
    if (chk_cal) {accread$P$data[, c("x", "y", "z")] <- scale(
      accread$P$data[, c("x", "y", "z")],
      center = -C$offset,
      scale  = 1 / C$scale
    )}

    ## mtx_data ----
    n_data <- nrow(accread$P$data)

    if (is.null(n_data)) n_data <- 0

    cols_temp <- grep(
      x           = names(accread$P$data),
      pattern     = "temp|temperature",
      ignore.case = TRUE
    )
    cols_all <-
      c(cols_desired, names(accread$P$data)[cols_temp])

    if (blocknumber == 1) {
      mtx_data <-
        as.matrix(accread$P$data[, cols_all])
    } else if (n_data >= 1) {
      mtx_data <-rbind(
        mtx_data,
        as.matrix(accread$P$data[, cols_all])
      )
    }

    blocknumber <- blocknumber + 1
    rm(accread)
    gc()

  }

  # Idle Sleep Mode ----
  # https://github.com/wadpac/GGIR/blob/167a0159c99ec78192e93b70164e8d50502ee42b/R/g.getmeta.R#L220
  # if (le_type %in% c("ACTIGRAPH - GT3X", "ACTIGRAPH - CSV")) {
  #   lst_impute <- g.imputeTimegaps(
  #     as.data.frame(mtx_data),
  #     sf                = I$sf,
  #     k                 = 0.25,
  #     impute            = TRUE,
  #     PreviousLastValue = c(0, 0, 1),
  #     PreviousLastTime  = NULL,
  #     epochsize         = params_general$windowsizes[1:2]
  #   )
  #   mtx_data <- lst_impute$x
  #   QClog <- lst_impute$QClog
  # }

  # Dont think I need this for anything later in the pipeline but putting it
  # here just in case.
  # SWMT <- get_starttime_weekday_truncdata(
  #   mon       = I$monc,
  #   dformat   = I$dformc,
  #   data      = mtx_data,
  #   header    = accread$header,
  #   desiredtz = params_general$desiredtz,
  #   sf        = I$sf,
  #   datafile  = fpa_read,
  #   ws2       = params_general$windowsizes[2],
  #   configtz  = params_general$configtz
  # )

  # detect_nonwear that appears in M$metalong, might need it for later but
  # putting it here for now.
  # NWCW <- detect_nonwear_clipping(
  #   data             = mtx_data,
  #   windowsizes      = params_general$windowsizes,
  #   sf               = I$sf,
  #   clipthres        = params_nw.clip.block$clipthres,
  #   sdcriter         = params_nw.clip.block$sdcriter,
  #   racriter         = params_nw.clip.block$racriter,
  #   nonwear_approach = nonwear_approach,
  #   params_rawdata   = params_rawdata
  # )

  # Write ----
  qs2::qd_save(
    mtx_data,
    file = fpa_write
  )
  return(fpa_write)

}
