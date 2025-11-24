read_acc_raw <- function(fpa_read,
                         vct_fpa_basic,
                         dir_cal,
                         my_tz) {

  if (is.null(fpa_read)) return(NULL)

  # Read ----
  # Adapted from "readAX_jhm" Sydney group code originally in "features.R".
  # Assuming initials "jhm" stand for jairo migueles, so shout out to him.
  fnm_sans_ext <-
    fpa_read |>
    basename() |>
    tools::file_path_sans_ext()
  fnm_ext <-
    tools::file_ext(fpa_read)

  ## GGIR Basic ----
  grep(
    x       = vct_fpa_basic,
    pattern = fnm_sans_ext,
    value   = TRUE
  ) |>
    load()
  # Don't need output from `g.getmeta`
  rm(M); gc()

  ## Inspect/Params ----
  # I <-
  #   GGIR::g.inspectfile(fpa_read)
  # Extract parameters for reading raw in chunks
  params_rawdata <-
    GGIR::extract_params(params2check = "rawdata")[["params_rawdata"]]
  params_nw.clip.block <- GGIR::get_nw_clip_block_params(
    monc           = I$monc,
    dformat        = I$dformc,
    sf             = I$sf,
    params_rawdata = params_rawdata
  )

  ## While loop ----
  isLastBlock <-
    FALSE
  blocknumber <-
    1
  iteration <-
    1
  PreviousLastValue <-
    c(0, 0, 1)
  PreviousLastTime <-
    NULL
  PreviousEndPage <-
    NULL
  cols_desired <- c(
    "x", "y", "z"
  )
  cat("\nReading data chunk:\n")

  while (isLastBlock == FALSE) {

    cat(blocknumber, " ")
    # 1 - read chunk
    data <- GGIR::g.readaccfile(
      filename          = fpa_read,
      blocksize         = params_nw.clip.block$blocksize,
      blocknumber       = blocknumber,
      filequality       = NULL,
      ws                = 3600,
      PreviousEndPage   = PreviousEndPage,
      inspectfileobject = I,
      PreviousLastValue = PreviousLastValue,
      PreviousLastTime  = PreviousLastTime
    )$P$data
    # data <-
    #   accread$P$data
    blocknumber <-
      blocknumber + 1
    # PreviousLastTime = accread$PreviousLastTime; PreviousEndPage = accread$PreviousEndPage
    # isLastBlock = accread$isLastBlock; S = accread$S
    # remaining_epochs = accread$remaining_epochs; nHoursRead = accread$nHoursRead
    # rm(accread); gc()
    gc()
    cols_temp <- grep(
      x           = names(data),
      pattern     = "temp|temperature",
      ignore.case = TRUE
    )
    cols_all <-
      c(cols_desired, names(data)[cols_temp])

    if (iteration == 1) {
      mtx_data <-
        as.matrix(data[, cols_all])
    } else if (iteration > 1 & length(data) >= 1) {
      mtx_data <-
        rbind(mtx_data,
              as.matrix(data[, cols_all]))
    }

    n_data <-
      nrow(data)

    if (length(n_data) == 0) n_data <- 0

    if (n_data < ((I$sf * 60 * 2) + 1)) isLastBlock <- TRUE

    iteration <-
      iteration + 1

  }

  # Impute Time Gaps ----
  # TODO: Implement from g.getmeta, line 220
  # https://github.com/wadpac/GGIR/blob/167a0159c99ec78192e93b70164e8d50502ee42b/R/g.getmeta.R#L220

  # Calibrate ----
  # TODO: Have someone look over this section.
  # The output from recalibration is different between GGIR and actimetric/sydney
  # code. Is it because actimetric/sydney is specific to GENEActiv? Hence the
  # GN in `calibrateGN`?
  # For now, doing it how its done in GGIR.

  # source(file.path("R", "sydney", "GN function_20191024.R"))
  # source(file.path("R", "sydney", "center_radius.R"))
  # C_sydney <- try(
  #   calibrateGN(raw = mtx_data,
  #               Fs  = I$sf),
  #   silent = TRUE
  # )
  # C_sydney$offset
  # C$offset
  # C_sydney$scale
  # C$scale

  ## While loop ----
  chk_cal <-
    C$cal.error.end < C$cal.error.start
  if (is.null(C$cal.error.end)) chk_cal <- FALSE

  if (chk_cal) {
    # variables used to read data in 24 hr increment
    chunk_is_last <-
      FALSE
    chunk_begin <-
      1
    chunk_end <- chunk_length <-
      I$sf * 60 * 60 * 24
    chunk_n <-
      1
    nrow_data <-
      dim(mtx_data)[1]

    while(!chunk_is_last) {

      if (chunk_end >= nrow_data) {
        # if chunk is less than 24 hrs, set to end of data and make this
        # the last loop.
        chunk_end <-
          nrow_data
        chunk_is_last <-
          TRUE
      }

      cat(
        "\rCalibrating hours",
        round(chunk_begin / I$sf / 3600,
              digits = 2),
        "to",
        round(chunk_end / I$sf / 3600,
              digits = 2),
        "out of",
        round(nrow_data / I$sf / 3600,
              digits = 2),
        "\r",
        sep = " "
      )

      ind_chunk <-
        chunk_begin:chunk_end

      # How it's done in `g.getmeta`, line 341
      # https://github.com/wadpac/GGIR/blob/167a0159c99ec78192e93b70164e8d50502ee42b/R/g.getmeta.R#L341
      mtx_data[ind_chunk, c("x", "y", "z")] <- scale(
        mtx_data[ind_chunk, c("x", "y", "z")],
        center = -C$offset,
        scale  = 1 / C$scale
      )
      # mtx_data[ind_chunk, "x"] <-
      #   C_sydney$scale[1] * (mtx_data[ind_chunk, "x"] - C_sydney$offset[1])
      # mtx_data[ind_chunk, "y"]<-
      #   C_sydney$scale[2] * (mtx_data[ind_chunk, "y"] - C_sydney$offset[2])
      # mtx_data[ind_chunk, "z"]<-
      #   C_sydney$scale[3] * (mtx_data[ind_chunk, "z"] - C_sydney$offset[3])

      chunk_begin <-
        chunk_begin + chunk_length
      chunk_end <-
        chunk_begin + chunk_length - 1
      chunk_n <-
        chunk_n + 1

    }
  }

  # Write ----
  fpa_write <- file.path(
    dir_cal,
    paste0(fnm_sans_ext, ".qs2")
  )
  qs2::qd_save(
    mtx_data,
    file = fpa_write
  )
  return(fpa_write)

}
