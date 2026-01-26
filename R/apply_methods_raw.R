# Adpated from Lily Koff
# https://github.com/lilykoff/step_algorithms/blob/505a0b81971b662927fb4cbe4b442e6277bbb0b7/code/R/utils.R#L48
# From my (JM) understanding, the only difference between Koff's SDT code and
# Muschelli's code (https://github.com/muschellij2/walking/blob/86c13bb7c0fbf9afabe045fcdae493b273b10201/R/sdt.R#L23)
# is it doesn't include the walking::standardize_data() and assertthat() code
estimate_steps_sdtnew <- function(data,
                                  sample_rate,
                                  order = 4L,
                                  high = 0.25,
                                  low = 2.5,
                                  location = c("wrist", "waist"),
                                  verbose = TRUE) {

  location <-
    match.arg(location, choices = c("wrist", "waist"))
  threshold <- ifelse(
    location == "wrist",
    yes = 0.0359,
    no  = 0.0267
  )

  # vm threshold based on location
  # create coefficients for a 4th order bandpass Butterworth filter
  b <- signal::butter(
    n     = order,
    W     = c(high, low) / (sample_rate / 2),
    type  = "pass",
    plane = "z"
  )

  data <-
    data |>
    # dplyr::ungroup() |>
    mutate(
      vm        = sqrt(x^2 + y^2 + z^2),
      # demean and filter data with dual pass filter to avoid signal shift
      demean_vm = vm - mean(vm),
      filt_vm   = signal::filtfilt(b, demean_vm),
      # find indices in which the value immediately before and immediately
      # after the value is smaller and vm is above threshold
      peak =
        filt_vm > dplyr::lag(filt_vm) &
        filt_vm > dplyr::lead(filt_vm) &
        filt_vm > threshold
    )

  if (verbose) {
    # return steps by second
    message("sdt completed")
  }

  # data |>
  #   dplyr::group_by(time = floor_date(HEADER_TIMESTAMP)) |>
  #   dplyr::summarize(steps_sdt = sum(peak, na.rm = TRUE))
  data |>
    summarise(
      steps_sdt = sum(peak, na.rm = TRUE),
      .by = datetime
    ) |>
    pull(steps_sdt) |>
    # The sum will always be a whole number, save as integer to save memory.
    as.integer()

}
apply_methods_raw <- function(fpa_read,
                              vct_fpa_basic,
                              dir_models,
                              dir_write,
                              my_tz) {

  if (is.null(fpa_read)) return(NULL)

  # Read ----
  # Find the corresponding GGIR basic RData by matching raw csv filename.
  fnm <-
    basename(fpa_read)
  fnm_sans_ext <-
    basename(fpa_read) |>
    tools::file_path_sans_ext()

  # Check if file was already created from a previous run of the pipeline.
  fpa_write <- file.path(
    dir_write, paste0(fnm_sans_ext, ".parquet")
  )

  if (file.exists(fpa_write)) {return(
    arrow::read_parquet(fpa_write)
  )}

  grep(
    x       = vct_fpa_basic,
    pattern = tools::file_path_sans_ext(fnm_sans_ext),
    value   = TRUE
  ) |>
    load()
  rm(C, GGIRversion, M)
  mtx_data <-
    qs2::qd_read(fpa_read)

  # start of recording ----
  if (I$dformn == "gt3x") {
    rec_start_junk <-
      data.frame(I$header["Start Date", "value"])
    names(rec_start_junk)<-
      "start"
    rec_start_junk$start <-
      as.POSIXct(as.character(rec_start_junk$start),
                 format = "%Y-%m-%d %H:%M:%S")
    rec_start_junk <-
      rec_start_junk$start
    rec_start_dttm <- strptime(
      rec_start_junk,
      format = "%Y-%m-%d %H:%M:%OS",
      tz     = my_tz
    )
  } else if (I$dformn == "cwa") {
    rec_start_junk <-
      data.frame((I$header[[1]][3]))
    rec_start_junk <-
      rec_start_junk$start
    rec_start_dttm <- strptime(
      rec_start_junk,
      format = "%Y-%m-%d %H:%M:%OS",
      tz     = my_tz
    )
  } else if (I$dformn == "bin"){
    rec_start_junk <-
      data.frame((I$header[[1]][8]))
    rec_start_junk <-
      rec_start_junk[1,1]
    rec_start_dttm <- strptime(
      rec_start_junk,
      format = "%Y-%m-%d %H:%M:%OS",
      tz     = my_tz
    )
  } else if (I$monn == "actigraph" && I$dformn == "csv") {
    rec_start_junk <- paste(
      gsub(x           = I$header["Start Date", "value"],
           pattern     = "^\\s+|\\s+$",
           replacement = ""),
      I$header["Start Time", "value"] # has a leading whitespace
    )
    # TODO: Don't know if raw csv's exported from ActiLife in non-US computers
    # will export in m/d/Y format.
    rec_start_dttm <- strptime(
      rec_start_junk,
      format = "%m/%d/%Y %H:%M:%OS",
      tz     = my_tz
    )
  }

  rec_start_dttm <-
    floor_date(rec_start_dttm, unit = "seconds")

  # In seconds from 1970-01-01.
  rec_start_sec <-
    as.numeric(rec_start_dttm)
  rm(rec_start_junk); gc()

  # Apply ----
  ## while loop ----
  ### Prep ----
  nrow_data <-
    dim(mtx_data)[1]

  # Montoye
  load(file.path(dir_models, "montoye2018.RData"))

  # Oak 1.0
  use_condaenv("WHO_WAVES_oak_1.0")
  forest <- import("forest")
  np <- import("numpy")

  # variables used to read data in 24 hr increment
  chunk_is_last    <- FALSE
  chunk_begin      <- 1
  chunk_end        <- chunk_length <- I$sf * 60 * 60 * 24
  chunk_n          <- 1
  chunk_start_dttm <- rec_start_dttm
  chunk_start_sec  <- rec_start_sec
  df_all <- tibble(
    id = fnm_sans_ext,
    datetime = seq.POSIXt(
      from = rec_start_dttm,
      to   = rec_start_dttm + ceiling(nrow_data / I$sf) - 1,
      by   = "1 sec"
    ),
    intensity_montoye.rf  = NA_character_,
    intensity_montoye.nn  = NA_character_,
    intensity_montoye.dt  = NA_character_,
    intensity_montoye.svm = NA_character_,
    class_trost = NA_character_,
    class_ellis = NA_character_,
    steps_oak.1.0            = NA,
    steps_sdt                = NA_integer_,
    steps_verisense.original = NA_integer_,
    steps_verisense.revised  = NA_integer_
  )

  while(!chunk_is_last) {

    message("chunk", chunk_n)

    if (chunk_end >= nrow_data) {
      # if chunk is less than 24 hrs, set to end of data and make this
      # the last loop.
      chunk_end <-  nrow_data
      chunk_is_last <- TRUE
    }

    cat(
      "\rHours",
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

    ### Intensity: Montoye 2018 ----
    message("Montoye...", appendLF = FALSE)
    df_montoye <-
      as.data.frame(mtx_data[ind_chunk, ])
    names(df_montoye) <-
      names(df_montoye) |>
      toupper()

    # Window size is 30 seconds.
    n_window <- ceiling(
      nrow(df_montoye) / (I$sf * 30)
    )
    ind_montoye <- which(
      df_all$datetime %in% seq.POSIXt(
        from = chunk_start_dttm,
        by   = "30 secs",
        length.out = n_window
      )
    )
    df_montoye$window <- rep(
      seq_len(n_window),
      each       = I$sf * 30,
      length.out = nrow(df_montoye)
    )
    df_features <-
      df_montoye |>
      dplyr::summarise(
        dplyr::across(
          .cols = everything(),
          .fns = list(
            Mean   = mean,
            StdDev = sd,
            Min    = min,
            Max    = max,
            # For some reason all the percentile features need to start with "X".
            `X10th` = ~quantile(.x, probs = 0.10),
            `X25th` = ~quantile(.x, probs = 0.25),
            `X50th` = ~quantile(.x, probs = 0.50),
            `X75th` = ~quantile(.x, probs = 0.75),
            `X90th` = ~quantile(.x, probs = 0.90)
          ),
          .names = "{.fn}{.col}"
        ),
        .by = window
      )
    df_all[ind_montoye, c("intensity_montoye.rf",
                          "intensity_montoye.nn",
                          "intensity_montoye.dt",
                          "intensity_montoye.svm")] <-
      tibble(
        intensity_montoye.rf = predict(
          bsu_random_forest,
          newdata = df_features
        ),
        intensity_montoye.nn = predict(
          bsu_neural_network,
          newdata = df_features,
          type    = "class"
        ),
        intensity_montoye.dt = predict(
          bsu_decision_tree,
          newdata = df_features
        ),
        intensity_montoye.svm = predict(
          bsu_support_vector_machine,
          newdata = df_features,
          type    = "response"
        )
      )
    gc()

    ### Steps: SDT ----
    message("SDT...", appendLF = FALSE)
    ind_steps <- seq(
      from = ceiling(chunk_begin / I$sf),
      to   = ceiling(chunk_end / I$sf),
      by   = 1
    )
    df_all$steps_sdt[ind_steps] <-
      as.data.frame(mtx_data[ind_chunk, ]) |>
      mutate(
        datetime =
          seq.POSIXt(
            from       = chunk_start_dttm,
            by         = 1 / I$sf,
            length.out = length(ind_chunk)
          ) |>
          floor_date(unit = "seconds")
      ) |>
      estimate_steps_sdtnew(
        sample_rate = I$sf,
        location    = "wrist"
      )

    ### Steps: Verisense ----
    # The Versense method only utilizes vector magnitude of data. Output from
    # `wrist_steps` function is the same as OG verisense.
    message("Verisense...", appendLF = FALSE)
    vm <- sqrt(
      mtx_data[ind_chunk, "x"]^2 +
        mtx_data[ind_chunk, "y"]^2 +
        mtx_data[ind_chunk, "z"]^2
    )
    df_all$steps_verisense.original[ind_steps] <-
      walking::verisense_count_steps(
        data        = vm,
        sample_rate = I$sf
      ) |>
      as.integer() |>
      # Since function warns "Assuming data is a vector of VM!" but its fo sho
      # a  vector of VM.
      suppressWarnings()
    le_steps <-
      walking::verisense_count_steps_revised(
        data        = vm,
        sample_rate = I$sf
      ) |>
      as.integer() |>
      suppressWarnings()

    # For some reason, at the end of data, revised will have one second less
    # compared to original.
    if (length(le_steps) < length(ind_steps)) {
      ind_veri <- ind_steps[-length(ind_steps)]
    } else {
      ind_veri <- ind_steps
    }

    df_all$steps_verisense.revised[ind_veri] <- le_steps

    ### Steps: oak ----
    # Split into max 6 hours to try and prevent overloading memory.

    # time (t_bout) has to be in double format AND contain fractional seconds.
    # The below won't work if your vector just repeats the time value throughout
    # the sampling frequency.
    # Correct: 1512410340.00 1512410340.01 1512410340.02 1512410340.03 1512410340.04
    # Incorrect: 1512410340 1512410340 1512410340 1512410340 1512410340
    message("Oak...")

    if (round(length(ind_chunk) / I$sf / 3600, digits = 2) > 6) {

      #### oak chunks ----
      chunk_is_last_oak <- FALSE
      chunk_begin_oak   <- chunk_begin
      chunk_length_oak  <- I$sf * 60 * 60 * 6
      chunk_end_oak     <- chunk_begin_oak + chunk_length_oak - 1
      chunk_n_oak       <- 1
      oak_start_dttm    <- chunk_start_dttm
      oak_start_sec     <- chunk_start_sec

      while (!chunk_is_last_oak) {
        if (chunk_end_oak >= chunk_end) {
          chunk_end_oak <- chunk_end
          chunk_is_last_oak <- TRUE
        }
        ind_chunk_oak <-
          chunk_begin_oak:chunk_end_oak
        ind_steps_oak <- seq(
          from = ceiling(chunk_begin_oak / I$sf),
          to   = ceiling(chunk_end_oak / I$sf),
          by   = 1
        )

        chk_decimal <-
          last(ind_chunk_oak) / I$sf !=
          round(last(ind_chunk_oak) / I$sf, digits = 0)

        if (chunk_is_last_oak & chk_decimal) {

          # Oak doesn't like it when the last bit isn't easily divisible by the
          # sample frequency. Don't read in last bit of Hz then.
          ind_chunk_oak <- seq(
            from = chunk_begin_oak,
            to   = round(last(ind_chunk_oak) / I$sf, digits = 0) * I$sf
          )
          df_all$steps_oak.1.0[last(ind_steps_oak)] <- 0
          ind_steps_oak <- ind_steps_oak[-length(ind_steps_oak)]

        }

        vm_bout <- forest$oak$base$preprocess_bout(
          t_bout = np$array(
            seq(
              from = oak_start_sec,
              by = 1 / I$sf,
              length.out = length(ind_chunk_oak)
            ),
            dtype = "float64"
          ),
          x_bout = np$array(mtx_data[ind_chunk_oak, "x"], dtype = "float64"),
          y_bout = np$array(mtx_data[ind_chunk_oak, "y"], dtype = "float64"),
          z_bout = np$array(mtx_data[ind_chunk_oak, "z"], dtype = "float64"),
          fs     = as.integer(I$sf)
        )

        # defaults except for fs
        # https://github.com/onnela-lab/forest/blob/develop/docs/source/oak.md#default-tuning-parameters-for-walking-recognition-and-step-counting
        df_all$steps_oak.1.0[ind_steps_oak] <- forest$oak$base$find_walking(
          vm_bout = vm_bout[[2]],
          fs = as.integer(I$sf),
          min_amp = 0.3,
          step_freq = c(1.4, 2.3),
          alpha = 0.6,
          beta = 2.5,
          min_t = 3L,
          delta = 20L
        )

        chunk_begin_oak <- chunk_begin_oak + chunk_length_oak
        chunk_end_oak   <- chunk_begin_oak + chunk_length_oak - 1
        chunk_n_oak     <- chunk_n_oak + 1
        oak_start_dttm  <- oak_start_dttm + floor(chunk_begin_oak / I$sf)
        oak_start_sec   <- as.numeric(oak_start_dttm)
      }
    } else {

      #### no chunks ----
      chk_decimal <-
        last(ind_chunk) / I$sf !=
        round(last(ind_chunk) / I$sf, digits = 0)

      if (chk_decimal) {

        # Oak doesn't like it when the last bit isn't easily divisible by the
        # sample frequency. Don't read in last bit of Hz then.
        ind_chunk_oak <- seq(
          from = chunk_begin,
          to   = round(last(ind_chunk) / I$sf, digits = 0) * I$sf
        )
        df_all$steps_oak.1.0[last(ind_steps)] <- 0
        ind_steps_oak <- ind_steps[-length(ind_steps)]

      } else {
        ind_chunk_oak <- ind_chunk
        ind_steps_oak <- ind_steps
      }

      vm_bout <- forest$oak$base$preprocess_bout(
        t_bout = np$array(
          seq(
            from = chunk_start_sec,
            by = 1 / I$sf,
            length.out = length(ind_chunk_oak)
          ),
          dtype = "float64"
        ),
        x_bout = np$array(mtx_data[ind_chunk_oak, "x"], dtype = "float64"),
        y_bout = np$array(mtx_data[ind_chunk_oak, "y"], dtype = "float64"),
        z_bout = np$array(mtx_data[ind_chunk_oak, "z"], dtype = "float64"),
        fs     = as.integer(I$sf)
      )

      # defaults except for fs
      # https://github.com/onnela-lab/forest/blob/develop/docs/source/oak.md#default-tuning-parameters-for-walking-recognition-and-step-counting
      df_all$steps_oak.1.0[ind_steps_oak] <- forest$oak$base$find_walking(
        vm_bout = vm_bout[[2]],
        fs = as.integer(I$sf),
        min_amp = 0.3,
        step_freq = c(1.4, 2.3),
        alpha = 0.6,
        beta = 2.5,
        min_t = 3L,
        delta = 20L
      )
    }
    gc()

    ### To restart loop ----
    chunk_begin      <- chunk_begin + chunk_length
    chunk_end        <- chunk_begin + chunk_length - 1
    chunk_n          <- chunk_n + 1
    chunk_start_dttm <- chunk_start_dttm + (chunk_length / I$sf)
    chunk_start_sec  <- as.numeric(chunk_start_dttm)

  }

  rm(
    chunk_is_last,
    chunk_begin,
    chunk_end,
    chunk_length,
    chunk_n,
    chunk_start_dttm,
    chunk_start_sec,
    #
    ind_montoye,
    df_montoye,
    n_window,
    df_features,
    bsu_random_forest,
    bsu_neural_network,
    bsu_decision_tree,
    bsu_support_vector_machine,
    #
    chunk_is_last_oak,
    chunk_begin_oak,
    chunk_length_oak,
    chunk_end_oak,
    chunk_n_oak,
    adept_start_dttm,
    adept_start_sec,
    ind_chunk_oak,
    ind_steps_oak,
    #
    ind_steps,
    vm_bout,
    le_steps
  ) |>
    suppressWarnings()
  gc()

  ## Class: Trost Adult RF Wrist ----
  df_trost <- trost2017.extended(
    raw        = mtx_data,
    Fs         = I$sf,
    ID         = fnm_sans_ext,
    dir_models = dir_models, # mypath,
    win        = 10,
    sleep      = TRUE,
    Classifier = "Trost Adult Wrist RF",
    start.time = rec_start_sec
    # output      = output,
    # folder_name = folder_name
  )
  ind_trost <- which(
    df_all$datetime %in%
      ymd_hms(paste(df_trost$date, df_trost$time), tz = my_tz)
  )
  df_all$class_trost[ind_trost] <-
    df_trost$class
  rm(df_trost, ind_trost)
  gc()

  ## Class: Ellis ----
  df_ellis <- ellis2016.wrist(
    raw        = mtx_data,
    Fs         = I$sf,
    ID         = fnm_sans_ext,
    dir_models = dir_models, # mypath,
    win        = 60,
    Classifier = "Ellis Wrist RF",
    sleep      = TRUE,
    start.time = rec_start_sec
  )
  ind_ellis <- which(
    df_all$datetime %in%
      ymd_hms(paste(df_ellis$date, df_ellis$time), tz = my_tz)
  )
  df_all$class_ellis[ind_ellis] <-
    df_ellis$class
  rm(df_ellis, ind_ellis)
  gc()

  # Return ----
  df_all <-
    df_all |>
    fill(
      matches("intensity|class"),
      .direction = "down"
    )

  # Shouldn't be any NA for other variables.
  # anyNA(df_all)

  # For some reason, arrow doesn't like the POSIXCT format for datetime. Save
  # as numeric and check back later to see when they fix this.
  df_all |>
    mutate(datetime = as.numeric(datetime)) |>
    arrow::write_parquet(sink = fpa_write)
  # df_all$datetime[1] |>
  #   as.numeric() |>
  #   as.POSIXct(tz = my_tz)

  return(df_all)

}
