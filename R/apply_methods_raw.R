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
# This script was originally copied from
# https://github.com/ShimmerEngineering/Verisense-Toolbox/tree/master/Verisense_step_algorithm
# where it included the following software license:

# Copyright (c) 2020 Shimmer
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# by Matthew R Patterson, mpatterson@shimmersensing.com
# Find peaks of RMS acceleration signal according to Gu et al, 2017 method
# This method is based off finding peaks in the summed and squared acceleration signal
# and then using multiple thresholds to determine if each peak is a step or an artefact.
# An additional magnitude threshold was added to the algorithm to prevent false positives
# in free living data.

# Additionally incorporates code from John Muschelli's R package `walking`. Where:
# - have thresholds be function arguments.
# - `acc` object renamed to `vm`
# - `length(vm)` is replaced with object `len_vm`.
# - return early whenever the nrow/length of peak_info was <=2 or no steps were detected.
# - use peak_info colnames whenever possible to for readability.
estimate_steps_verisense <- function(
    data,
    sample_rate,
    k = 3, # window size
    period_min = 5,
    period_max = 15,
    similarity_threshold = -0.5,
    continuity_window_size = 4,
    continuity_threshold = 4,
    variance_threshold = 0.001,
    vm_threshold = 1.2,
    global_vm_threshold = 0.025
) {

  if (is.vector(data) && is.numeric(data)) {
    warning("Assuming data is a vector of VM!")
    vm <- data
  } else {
    vm <- sqrt(
      data[, "x"]^2 +
        data[,"y"]^2 +
        data[, "z"]^2
    )
  }

  len_vm <- length(vm)

  if (sd(vm) < global_vm_threshold) {
    # acceleration too low, no steps
    num_seconds <- round(len_vm / sample_rate)
    steps_per_sec <- rep(0, num_seconds)
    return(steps_per_sec)
  }

  half_k <- round(k / 2)
  segments <- floor(len_vm / k)
  peak_info <- matrix(NA, nrow = segments, ncol = 5)
  colnames(peak_info) <- c(
    "peak_location",
    "vm_max",
    "periodicity",
    "similarity",
    "continuity"
  )

  # for each segment find the peak location ----
  for (i in seq_len(segments)) {
    start_idx <- (i - 1) * k + 1
    end_idx <- start_idx + (k - 1)
    tmp_loc_a <- which.max(vm[start_idx:end_idx])
    tmp_loc_b <- (i - 1) * k + tmp_loc_a

    # only save if this is a peak value in range of -k/2:+K/2
    start_idx_ctr <- max(tmp_loc_b - half_k, 1)
    end_idx_ctr <- min(tmp_loc_b + half_k, len_vm)
    check_loc <- which.max(vm[start_idx_ctr:end_idx_ctr])

    if (check_loc == (half_k + 1)) {
      peak_info[i, "peak_location"] <- tmp_loc_b
      peak_info[i, "vm_max"] <- max(vm[start_idx:end_idx])
    }
  }

  peak_info <- peak_info[!is.na(peak_info[, "peak_location"]), ] # get rid of na rows

  # filter max vector magnitude based on vm_threshold ----
  peak_info <- peak_info[peak_info[, "vm_max"] > vm_threshold, ]

  # filter by periodicity ----
  # there must be at least two steps
  n_peaks <- nrow(peak_info)
  no_steps <- TRUE

  if (n_peaks > 2) {
    # Calculate periodicity.
    no_steps <- FALSE
    peak_info[1:(n_peaks - 1), "periodicity"] <- diff(peak_info[, "peak_location"]) # calculate periodicity
    peak_info <- peak_info[peak_info[, "periodicity"] > period_min, ] # filter peaks based on period_min
    peak_info <- peak_info[peak_info[, "periodicity"] < period_max, ]   # filter peaks based on period_max
  }

  n_peaks <- nrow(peak_info)

  if (n_peaks <= 2 || no_steps) {
    # no steps found
    num_seconds = round(len_vm / sample_rate)
    steps_per_sec = rep(0, num_seconds)
    return(steps_per_sec)
  }

  # Calculate similarity ----
  peak_info[1:(n_peaks - 2), "similarity"] <- -abs(diff(peak_info[, "vm_max"], lag = 2))
  peak_info <- peak_info[peak_info[, "similarity"] > similarity_threshold, , drop = FALSE]  # filter based on similarity_threshold
  peak_info <- peak_info[!is.na(peak_info[, "peak_location"]), , drop = FALSE] # previous statement can result in an NA in col-1

  # calculate continuity ----
  peak_info[, "continuity"] <- 0

  if (nrow(peak_info) > 5) {
    end_for <- nrow(peak_info) - 1

    for (i in continuity_threshold:end_for) {
      # for each bw peak period calculate vm var
      v_count <- 0 # count how many windows were over the variance threshold

      for (x in seq_len(continuity_threshold)) {
        ind_variance <-
          peak_info[i - x + 1, "peak_location"]:peak_info[i - x + 2, "peak_location"]

        if (var(vm[ind_variance]) > variance_threshold) v_count <- v_count + 1
      }

      if (v_count >= continuity_window_size) peak_info[i, "continuity"] <- 1 # set continuity to 1, otherwise, 0

    }
  }

  # continuity test - only keep locations after this.
  peak_location <- peak_info[peak_info[, "continuity"] == 1, "peak_location"]
  peak_location <- peak_location[!is.na(peak_location)] # previous statement can result in an NA in col-1

  if (length(peak_location) == 0) {
    # no steps found
    num_seconds = round(len_vm / sample_rate)
    steps_per_sec = rep(0, num_seconds)
    return(steps_per_sec)
  }

  # debug plot ----
  # install.packages("plotly")
  # df_vm <- data.frame(vm = vm, det_step = integer(len_vm))
  # df_vm$det_step[peak_location] <- 1
  # df_vm$idx <- as.numeric(row.names(df_vm))
  # plt <-
  #   ggplot(data=df_vm, aes(x = idx, y = vm)) +
  #   geom_line() +
  #   geom_point(
  #     data = subset(df_vm, det_step == 1),
  #     aes(x = idx, y = vm),
  #     color = 'red',
  #     size = 1,
  #     alpha = 0.7
  #   )
  # plotly::ggplotly(plt)

  # for GGIR, output the number of steps in 1 second chunks
  start_idx_vec <- seq(
    from = 1,
    to = len_vm,
    by = sample_rate
  )
  steps_per_sec <-
    findInterval(peak_location, start_idx_vec) |>
    factor(levels = seq_along(start_idx_vec)) |>
    table() |>
    as.integer()

  return(steps_per_sec)

}
apply_methods_raw <- function(fpa_read,
                              vct_fpa_basic,
                              dir_models,
                              dir_write,
                              df_start_tz,
                              lst_miniconda) {

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

  if (file.exists(fpa_write)) return(fpa_write)

  grep(
    x       = vct_fpa_basic,
    pattern =
      fnm_sans_ext |>
      file_path_sans_ext() |>
      stringr::str_escape(),
    value   = TRUE
  ) |>
    load()
  rm(C, GGIRversion, M)
  mtx_data <-
    qs2::qd_read(fpa_read)

  lst_start_tz <-
    df_start_tz |>
    dplyr::filter(fnm == fnm_sans_ext) |>
    as.list()

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
  chunk_start_dttm <- lst_start_tz$start_dttm
  chunk_start_sec  <- lst_start_tz$start_secs
  df_all <- tibble(
    id = fnm_sans_ext,
    datetime = seq.POSIXt(
      from = lst_start_tz$start_dttm,
      to   = lst_start_tz$start_dttm + ceiling(nrow_data / I$sf) - 1,
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
      estimate_steps_verisense(
        data = vm,
        sample_rate = I$sf
      ) |>
      # Since function warns "Assuming data is a vector of VM!" but its fo sho
      # a  vector of VM.
      suppressWarnings()
    le_steps <-
      estimate_steps_verisense(
        data = vm,
        sample_rate = I$sf,
        k                      = 4,
        period_min             = 4,
        period_max             = 20,
        similarity_threshold   = -1,
        continuity_window_size = 4,
        continuity_threshold   = 4,
        variance_threshold     = 0.01,
        vm_threshold           = 1.25,
        global_vm_threshold    = 0.025
      ) |>
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
            to   = floor(last(ind_chunk_oak) / I$sf) * I$sf
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
          to   = floor(last(ind_chunk_oak) / I$sf) * I$sf
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
    # montoye
    ind_montoye,
    df_montoye,
    n_window,
    df_features,
    bsu_random_forest,
    bsu_neural_network,
    bsu_decision_tree,
    bsu_support_vector_machine,
    # SDT
    ind_steps,
    # Verisense
    vm,
    le_steps,
    # Oak
    chunk_is_last_oak,
    chunk_begin_oak,
    chunk_length_oak,
    chunk_end_oak,
    chunk_n_oak,
    oak_start_dttm,
    oak_start_sec,
    ind_chunk_oak,
    ind_steps_oak,
    chk_decimal,
    vm_bout
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
    start.time = lst_start_tz$start_secs
  )
  ind_trost <- which(
    df_all$datetime %in%
      ymd_hms(paste(df_trost$date, df_trost$time), tz = "UTC")
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
    start.time = lst_start_tz$start_secs
  )
  ind_ellis <- which(
    df_all$datetime %in%
      ymd_hms(paste(df_ellis$date, df_ellis$time), tz = "UTC")
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

  return(fpa_write)

}

#' @title  Apply Oak from `walking` R package
#'
#' @description This cannot be within `apply_methods_raw` function as reticulate
#'  does not like swithing between conda environments within the same R session.
#'  The following error appears:
#'  The requested version of Python ('C:\Users\martinezj7\AppData\Local\r-miniconda\envs\WHO_WAVES_oak_pre/python.exe')
#'  cannot be used, as another version of Python
#'  ('C:/Users/martinezj7/AppData/Local/r-miniconda/envs/WHO_WAVES_oak_1.0/python.exe') has already been initialized. Please
#'  restart the R session if you need to attach reticulate to a different version of Python.
#' @param fpa_read
#' @param vct_fpa_basic
#' @param dir_write
#' @param df_start_tz
#' @param lst_miniconda
#'
#' @returns
#' @export
#'
#' @examples
apply_oak.pre <- function(fpa_read,
                          vct_fpa_basic,
                          dir_write,
                          df_start_tz,
                          lst_miniconda) {

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

  if (file.exists(fpa_write)) return(fpa_write)

  grep(
    x       = vct_fpa_basic,
    pattern =
      fnm_sans_ext |>
      file_path_sans_ext() |>
      stringr::str_escape(),
    value   = TRUE
  ) |>
    load()
  rm(C, GGIRversion, M)
  mtx_data <-
    qs2::qd_read(fpa_read)

  lst_start_tz <-
    df_start_tz |>
    dplyr::filter(fnm == fnm_sans_ext) |>
    as.list()

  # Apply ----
  ## while loop ----
  ### Prep ----
  nrow_data <-
    dim(mtx_data)[1]

  # Oak Pre-release
  use_condaenv("WHO_WAVES_oak_pre")
  forest <- import("forest")
  np <- import("numpy")

  # variables used to read data in 24 hr increment
  chunk_is_last    <- FALSE
  chunk_begin      <- 1
  chunk_end        <- chunk_length <- I$sf * 60 * 60 * 24
  chunk_n          <- 1
  chunk_start_dttm <- lst_start_tz$start_dttm
  chunk_start_sec  <- lst_start_tz$start_secs
  df_all <- tibble(
    id = fnm_sans_ext,
    datetime = seq.POSIXt(
      from = lst_start_tz$start_dttm,
      to   = lst_start_tz$start_dttm + ceiling(nrow_data / I$sf) - 1,
      by   = "1 sec"
    ),
    steps_oak.pre            = NA
  )

  while(!chunk_is_last) {

    if (chunk_end >= nrow_data) {
      # if chunk is less than 24 hrs, set to end of data and make this
      # the last loop.
      chunk_end <-  nrow_data
      chunk_is_last <- TRUE
    }

    ind_chunk <-
      chunk_begin:chunk_end
    ind_steps <- seq(
      from = ceiling(chunk_begin / I$sf),
      to   = ceiling(chunk_end / I$sf),
      by   = 1
    )

    ### Steps: oak ----
    # Split into max 6 hours to try and prevent overloading memory.

    # time (t_bout) has to be in double format AND contain fractional seconds.
    # The below won't work if your vector just repeats the time value throughout
    # the sampling frequency.
    # Correct: 1512410340.00 1512410340.01 1512410340.02 1512410340.03 1512410340.04
    # Incorrect: 1512410340 1512410340 1512410340 1512410340 1512410340

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
            to   = floor(last(ind_chunk_oak) / I$sf) * I$sf
          )
          df_all$steps_oak.pre[last(ind_steps_oak)] <- 0
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
        df_all$steps_oak.pre[ind_steps_oak] <- forest$oak$base$find_walking(
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
          to   = floor(last(ind_chunk_oak) / I$sf) * I$sf
        )
        df_all$steps_oak.pre[last(ind_steps)] <- 0
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
      df_all$steps_oak.pre[ind_steps_oak] <- forest$oak$base$find_walking(
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

  # Return ----
  # For some reason, arrow doesn't like the POSIXCT format for datetime. Save
  # as numeric and check back later to see when they fix this.
  df_all |>
    mutate(datetime = as.numeric(datetime)) |>
    arrow::write_parquet(sink = fpa_write)

  return(fpa_write)

}
