# vct_raw_csv = tar_read(vct_gt3x.raw_csv_field)
# sf          = tar_read(sf)
wrapper_GGIR <- function(vct_raw_csv) {

  if (is.null(vct_raw_csv)) return(NULL)

  # Determine average file size. If >= 1GB, its most likely field data.
  dbl_fsize <-
    file.size(vct_raw_csv) |>
    mean() / 2^30

  if (dbl_fsize >= 1) {
    le_mode <-
      1:5
    le_studyname <-
      "WAVES-FIELD"
  } else {
    le_mode <-
      1
    le_studyname <-
      "WAVES-VISIT"
  }

  GGIR::GGIR(
    mode       = le_mode,
    datadir    = vct_raw_csv,
    outputdir  = "data/GGIR",
    studyname  = le_studyname,
    # fo         = 1,
    # f1         = 2,
    do.report  = c(),
    configfile = "data/GGIR/config_WAVES.csv"
  )

  le_output <-
    paste0("output_", le_studyname)
  list.files(
    file.path("data", "GGIR", le_output, "meta", "basic"),
    pattern = "RData$",
    full.names = TRUE
  )
}
# df = data.table::fread(
#   vct_gt3x.raw.csv_visit[5],
#   sep = ",",
#   header = TRUE,
#   skip = 10,
#   # select = c("Accelerometer X", "Accelerometer Y", "Accelerometer Z"),
#   drop = "Timestamp",
#   col.names = c("x", "y", "z")
# )
# parameters = list(
#   sf     = 100,
#   window = 30
# )
calc_ggir_metrics_montoye2018 <- function(df,
                                          parameters = list(
                                            sf     = 100, # sampling frequency
                                            window = 30 # in seconds
                                          )) {

  df <-
    as.data.frame(df)
  names(df) <-
    names(df) |>
    toupper()
  n_window <- ceiling(
    nrow(df) / (parameters$sf * parameters$window)
  )
  df$window <- rep(
    seq_len(n_window),
    each       = parameters$sf * parameters$window,
    length.out = nrow(df)
  )
  df_features <-
    df |>
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
  return(df_features)

}
