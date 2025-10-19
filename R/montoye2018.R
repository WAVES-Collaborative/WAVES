# df = suppressWarnings(data.table::fread(
#   tar_read(vct_gt3x.raw.csv_visit_fpa)[2],
#   sep       = ",",
#   header    = TRUE,
#   skip      = 10,
#   drop      = c("Timestamp"),
#   col.names = c("x", "y", "z")
# ))
# parameters = list(
#   sf     = 100, # sampling frequency
#   window = 30 # in seconds
# )
calc_ggir_metrics_montoye2018 <- function(df,
                                          parameters = list(
                                            sf     = 100, # sampling frequency
                                            window = 30 # in seconds
                                          )) {

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
