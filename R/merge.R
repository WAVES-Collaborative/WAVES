merge_output_config <- function(vct_out.raw,
                                vct_out.oak.pre,
                                vct_out.cut,
                                vct_ox,
                                dir_merged,
                                my_tz) {

  vct_out.raw[sapply(vct_out.raw, is.null)] <- NULL
  vct_out.oak.pre[sapply(vct_out.oak.pre, is.null)] <- NULL
  vct_out.cut[sapply(vct_out.cut, is.null)] <- NULL

  # Merge ----
  df <-
    full_join(
      rbindlist(vct_out.raw) |>
        mutate(datetime = as.POSIXct(datetime, tz = my_tz)),
      rbindlist(vct_out.cut),
      by = join_by(id, datetime)
    ) |>
    left_join(
      rbindlist(vct_out.oak.pre) |>
        mutate(datetime = as.POSIXct(datetime, tz = my_tz)),
      by = join_by(id, datetime)
    ) |>
    left_join(
      rbindlist(vct_ox),
      by = join_by(id, datetime)
    ) |>
merge_wrangler <- function(df) {
  df |>
    mutate(
      across(
        .cols = starts_with("intensity_montoye"),
        .fns = ~factor(
          .x,
          levels = c("SED", "LPA", "MPA", "VPA"),
          labels = c("sedentary", "light", "mvpa", "mvpa")
        )
      ),
      intensity_trost = factor(
        class_trost,
        levels = c("1",
                   "2",
                   "stand_still",
                   "3",
                   "4",
                   "mpa",
                   "6",
                   "7"),
        labels = c("sedentary", # 1 sedentary
                   "light",     # 2 stationary
                   "light",     #   stand_still
                   "light",     # 3 walking (non mvpa after applying 100mg right?)
                   "mvpa",      # 4 run
                   "mvpa",      #   mpa
                   "sleep",     # 6 JM is assuming this
                   "nonwear")   # 7 JM is assuming this
      ),
      intensity_ellis = factor(
        class_ellis,
        levels = c("Sedentary",
                   "Vehicle",
                   "StandingMoving",
                   "StandingStill",
                   "Walking",
                   "Biking",
                   "6",
                   "7"),
        labels = c("sedentary",
                   "sedentary",
                   "light",
                   "light",
                   "light",
                   "mvpa",
                   "sleep",
                   "nonwear")
      )
    ) |>
    select(id, datetime,
           starts_with("intensity"),
           starts_with("steps"),
           starts_with("class")) |>
    # Make all intensity variables have the same levels
    mutate(across(
      .cols = starts_with("intensity"),
      .fns = ~factor(.x, levels = c("sedentary", "light", "mvpa", "sleep", "nonwear"))
    ))
}
merge_output <- function(vct_out.raw,
                         vct_out.oak.pre,
                         vct_out.cut,
                         vct_ox,
                         dir_merged,
                         df_start_tz) {

  # Read ----
  # Name vectors with filenames.
  lst_out <-
    lapply(
      list(vct_out.raw, vct_out.oak.pre, vct_out.cut, vct_ox),
      FUN = \(x) setNames(x, basename(x) |> file_path_sans_ext())
    ) |>
    setNames(c("raw", "oak.pre", "cut", "ox"))
  # Only merge files that have gone through all steps.
  vct_fnm <- Reduce(
    intersect,
    x = lapply(
      lst_out,
      FUN = names
    )
  )
  vct_fpa_write <-
    file.path(
      dir_merged, paste0(vct_fnm, ".parquet")
    ) |>
    setNames(vct_fnm)
  vct_complete <- vector("logical", length = length(vct_fnm))

  # Merge ----
  for (i in seq_along(vct_fnm)) {

    le_fnm <- vct_fnm[i]
    fpa_write <- vct_fpa_write[i]
    lst_start_tz <-
      df_start_tz |>
      dplyr::filter(fnm == le_fnm) |>
      as.list()

    # Check if file was already created from a previous run of the pipeline.
    if (file.exists(fpa_write)) {
      vct_complete[i] <- TRUE
      next
    }

    # read & merge
    full_join(
      read_parquet(lst_out$raw[le_fnm]) |> mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
      read_parquet(lst_out$oak.pre[le_fnm]) |> mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
      by = join_by(id, datetime)
    ) |>
      full_join(
        read_parquet(lst_out$cut[le_fnm]),
        by = join_by(id, datetime)
      ) |>
      left_join(
        read_parquet(lst_out$ox[le_fnm]),
        by = join_by(id, datetime)
      ) |>
      # wrangle
      merge_wrangler() |>
      # add date column and time column in timezone data was collected.
      mutate(
        date =
          with_tz(datetime, tzone = lst_start_tz$tz) |>
          date(),
        time =
          with_tz(datetime, tzone = lst_start_tz$tz) |>
          format("%H:%M:%S%z"),
        .after = datetime
      ) |>
      # write
      write_parquet(sink = fpa_write)

    vct_complete[i] <- TRUE

  }

  return(vct_fpa_write[vct_complete])

}
merge_output_config <- function(vct_out.raw,

}
