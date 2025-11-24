# lst_out.raw = tar_read(lst_out.raw_visit)
# lst_out.cut = tar_read(lst_out.cut_visit)
# dir_merged = tar_read(dir_merged)
merge_output <- function(lst_out.raw,
                         lst_out.cut,
                         lst_ox,
                         dir_merged) {

  # Merge ----
  df <-
    full_join(
      rbindlist(lst_out.raw),
      rbindlist(lst_out.cut),
      by = join_by(id, datetime)
    ) |>
    left_join(
      rbindlist(lst_ox),
      by = join_by(id, datetime)
    ) |>
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


  # Return ----
  fnm_write <- "WAVES_OUTPUT_ALL.parquet"
  fpa_write <- file.path(dir_merged, fnm_write)
  arrow::write_parquet(df, fpa_write)
  return(fpa_write)

}
