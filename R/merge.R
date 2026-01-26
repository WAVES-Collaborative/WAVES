merge_output <- function(lst_out.raw,
                         lst_out.oak.pre,
                         lst_out.cut,
                         lst_ox,
                         dir_merged,
                         my_tz) {

  lst_out.raw[sapply(lst_out.raw, is.null)] <- NULL
  lst_out.oak.pre[sapply(lst_out.oak.pre, is.null)] <- NULL
  lst_out.cut[sapply(lst_out.cut, is.null)] <- NULL

  vct_id_out.raw <- sapply(
    lst_out.raw, \(.x) .x$id[1]
  )
  vct_id_out.oak.pre <- sapply(
    lst_out.oak.pre, \(.x) .x$id[1]
  )
  vct_id_out.cut <- sapply(
    lst_out.cut, \(.x) .x$id[1]
  )
  vct_id_ox <- sapply(
    lst_ox, \(.x) .x$id[1]
  )

  names(lst_out.raw) <- vct_id_out.raw
  names(lst_out.oak.pre) <- vct_id_out.oak.pre
  names(lst_out.cut) <- vct_id_out.cut
  names(lst_ox) <- vct_id_ox

  vct_id_all <-
    union(vct_id_out.raw, vct_id_out.oak.pre) |>
    union(vct_id_out.cut)
    union(vct_id_ox)
  vct_fpa_write <- file.path(
    dir_merged, paste0(vct_id_all, ".parquet")
  )
  names(vct_fpa_write) <- vct_id_all

  # Merge ----
  for (i in seq_along(vct_id_all)) {

    le_id <- vct_id_all[i]
    fpa_write <- vct_fpa_write[le_id]

    # Check if file was already created from a previous run of the pipeline.
    if (file.exists(fpa_write)) next()

    # Determine which lists have the ID. If a list does not have a given ID,
    # then return an empty data.frame so it still merged.
    # First, get the earliest datetime from one of the lists that do have the ID.
    # Note: If one of the below `dttm_` objects are null, the `union` function
    # will return a numeric.
    dttm_raw <- lst_out.raw[[le_id]]$datetime[1]
    dttm_oak.pre <- lst_out.oak.pre[[le_id]]$datetime[1]
    dttm_cut <- lst_out.cut[[le_id]]$datetime[1]
    dttm_ox <- lst_ox[[le_id]]$datetime[1]

    # From union, get the earliest datetime so merge will go smoothly.
    dttm_earliest <-
      union(dttm_raw, dttm_oak.pre) |>
      union(dttm_cut)
      union(dttm_ox) |>
      as.POSIXct(tz = my_tz)
    chk_raw <- !le_id %in% vct_id_out.raw
    chk_oak.pre <- !le_id %in% vct_id_out.oak.pre
    chk_cut <- !le_id %in% vct_id_out.cut
    chk_ox <- !le_id %in% vct_id_ox

    if (chk_raw) {
      lst_out.raw[[le_id]] <- tibble(
        id = le_id,
        datetime = dttm_earliest,
        intensity_montoye.rf  = "9999",
        intensity_montoye.nn  = "9999",
        intensity_montoye.dt  = "9999",
        intensity_montoye.svm = "9999",
        class_trost = "9999",
        class_ellis = "9999",
        steps_oak.1.0            = 9999,
        steps_sdt                = 9999L,
        steps_verisense.original = 9999L,
        steps_verisense.revised  = 9999L
      )
    } else if (chk_oak.pre) {
      lst_out.oak.pre[[le_id]] <- tibble(
        id = le_id,
        datetime = dttm_earliest,
        steps_oak.pre = 9999
      )
    }else if (chk_cut) {
      lst_out.cut[[le_id]] <- tibble(
        id = le_id,
        datetime = dttm_earliest,
        intensity_bakrania.enmo.simple = "9999",
        intensity_bakrania.enmo.average = "9999",
        intensity_hildebrand = "9999",
        intensity_mielke = "9999",
        intensity_white.enmo.lin = "9999",
        intensity_white.enmo.pol = "9999",
        intensity_esliger = "9999",
        intensity_fraysee = "9999",
        intensity_white.hpfvm.lin = "9999",
        intensity_white.hpfvm.pol = "9999",
        intensity_bakrania.mad.simple = "9999",
        intensity_bakrania.mad.average = "9999"
      )
    } else if (chk_ox) {
      lst_ox[[le_id]] <- tibble(
        id = le_id,
        datetime = dttm_earliest,
        intensity_wamsley = "9999",
        intensity_actinet = "9999",
        steps_stepcount = 9999
      )
    }
    df <-
      full_join(
        lst_out.raw[[le_id]] |>
          mutate(datetime = as.POSIXct(datetime, tz = my_tz)),
        lst_out.oak.pre[[le_id]] |>
          mutate(datetime = as.POSIXct(datetime, tz = my_tz)),
        by = join_by(id, datetime)
      ) |>
      full_join(
        lst_out.cut[[le_id]],
        by = join_by(id, datetime)
      )
      left_join(
        lst_ox[[le_id]],
        by = join_by(id, datetime)
      ) |>
      mutate(
        across(
          .cols = starts_with("intensity_montoye"),
          .fns = ~factor(
            .x,
            levels = c("SED", "LPA", "MPA", "VPA", "9999"),
            labels = c("sedentary", "light", "mvpa", "mvpa", "9999")
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
                     "7",
                     "9999"),
          labels = c("sedentary", # 1 sedentary
                     "light",     # 2 stationary
                     "light",     #   stand_still
                     "light",     # 3 walking (non mvpa after applying 100mg right?)
                     "mvpa",      # 4 run
                     "mvpa",      #   mpa
                     "sleep",     # 6 JM is assuming this
                     "nonwear",   # 7 JM is assuming this
                     "9999")
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
                     "7",
                     "9999"),
          labels = c("sedentary",
                     "sedentary",
                     "light",
                     "light",
                     "light",
                     "mvpa",
                     "sleep",
                     "nonwear",
                     "9999")
        )
      ) |>
      select(id, datetime,
             starts_with("intensity"),
             starts_with("steps"),
             starts_with("class")) |>
      # Make all intensity variables have the same levels
      mutate(across(
        .cols = starts_with("intensity"),
        .fns = ~factor(.x, levels = c("sedentary", "light", "mvpa", "sleep", "nonwear", "9999"))
      ))

    # Write ----
    arrow::write_parquet(df, fpa_write)

  }

  list.files(
    path = dir_merged,
    full.names = TRUE
  )

}
merge_output_config <- function(lst_out.raw,
                                lst_out.oak.pre,
                                lst_out.cut,
                                lst_ox,
                                dir_merged,
                                my_tz) {

  # Merge ----
  df <-
    full_join(
      rbindlist(lst_out.raw) |>
        mutate(datetime = as.POSIXct(datetime, tz = my_tz)),
      rbindlist(lst_out.cut),
      by = join_by(id, datetime)
    ) |>
    left_join(
      rbindlist(lst_out.oak.pre),
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
          levels = c("SED", "LPA", "MPA", "VPA", "9999"),
          labels = c("sedentary", "light", "mvpa", "mvpa", "9999")
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
                   "7",
                   "9999"),
        labels = c("sedentary", # 1 sedentary
                   "light",     # 2 stationary
                   "light",     #   stand_still
                   "light",     # 3 walking (non mvpa after applying 100mg right?)
                   "mvpa",      # 4 run
                   "mvpa",      #   mpa
                   "sleep",     # 6 JM is assuming this
                   "nonwear",   # 7 JM is assuming this
                   "9999")
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
                   "7",
                   "9999"),
        labels = c("sedentary",
                   "sedentary",
                   "light",
                   "light",
                   "light",
                   "mvpa",
                   "sleep",
                   "nonwear",
                   "9999")
      )
    ) |>
    select(id, datetime,
           starts_with("intensity"),
           starts_with("steps"),
           starts_with("class")) |>
    # Make all intensity variables have the same levels
    mutate(across(
      .cols = starts_with("intensity"),
      .fns = ~factor(.x, levels = c("sedentary", "light", "mvpa", "sleep", "nonwear", "9999"))
    ))

  # Return ----
  fnm_write <- "WAVES_ALL_CONFIG.parquet"
  fpa_write <- file.path(dir_merged, fnm_write)
  arrow::write_parquet(df, fpa_write)
  return(fpa_write)

}
