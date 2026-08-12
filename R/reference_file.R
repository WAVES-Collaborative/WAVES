process_reference_file <- function(vct_meta_ref,
                                   lst_yaml,
                                   dir_out.ref) {

  le_ref <-
    vct_meta_ref |>
    basename() |>
    file_path_sans_ext() |>
    stri_extract(regex = "(?!.*_).+")
  le_id <-
    vct_meta_ref |>
    basename() |>
    stri_replace(regex       = paste0("_", le_ref, "\\.qs"),
                 replacement = "")

  if (le_ref %in% c("do", "pal", "pass")) {
    switch(
      le_ref,
      "do" = {}, # TODO
      "pal" = tryCatch(
        process_activpal_file(
          vct_epoch   = lst_yaml$ref$pal$palp_fpa,
          vct_event   = lst_yaml$ref$pal$palv_fpa,
          le_id       = le_id,
          dir_out.ref = dir_out.ref
        ),
        error = \(e) e
      ),
      "pass" = {} # TODO
    )
  } else {
    # TODO: Flush out more if people want to add own reference processing
    # functions
    lst_yaml$ref[[le_ref]]$process_file_function(
      lst_param <- lst_yaml$ref[[le_ref]][
        !names(lst_yaml$ref[[le_ref]]) %in% c("process_meta_function",
                                              "process_file_function")
      ]
    )
  }

}
process_activpal_file <- function(vct_epoch,
                                  vct_event,
                                  le_id,
                                  dir_out.ref) {

  message("Processing: ", le_id)
  # EPOCH --------------------------------------------------------------------
  df_epoch <-
    fread(
      vct_epoch[grep(
        x = basename(vct_epoch),
        pattern = le_id
      )],
      sep    = ";",
      header = TRUE,
      skip   = 12
    ) |>
    mutate(
      id = le_id,
      # Time variable is number of days since 1899-12-30 but some of the floating
      # point values are ever so slightly smaller than what is printed to the console.
      # 1) Using only first value, change to seconds since 1899-12-30 (multiply by 60 * 60 * 24),
      # 2) convert to POSIXct,
      # 3) change to seconds since 1970-01-01,
      # 4) round for values that are ever so slightly smaller than what it should be,
      # 5) convert back to POSIXct
      datetime =
        (Time * 86400) |>
        as.POSIXct(tz = "UTC", origin = "1899-12-30") |>
        as.numeric() |>
        round(digits = 0) |>
        as.POSIXct(tz = "UTC"),
      time_palp = `Time(approx)`,
      mets_palp  = `Activity Score (MET.s)`,
      intensity4_palp = cut(
        mets_palp,
        breaks = c(0, 1.5, 2.0, 6.0, Inf),
        labels = c("sedentary",
                   "light",
                   "moderate",
                   "vigorous")
      ),
      intensity3_palp = factor(
        intensity4_palp,
        levels = c("sedentary",
                   "light",
                   "moderate",
                   "vigorous"),
        labels = c("sedentary",
                   "light",
                   "mvpa",
                   "mvpa")
      ),
      steps_palp = StepCount,
      sed_time = `Sedentary Time (s)`,
      upr_time = `Upright Time (s)`,
      stp_time = `Stepping Time (s)`,
      cyc_time = `Cycling Time (s)`,
      ply_time = `Primary Lying Time (s)`,
      sly_time = `Secondary Lying Time (s)`,
      non_time = `Nonwear Time (s)`,
      tra_time = `Seated Transport Time (s)`,
      err_time = `Data Errors (s)`,
      .keep = "none"
    )

  if (uniqueN(df_epoch$err_time) > 1) {
    tar_error(
      message = c(
        "`process_activpal` function:",
        "x" = "Epoch file has a data error. Double-check events file to see what corresponding {.var class_palv} is.",
        "i" = "Can be fixed, please reach out to WAVES team.",
        "i" = glue::glue("ID: {le_id}"),
        "i" = glue::glue("File: {basename(vct_event[le_id])}")
      ),
      class = "process_activpal"
    )
  }

  # EVENT --------------------------------------------------------------------
  df_event <-
    fread(
      vct_event[grep(
        x = basename(vct_event),
        pattern = le_id
      )],
      sep    = ";",
      header = TRUE,
      skip   = 14,
      fill   = TRUE
    ) |>
    mutate(
      id = le_id,
      # Time variable is number of days since 1899-12-30.
      # 1) Change to seconds since 1899-12-30 (multiply by 60 * 60 * 24),
      # 2) convert to POSIXct,
      # 3) change to seconds since 1970-01-01,
      # 4) convert back to POSIXct,
      # 5) include floor_date to truncate seconds since `Time(approx)` variable
      #    goes off rounded time.
      datetime =
        (Time * 86400) |>
        as.POSIXct(tz = "UTC", origin = "1899-12-30") |>
        as.numeric() |>
        as.POSIXct(tz = "UTC") |>
        floor_date(unit = "seconds"),
      time_palv = `Time(approx)`,
      class_palv = factor(
        x = `Event Type`,
        levels = c(0, 1, 2, 2.1, 3, 3.1, 3.2, 4, 5),
        labels = c("Sedentary",
                   "Upright",
                   "Stepping",
                   "Cycling",
                   "Lying",
                   "Primary Lying",
                   "Seconday Lying",
                   "Non-Wear",
                   "Travelling")
        # https://github.com/PALkitchen/activPAL/blob/d528217ce423fd2652d1c55f9469883cf147fb9f/R/activity.summary.by.period.R#L122
        # 0   ~ "Sedentary",
        # 1   ~ "Upright",
        # 2   ~ "Stepping",
        # 2.1 ~ "Cycling",
        # 3   ~ "Lying",
        # 3.1 ~ "Primary Lying", (Time in Bed??)
        # 3.2 ~ "Seconday Lying",
        # 4   ~ "Non-Wear",
        # 5   ~ "Travelling"
      ),
      mets_palv  = `Activity Score (MET.h)` / `Duration (s)` * 3600,
      intensity4_palv = cut(
        mets_palv,
        breaks = c(0, 1.5, 2.0, 6.0, Inf),
        labels = c("sedentary",
                   "light",
                   "moderate",
                   "vigorous")
      ),
      intensity3_palv = factor(
        intensity4_palv,
        levels = c("sedentary",
                   "light",
                   "moderate",
                   "vigorous"),
        labels = c("sedentary",
                   "light",
                   "mvpa",
                   "mvpa")
      ),
      # Cumulative steps * 2 = actual steps
      # https://github.com/PALkitchen/activPAL/blob/d528217ce423fd2652d1c55f9469883cf147fb9f/R/load.events.file.R#L124
      steps_palv = c(0, diff(`Cumulative Step Count`)) * 2,
      # steps_cum  = `Cumulative Step Count` * 2,
      waking_day = `Waking Day`,
      .keep = "none"
    )

  if (anyNA(df_event$event_palv)) {
    tar_error(
      message = c(
        "`process_activpal` function:",
        "x" = "Events file has NA in events type column after renaming.",
        "i" = "Can be fixed, please reach out to WAVES team.",
        "i" = "ID: {le_id}",
        "i" = "File: {basename(vct_event[le_id])}"
      ),
      class = "process_activpal"
    )
  }

  # MERGE --------------------------------------------------------------------
  df_pal <- full_join(
    df_epoch,
    df_event,
    by = join_by(id, datetime)
  ) |>
    mutate(
      waking_day      = vctrs::vec_fill_missing(waking_day,
                                                direction = "down"),
      mets_palv       = vctrs::vec_fill_missing(mets_palv,
                                                direction = "down"),
      intensity4_palv = vctrs::vec_fill_missing(intensity4_palv,
                                                direction = "down"),
      intensity3_palv = vctrs::vec_fill_missing(intensity3_palv,
                                                direction = "down"),
      steps_palv      = ifelse(is.na(steps_palv),
                               0,
                               steps_palv),
      # steps_cum       = vctrs::vec_fill_missing(steps_cum,
      #                                           direction = "down"),
      class_palv      = vctrs::vec_fill_missing(class_palv,
                                                direction = "down")
    ) |>
    select(
      id, datetime, time_palp, time_palv, waking_day,
      starts_with("mets"),
      starts_with("intensity3"),
      starts_with("intensity4"),
      starts_with("steps"),
      class_palv,
      everything()
    )
  # WRITE --------------------------------------------------------------------
  fpa_out.ref <- file.path(
    dir_out.ref,
    paste0(le_id, "_", "pal.parquet")
  )
  write_parquet(
    df_pal,
    sink = fpa_out.ref
  )

  if (file.exists(fpa_out.ref)) return(fpa_out.ref) else NULL
  # df_pal |>
  #   mutate(date = format(datetime, "%F")) |>
  #   select(id, datetime, date, mets_palp, mets_palv) |>
  #   dplyr::filter(date == "2019-07-22") |>
  #   pivot_longer(
  #     cols = starts_with("mets"),
  #     names_to = c("source"),
  #     names_pattern = "mets_(.*)",
  #     values_to = "mets"
  #   ) |>
  #   ggplot() +
  #   geom_line(
  #     mapping = aes(x = datetime,
  #                   y = mets,
  #                   group = source,
  #                   color = source)
  #   )
  #
  # Steps are different from epoch compared to event file. It is always more.
  # The highest cumulative steps from event file always matches total steps
  # from steps_palv.
  # df_pal |>
  #   mutate(date = as_date(datetime)) |>
  #   summarise(
  #     across(c(steps_palp, steps_palv), sum),
  #     steps_cum = max(steps_cum, na.rm = TRUE),
  #     .by = date
  #   ) |>
  #   mutate(
  #     steps_cum = c(steps_cum[1], diff(steps_cum))
  #   )
}
