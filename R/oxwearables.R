
# tar_load(vct_raw)
# fdr_write = file.path(getwd(), tar_read(dir_stepcount))
apply_ox_stepcount <- function(vct_raw,
                               fdr_write) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  if (chk_windows) {
    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WHO_WAVES_stepcount & ",
        paste0(
          'stepcount "', vct_raw, '" -o "', fdr_write, '"',
          collapse = " & "
        ) |>
          # file paths to windows style.
          gsub(x = _,
               pattern = "/",
               replacement = "\\\\"),
        collapse = ""
      )
    )
  } else {
    # the `system2` command uses a shell within MAC.
    # During miniconda install, if the user opts to not add miniconda on PATH, will
    # it show up on the shell regardless? Ask Kirsten for testing this.
    stop("TODO")
  }

  list.files(
    path = fdr_write,
    pattern = "-StepTimes.csv.gz",
    full.names = TRUE,
    recursive = TRUE
  )

}
# tar_load(vct_raw)
# fdr_write = file.path(getwd(), tar_read(dir_walmsley))
# tar_load(my_tz)
apply_ox_walmsley <- function(vct_raw,
                              fdr_write,
                              my_tz) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  if (chk_windows) {

    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WHO_WAVES_accelerometer & ",
        paste0(
          'accProcess "', vct_raw, '" -o "', fdr_write, '"',
          " --timeZone ", my_tz,
          collapse = " & "
        ) |>
          # file paths to windows style.
          gsub(x = _,
               pattern = "/",
               replacement = "\\\\") |>
          # The above also changes the timezone, change it back
          gsub(x = _,
               pattern = sub(my_tz, pattern = "/", replacement = "\\\\\\\\"),
               replacement = my_tz),
        collapse = ""
      )
    )

  } else {
    # the `system2` command uses a shell within MAC.
    # During miniconda install, if the user opts to not add miniconda on PATH, will
    # it show up on the shell regardless? Ask Kirsten for testing this.
    stop("TODO")
  }

  list.files(
    path = fdr_write,
    pattern = "-timeSeries.csv.gz",
    full.names = TRUE
  )

}
# tar_load(vct_raw)
# fdr_write = file.path(getwd(), tar_read(dir_actinet))
apply_ox_actinet <- function(vct_raw,
                             fdr_write) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  if (chk_windows) {

    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WHO_WAVES_actinet & ",
        paste0(
          'actinet "', vct_raw, '" -o "', fdr_write, '"',
          collapse = " & "
        ) |>
          # file paths to windows style.
          gsub(x = _,
               pattern = "/",
               replacement = "\\\\"),
        collapse = ""
      )
    )

  } else {
    # the `system2` command uses a shell within MAC.
    # During miniconda install, if the user opts to not add miniconda on PATH, will
    # it show up on the shell regardless? Ask Kirsten for testing this.
    stop("TODO")
  }

  list.files(
    path = fdr_write,
    pattern = "-timeSeries.csv.gz",
    full.names = TRUE,
    recursive = TRUE
  )

}
# tar_load(vct_ox_step)
# tar_load(vct_ox_wlms)
# tar_load(vct_ox_acti)
merge_ox <- function(vct_ox_step,
                     vct_ox_wlms,
                     vct_ox_acti) {

  # stepcount ----
  df <- fread(
    list.files(
      path = fdr_write,
      pattern = "-StepTimes.csv.gz",
      full.names = TRUE,
      recursive = TRUE
    )[1],
    sep = ","
  ) |>
    mutate(
      # Floor it to the nearest second
      datetime =
        floor_date(time, unit = "seconds") |>
        force_tz(tzone  = my_tz)
    ) |>
    summarise(
      steps_stepcount = n(),
      .by = datetime
    )
  # walmsley ----
  df <-
    fread(
      list.files(
        path = fdr_write,
        pattern = "-timeSeries.csv.gz",
        full.names = TRUE
      )[1]
    ) |>
    mutate(
      datetime =
        ymd_hms(time, tz = my_tz, quiet = TRUE) |>
        floor_date(unit = "seconds"),
      intensity = case_when(
        sedentary == 1 ~ "sedentary",
        light == 1     ~ "light",
        `moderate-vigorous` == 1 ~ "mvpa",
        sleep == 1 ~ "sleep",
        .default = NA
      ),
      .keep = "none"
    ) |>
    reframe(
      datetime = seq.POSIXt(
        from = datetime[1],
        to = last(datetime) + 29,
        by = "1 sec"
      ),
      intensity_wamsley = rep(intensity, each = 30)
    )
  # actinet ----
  df <-
    fread(
      list.files(
        path = fdr_write,
        pattern = "-timeSeries.csv.gz",
        full.names = TRUE,
        recursive = TRUE
      )[1]
    ) |>
    mutate(
      datetime = force_tz(time, tzone = my_tz),
      intensity = case_when(
        sedentary == 1 ~ "sedentary",
        light == 1     ~ "light",
        `moderate-vigorous` == 1 ~ "mvpa",
        sleep == 1 ~ "sleep",
        .default = NA
      ),
      .keep = "none"
    ) |>
    reframe(
      datetime = seq.POSIXt(
        from = datetime[1],
        to = last(datetime) + 29,
        by = "1 sec"
      ),
      intensity_actinet = rep(intensity, each = 30)
    )

}
