
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
# tar_load(my_tz)
merge_ox <- function(vct_ox_step,
                     vct_ox_wlms,
                     vct_ox_acti,
                     my_tz) {

  # Only merge files that have gone through all three algorithms.
  vct_fnm <-
    sapply(
      c(vct_ox_step, vct_ox_wlms, vct_ox_acti),
      \(.x) {
        basename(.x) |>
          sub(x = _,
              pattern = "-StepTimes\\.csv\\.gz|-timeSeries\\.csv\\.gz",
              replacement = "")
      }
    ) |>
    unique()
  lst_all <-
    vector(mode = "list", length = length(vct_fnm)) |>
    setNames(nm = vct_fnm)

  for (i in seq_along(vct_fnm)) {

    le_fnm <-
      vct_fnm[i]

    # Full join walmsley and actinet output (should always be a complete match,
    # as in there is no difference with performing a inner join), then left join
    # with stepcount output (since stepcount output is not complete time series).
    # Ok, I lied about walmsley and actinet always being a complete match.
    # Actinet will sometimes always(?) have one epoch longer than walmsley. BUT
    # in the grand scheme of things, the very last bit of data will not be used.
    # So just use a full join initially and when merged with all the other data
    # it will be fixed.
    lst_all[[le_fnm]] <-
      full_join(
        # walmsley
        fread(
          grep(x = vct_ox_wlms,
               pattern = le_fnm,
               value = TRUE),
          sep = ","
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
          ),
        # actinet
        fread(
          grep(x = vct_ox_acti,
               pattern = le_fnm,
               value = TRUE),
          sep = ","
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
          ),
        by = join_by(datetime)
      ) |>
      left_join(
        # stepcount
        fread(
          grep(x = vct_ox_step,
               pattern = le_fnm,
               value = TRUE),
          sep = ","
        ) |>
          mutate(
            # Floor it to the nearest second
            datetime =
              floor_date(time, unit = "seconds") |>
              force_tz(tzone  = my_tz)
          ) |>
          summarise(
            steps_stepcount = as.integer(n()),
            .by = datetime
          ),
        by = join_by(datetime)
      ) |>
      mutate(
        id = le_fnm,
        steps_stepcount = replace_na(steps_stepcount, replace = 0L),
        .before = 1
      )
  }

  return(lst_all)

}
