prepare_ox_input <- function(vct_raw,
                             vct_raw_type,
                             vct_basic) {

  vct_ox_input <- vector(
    mode = "character",
    length = length(vct_raw)
  )

  for (i in seq_along(vct_raw)) {

    fpa_raw <- vct_raw[i]
    le_type <- vct_raw_type[i]
    chk_gen <- le_type %in% c(
      "GENEACTIV - CSV w/ HEADER",
      "ADHOC",
      "UKNOWN"
    )
    chk_gt3x <- le_type == "ACTIGRAPH - CSV"
    chk_axiv <- le_type == "AXIVITY - CSV"

    if (chk_gen) {
      next()
    } else if (chk_gt3x) {

      # TODO
      next()

      # # Determine if timestamp column exists. If it does, supply it as a column
      # # in --csv-txyz. If not, then need to supply --start BUT unsure how to
      # # specify absent timestamp column.
      # chk_time_col <- "Timestamp" %in% (
      #   fread(fpa_raw,
      #         nrows = 2,
      #         skip = 10,
      #         header = TRUE) |>
      #     colnames()
      # )
      #
      # if (chk_time_col) {
      #   vct_ox_input[i] <- paste0(
      #     '"', fpa_raw, '"',
      #     ' --csv-txyz "Timestamp,Accelerometer X,Accelerometer Y,Accelerometer Z"',
      #     ' --csv-start-row 11'
      #   )
      #
      # }
      #
      # grep(
      #   x       = vct_basic,
      #   pattern = basename(fpa_raw),
      #   value   = TRUE
      # ) |>
      #   load()
      # M

    } else if (chk_axiv) {
      # TODO
      next()
    } else {
      vct_ox_input[i] <- paste0('"', fpa_raw, '"')
    }

  }

  return(
    vct_ox_input[!stri_isempty(vct_ox_input)]
  )

}
apply_ox_stepcount <- function(vct_ox_input,
                               fdr_write,
                               fdr_log,
                               log_prefix = "",
                               ...) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  # Check if files were already created from a previous run of the pipeline.
  vct_fnm_write <-
    vct_ox_input |>
    gsub(x = _,
         pattern = '"',
         replacement = "") |>
    basename() |>
    file_path_sans_ext()
  vct_fpa_write <-
    vct_fnm_write |>
    paste0(... = _, "-StepTimes\\.csv\\.gz",
           collapse = "|") |>
    list.files(path = fdr_write,
               pattern = _,
               full.names = TRUE,
               recursive = TRUE)
  chk_write <- length(vct_fpa_write) > 0
  chk_length <- length(vct_fpa_write) == length(vct_ox_input)

  if (chk_write & chk_length) {
    return(vct_fpa_write)
  } else if (chk_write) {
    vct_ox_input <- grep(
      x = vct_ox_input,
      pattern =
        vct_fpa_write |>
        dirname() |>
        basename() |>
        paste0(... = _, collapse = "|"),
      value = TRUE,
      invert = TRUE
    )
  }

  if (chk_windows) {
    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WHO_WAVES_stepcount & ",
        paste0(
          'stepcount ', vct_ox_input, ' -o "', fdr_write, '"',
          collapse = " & "
        ) |>
          # file paths to windows style.
          gsub(x = _,
               pattern = "/",
               replacement = "\\\\"),
        collapse = ""
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "stepcount_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "stepcount_err.txt"))
    )
  } else {
    # The `system2` command uses a shell within MacOS and Linux.
    # TODO Ask Kirsten for testing this.
    # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_stepcount ; stepcount stepcountTest.gt3x -o ~/WAVES/data/stepcount
    system2(
      command = "source",
      args = paste(
        paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        "conda activate WHO_WAVES_stepcount",
        paste0(
          'stepcount ', vct_ox_input, ' -o "', fdr_write, '"',
          collapse = " ; "
        ),
        sep = " ; "
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "stepcount_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "stepcount_err.txt"))
    )
  }

  list.files(
    path = fdr_write,
    pattern = "-StepTimes.csv.gz",
    full.names = TRUE,
    recursive = TRUE
  )

}
apply_ox_walmsley <- function(vct_ox_input,
                              fdr_write,
                              fdr_log,
                              log_prefix = "",
                              my_tz,
                              ...) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  # Check if files were already created from a previous run of the pipeline.
  vct_fnm_write <-
    vct_ox_input |>
    gsub(x = _,
         pattern = '"',
         replacement = "") |>
    basename() |>
    file_path_sans_ext()
  vct_fpa_write <-
    vct_fnm_write |>
    paste0(... = _, "-timeSeries\\.csv\\.gz",
           collapse = "|") |>
    list.files(path = fdr_write,
               pattern = _,
               full.names = TRUE,
               recursive = TRUE)
  chk_write <- length(vct_fpa_write) > 0
  chk_length <- length(vct_fpa_write) == length(vct_ox_input)

  if (chk_write & chk_length) {
    return(vct_fpa_write)
  } else if (chk_write) {
    vct_ox_input <- grep(
      x = vct_ox_input,
      pattern =
        vct_fpa_write |>
        basename() |>
        sub(x = _,
             pattern = "-timeSeries\\.csv\\.gz",
             replacement = "") |>
        paste0(... = _, collapse = "|"),
      value = TRUE,
      invert = TRUE
    )
  }

  if (chk_windows) {

    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WHO_WAVES_accelerometer & ",
        paste0(
          'accProcess ', vct_ox_input, ' -o "', fdr_write, '"',
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
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "walmsley_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "walmsley_err.txt"))
    )

  } else {
    # The `system2` command uses a shell within MacOS and Linux.
    # TODO Ask Kirsten for testing this.
    # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_accelerometer ; accProcess stepcountTest.gt3x -o ~/WAVES/data/stepcount --timeZone America/Chicago
    system2(
      command = "source",
      args = paste(
        paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        "conda activate WHO_WAVES_accelerometer",
        paste0(
          'accProcess ', vct_ox_input, ' -o "', fdr_write, '"',
          " --timeZone ", my_tz,
          collapse = " ; "
        ),
        sep = " ; "
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "walmsley_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "walmsley_err.txt"))
    )
  }

  list.files(
    path = fdr_write,
    pattern = "-timeSeries.csv.gz",
    full.names = TRUE
  )

}
apply_ox_actinet <- function(vct_ox_input,
                             fdr_write,
                             fdr_log,
                             log_prefix = "",
                             ...) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  # Check if files were already created from a previous run of the pipeline.
  vct_fnm_write <-
    vct_ox_input |>
    gsub(x = _,
         pattern = '"',
         replacement = "") |>
    basename() |>
    file_path_sans_ext()
  vct_fpa_write <-
    vct_fnm_write |>
    paste0(... = _, "-timeSeries\\.csv\\.gz",
           collapse = "|") |>
    list.files(path = fdr_write,
               pattern = _,
               full.names = TRUE,
               recursive = TRUE)
  chk_write <- length(vct_fpa_write) > 0
  chk_length <- length(vct_fpa_write) == length(vct_ox_input)

  if (chk_write & chk_length) {
    return(vct_fpa_write)
  } else if (chk_write) {
    vct_ox_input <- grep(
      x = vct_ox_input,
      pattern =
        vct_fpa_write |>
        dirname() |>
        basename() |>
        paste0(... = _, collapse = "|"),
      value = TRUE,
      invert = TRUE
    )
  }

  if (chk_windows) {

    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WHO_WAVES_actinet & ",
        paste0(
          'actinet ', vct_ox_input, ' -o "', fdr_write, '"',
          collapse = " & "
        ) |>
          # file paths to windows style.
          gsub(x = _,
               pattern = "/",
               replacement = "\\\\"),
        collapse = ""
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "actinet_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "actinet_err.txt"))
    )

  } else {
    # The `system2` command uses a shell within MacOS and Linux.
    # TODO Ask Kirsten for testing this.
    # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_actinet ; actinet stepcountTest.gt3x -o ~/WAVES/data/stepcount
    system2(
      command = "source",
      args = paste(
        paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        "conda activate WHO_WAVES_actinet",
        paste0(
          'actinet ', vct_ox_input, ' -o "', fdr_write, '"',
          collapse = " ; "
        ),
        sep = " ; "
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "actinet_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "actinet_err.txt"))
    )
  }

  list.files(
    path = fdr_write,
    pattern = "-timeSeries.csv.gz",
    full.names = TRUE,
    recursive = TRUE
  )

}
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
            intensity_walmsley = rep(intensity, each = 30)
          ),
        # actinet
        fread(
          grep(x = vct_ox_acti,
               pattern = le_fnm,
               value = TRUE),
          sep = ","
        ) |>
          mutate(
            datetime =
              floor_date(time, unit = "seconds") |>
              force_tz(tzone  = my_tz),
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
