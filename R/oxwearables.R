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
apply_ox_stepcount <- function(ox_input,
                               fdr_write,
                               fdr_log,
                               log_prefix = "",
                               lst_miniconda) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  # Check if files were already created from a previous run of the pipeline.
  fnm_write <-
    ox_input |>
    gsub(x = _,
         pattern = '"',
         replacement = "") |>
    basename() |>
    file_path_sans_ext()
  fpa_write <- file.path(
    fdr_write, fnm_write,
    paste0(fnm_write, "-StepTimes.csv.gz")
  )

  if (file.exists(fpa_write)) return(fpa_write)

  if (chk_windows) {
    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WAVES_stepcount & ",
        paste0(
          'stepcount ', ox_input, ' -o "', fdr_write, '"'
        ) |>
          # file paths to windows style.
          gsub(x = _,
               pattern = "/",
               replacement = "\\\\"),
        collapse = ""
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "stepcount_", fnm_write, "_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "stepcount_", fnm_write, "_err.txt"))
    )
  } else {
    # The `system2` command uses a shell within MacOS and Linux.
    # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WAVES_stepcount ; stepcount "data/0_CONFIG/RAW/WAVES_10004_RAW.gt3x" -o ~/WAVES/data/stepcount
    system2(
      command = "source",
      args = paste(
        paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        "conda activate WAVES_stepcount",
        paste0(
          'stepcount ', ox_input, ' -o "', fdr_write, '"'
        ),
        sep = " ; "
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "stepcount_", fnm_write, "_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "stepcount_", fnm_write, "_err.txt"))
    )
  }

  if (file.exists(fpa_write)) {
    return(fpa_write)
  } else {
    cli::cli_abort(c(
      paste0(fnm_write, " did not produce output."),
      "i" = paste0(
        "Please check ", file.path(fdr_log, paste0(log_prefix, "stepcount_", fnm_write, "_err.txt")),
        " and report to WAVES team."
      )
    ))
  }

}
apply_ox_walmsley <- function(ox_input,
                              fdr_write,
                              fdr_log,
                              log_prefix = "",
                              lst_miniconda) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  # Check if files were already created from a previous run of the pipeline.
  fnm_write <-
    ox_input |>
    gsub(x = _,
         pattern = '"',
         replacement = "") |>
    basename() |>
    file_path_sans_ext()
  fpa_write <- file.path(
    fdr_write,
    paste0(fnm_write, "-timeSeries.csv.gz")
  )

  if (file.exists(fpa_write)) return(fpa_write)

  if (chk_windows) {

    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WAVES_accelerometer & ",
        paste0(
          'accProcess ', ox_input, ' -o "', fdr_write, '"', " --timeZone UTC"
        ) |>
          # file paths to windows style.
          gsub(x = _,
               pattern = "/",
               replacement = "\\\\"),
        collapse = ""
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "walmsley_", fnm_write, "_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "walmsley_", fnm_write, "_err.txt"))
    )

  } else {
    # The `system2` command uses a shell within MacOS and Linux.
    # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WAVES_accelerometer ; accProcess "data/0_CONFIG/RAW/WAVES_10004_RAW.gt3x" -o ~/WAVES/data/walmsley --timeZone UTC
    system2(
      command = "source",
      args = paste(
        paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        "conda activate WAVES_accelerometer",
        paste0(
          'accProcess ', ox_input, ' -o "', fdr_write, '"', " --timeZone UTC"
        ),
        sep = " ; "
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "walmsley_", fnm_write, "_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "walmsley_", fnm_write, "_err.txt"))
    )
  }

  if (file.exists(fpa_write)) {
    return(fpa_write)
  } else {
    cli::cli_abort(c(
      paste0(fnm_write, " did not produce output."),
      "i" = paste0(
        "Please check ", file.path(fdr_log, paste0(log_prefix, "stepcount_", fnm_write, "_err.txt")),
        " and report to WAVES team."
      )
    ))
  }

}
apply_ox_actinet <- function(ox_input,
                             fdr_write,
                             fdr_log,
                             log_prefix = "",
                             lst_miniconda) {

  chk_windows <- grepl(
    x = Sys.getenv("OS"),
    pattern = "windows",
    ignore.case = TRUE
  )

  # Check if files were already created from a previous run of the pipeline.
  fnm_write <-
    ox_input |>
    gsub(x = _,
         pattern = '"',
         replacement = "") |>
    basename() |>
    file_path_sans_ext()
  fpa_write <- file.path(
    fdr_write, fnm_write,
    paste0(fnm_write, "-timeSeries.csv.gz")
  )

  if (file.exists(fpa_write)) return(fpa_write)

  if (chk_windows) {

    # Run activate.bat which is what is activated when "Anaconda Prompt" runs.
    system2(
      command = file.path(miniconda_path(), "Scripts", "activate.bat"),
      args = paste0(
        "activate WAVES_actinet & ",
        paste0(
          'actinet ', ox_input, ' -o "', fdr_write, '"'
        ) |>
          # file paths to windows style.
          gsub(x = _,
               pattern = "/",
               replacement = "\\\\"),
        collapse = ""
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "actinet_", fnm_write, "_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "actinet_", fnm_write, "_err.txt"))
    )

  } else {
    # The `system2` command uses a shell within MacOS and Linux.
    # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WAVES_actinet ; actinet "data/0_CONFIG/RAW/WAVES_10004_RAW.gt3x" -o ~/WAVES/data/actinet
    system2(
      command = "source",
      args = paste(
        paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        "conda activate WAVES_actinet",
        paste0(
          'actinet ', ox_input, ' -o "', fdr_write, '"'
        ),
        sep = " ; "
      ),
      stdout = file.path(fdr_log, paste0(log_prefix, "actinet_", fnm_write, "_out.txt")),
      stderr = file.path(fdr_log, paste0(log_prefix, "actinet_", fnm_write, "_err.txt"))
    )
  }

  if (file.exists(fpa_write)) {
    return(fpa_write)
  } else {
    cli::cli_abort(c(
      paste0(fnm_write, " did not produce output."),
      "i" = paste0(
        "Please check ", file.path(fdr_log, paste0(log_prefix, "stepcount_", fnm_write, "_err.txt")),
        " and report to WAVES team."
      )
    ))
  }

}
merge_ox <- function(vct_ox_step,
                     vct_ox_wlms,
                     vct_ox_acti,
                     dir_write,
                     vct_raw_type,
                     df_start_tz) {

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
  vct_fpa_write <- file.path(
    dir_write, paste0(vct_fnm, ".parquet")
  )
  vct_complete <- vector("logical", length = length(vct_fnm))

  for (i in seq_along(vct_fnm)) {

    le_fnm    <- vct_fnm[i]
    fpa_write <- vct_fpa_write[i]
    le_type   <- vct_raw_type[le_fnm]

    if (file.exists(fpa_write)) {
      vct_complete[i] <- TRUE
      next
    }

    # walmsley ----
    df_wlms <-
      fread(
        grep(x = vct_ox_wlms,
             pattern = stringr::str_escape(le_fnm),
             value = TRUE),
        sep = ","
      ) |>
      mutate(
        # If the file was .gt3x, then the time, when read by accProcess, will
        # be treated as local time and then changed to UTC. This does not
        # happen for .bin and .cwa files.
        datetime =
          if (le_type %in% c("ACTIGRAPH - GT3X", "ACTIGRAPH - CSV")) {
            seq.POSIXt(
              from =
                (ymd_hms(time)[1] |>
                   floor_date(unit = "seconds")) +
                df_start_tz |>
                dplyr::filter(fnm == le_fnm) |>
                pull(offset) * 60 * 60,
              length.out = n(),
              by = "30 sec"
            )
          } else {
            ymd_hms(time, tz = "UTC", quiet = TRUE) |>
              floor_date(unit = "seconds")
          },
        intensity = case_when(
          sedentary == 1 ~ "sedentary",
          light == 1     ~ "light",
          `moderate-vigorous` == 1 ~ "mvpa",
          sleep == 1 ~ "sleep",
          .default = NA
        ),
        .keep = "none"
      ) |>
      # second-by-second
      reframe(
        datetime = seq.POSIXt(
          from = datetime[1],
          to = last(datetime) + 29,
          by = "1 sec"
        ),
        intensity_walmsley = rep(intensity, each = 30)
      )

    # actinet ----
    df_acti <-
      fread(
        grep(x = vct_ox_acti,
             pattern = stringr::str_escape(le_fnm),
             value = TRUE),
        sep = ","
      ) |>
      mutate(
        # time column when read with fread() function is automatically UTC
        # and actinet already exports in UTC.
        datetime = floor_date(time, unit = "seconds"),
        intensity = case_when(
          # actinet will be NA for epochs that had idle-sleep mode on. Just
          # make them sedentary as we will use nonwear/sleep from other methods.
          is.na(acc) ~ "sedentary",
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

    # stepcount ----
    df_step <-
      fread(
        grep(x = vct_ox_step,
             pattern = stringr::str_escape(le_fnm),
             value = TRUE),
        sep = ","
      ) |>
      mutate(
        # time column when read with fread() function is automatically UTC
        # and stepcount already exports in UTC. Contains fractional seconds
        # though so floor it to the nearest second
        datetime =
          floor_date(time, unit = "seconds")
      ) |>
      summarise(
        steps_stepcount = as.integer(n()),
        .by = datetime
      )

    # return ----
    # Full join walmsley and actinet output (should always be a complete match,
    # as in there is no difference with performing a inner join), then left join
    # with stepcount output (since stepcount output is not complete time series).
    # Ok, I lied about walmsley and actinet always being a complete match.
    # Actinet will sometimes (always?) have one epoch longer than walmsley. BUT
    # in the grand scheme of things, the very last bit of data will not be used.
    # So just use a full join initially and when merged with all the other data
    # it will be fixed.
    full_join(
      df_wlms,
      df_acti,
      by = join_by(datetime)
    ) |>
      left_join(
        df_step,
        by = join_by(datetime)
      ) |>
      mutate(
        id = le_fnm,
        steps_stepcount = replace_na(steps_stepcount, replace = 0L),
        .before = 1
      ) |>
      arrow::write_parquet(sink = fpa_write)

    vct_complete[i] <- TRUE

  }

  return(vct_fpa_write[vct_complete])

}
