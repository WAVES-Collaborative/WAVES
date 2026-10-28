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
    rename_with(
      .cols = starts_with("intensity_"),
      .fn  = ~stri_replace(
        .x,
        regex = "intensity_",
        replacement = "intensity3_"
      )
    ) |>
    select(
      site, pid, any_of("observation"),
      datetime, starts_with("dttm"),
      date, time,
      invalid, sleep,
      starts_with("domain"),
      starts_with("posture"),
      starts_with("sedtype"),
      starts_with("mets"),
      starts_with("intensity"),
      starts_with("steps"),
      starts_with("class")
    ) |>
    # Make all intensity variables have the same levels
    mutate(across(
      .cols = starts_with("intensity3"),
      .fns = ~factor(.x, levels = c("sedentary", "light", "mvpa", "sleep", "nonwear"))
    )) |>
    mutate(across(
      .cols = starts_with("intensity4"),
      .fns = ~factor(.x, levels = c("sedentary", "light", "moderate", "vigorous", "sleep", "nonwear"))
    ))
}
merge_output <- function(de_id,
                         df_pid,
                         spo_pat_raw,
                         spo_pat_ref,
                         lst_out.ref,
                         vct_nw.sleep,
                         vct_out.raw,
                         vct_out.oak.pre,
                         vct_out.cut,
                         vct_ox,
                         dir_merged,
                         df_start_tz) {

  # Read ----
  # Name vectors with filenames.
  lst_out <-
    lapply(
      list(vct_nw.sleep, vct_out.raw, vct_out.oak.pre, vct_out.cut, vct_ox),
      FUN = \(x) setNames(x, basename(x) |> file_path_sans_ext())
    ) |>
    setNames(c("nw.sleep", "raw", "oak.pre", "cut", "ox"))
  vct_ref_secondary <- c("pal", "pass")[c("pal", "pass") %in% names(df_pid)]

  # # Figure out if (?<obs> ) was included within raw or secondary references
  # # by checking the filenames.
  # # If it was included within raw spo_pat, then there will be 2 underscore separators.
  # # If it was included within secondary ref spo_pat, then there will be 3 underscore
  # # separators.
  # vct_fnm <- Reduce(
  #   intersect,
  #   x = lapply(
  #     lst_out[c("nw.sleep", "raw", "oak.pre", "cut", "ox")],
  #     FUN = names
  #   )
  # )
  # chk_obs_raw <- stri_count(vct_fnm, regex = "_")
  # if (length(vct_ref_secondary) != 0) {
  #   chk_obs_ref <- sapply(
  #     lst_out.ref[vct_ref_secondary],
  #     \(x) all(basename(x) |> stri_count(regex = "_") == 3)
  #   )
  # } else {chk_obs_ref <- FALSE}

  # Figure out if (?<obs> ) was included within the raw or secondary references
  # spo_pat by directly checking spo_pat from lst_yaml. Through this method, it
  # is less likely for unknown errors to pop up if I decide to change how
  # processed ref files are named in the future, or if an underscore is ever
  # included within `site`.
  chk_obs_raw <- stri_detect(spo_pat_raw,
                             regex = "\\(\\?\\<obs\\>")
  if (length(vct_ref_secondary) != 0) {
    vct_chk_obs_ref <-
      stri_detect(spo_pat_ref, regex = "\\(\\?\\<obs\\>") |>
      setNames(vct_ref_secondary)
  } else {
    vct_chk_obs_ref <- FALSE
  }

  if (de_id) {
    vct_cols_fnm_write <- c("site_deid", "pid_deid")
  } else {
    vct_cols_fnm_write <- c("site", "pid")
  }

  if (chk_obs_raw) vct_cols_fnm_write <- c(vct_cols_fnm_write, "observation")

  lst_fpa_write <-
    vector(mode = "list", length = 2) |>
    setNames(c("do", "raw"))

  if ("do" %in% names(lst_out.ref)) {

    # Use direct observations data as "anchor", where all other data, including other
    # reference data, are left_joined to direct observation data. Then merge with
    # algorithm output.
    fs::dir_create(file.path(dir_merged, "ANCHOR_DO"))
    vct_fpa_write <- file.path(
        dir_merged,
        "ANCHOR_DO",
        paste0(
          df_pid[[vct_cols_fnm_write[1]]], "_",
          df_pid[[vct_cols_fnm_write[2]]], "_",
          df_pid$observation,
          ".parquet"
        )
      )
    vct_complete <- vector("logical", length = length(vct_fpa_write))
    lst_df_ref <-
      vector(mode = "list",
             length = length(vct_ref_secondary)) |>
      setNames(vct_ref_secondary)
    vct_prev_regex <-
      character(length = length(vct_ref_secondary)) |>
      setNames(vct_ref_secondary)

    for (i in seq_along(vct_fpa_write)) {

      le_site <- df_pid$site[i]
      le_pid  <- df_pid$pid[i]
      le_obs  <- df_pid$observation[i]
      le_fnm_raw <- df_pid$fnm_write_raw[i]
      fpa_write <- vct_fpa_write[i]

      # Commenting this out for now. Since the repository is constantly being
      # updated, I don't want a user to accidently send over merged data that
      # is incorrect somehow. So if there are upstream changes, always rerun
      # this step.
      # # Check if file was already created from a previous run of the pipeline.
      # if (file.exists(fpa_write)) {
      #   vct_complete[i] <- TRUE
      #   next
      # }

      # Load non-direct observation reference data first so as to not repeatedly
      # load the same file multiple times if (?<obs> ) was not provided (i.e.
      # user has one multiple day file for each direct observation session).
      for (ii in seq_along(vct_chk_obs_ref)) {

        chk_obs_ref <- vct_chk_obs_ref[ii]
        le_ref <- vct_ref_secondary[ii]

        chk_ref <-
          df_pid |>
          dplyr::filter(
            site        == le_site,
            pid         == le_pid,
            observation == le_obs
          ) |>
          pull(all_of(le_ref)) |>
          is.na()

        if (chk_ref) next # No other reference data for pid

        if (chk_obs_ref) {
          le_fnm_ref <- paste(
            le_site, le_pid, le_obs,
            sep = "_"
          )
          lst_df_ref[[le_ref]] <-
            lst_out.ref[[le_ref]] |>
            stri_subset(regex = le_fnm_ref) |>
            read_parquet()
        } else {
          le_fnm_ref <- paste(
            le_site, le_pid,
            sep = "_"
          )

          if (i == 1) {
            lst_df_ref[[le_ref]] <-
              lst_out.ref[[le_ref]] |>
              stri_subset(regex = le_fnm_ref) |>
              read_parquet()
            prev_fnm_ref <- le_fnm_ref
          } else if (prev_fnm_ref != le_fnm_ref) {
            lst_df_ref[[le_ref]] <-
              lst_out.ref[[le_ref]] |>
              stri_subset(regex = le_fnm_ref) |>
              read_parquet()
            prev_fnm_ref <- le_fnm_ref
          }
        }
      }

      # Now merge reference data with direct observation.
      df_merge_ref <-
        lst_out.ref$do |>
        dplyr::filter(
          site        == le_site,
          pid         == le_pid,
          observation == le_obs
        )

      for (ii in seq_along(lst_df_ref)) {
        if (length(lst_df_ref[[ii]]) == 0) next

        df_merge_ref <-
          left_join(
            df_merge_ref,
            lst_df_ref[[ii]]
          ) |>
          suppressMessages()
      }

      # Now the rest of the output.
      df_merge_ref |>
        # Left join non-wear/sleep and cutpoint as they are both based off
        # M$metashort and can just do a down fill for non-wear/sleep.
        left_join(
          read_parquet(lst_out$cut[le_fnm_raw]),
          by = join_by(datetime)
        ) |>
        left_join(
          read_parquet(lst_out$nw.sleep[le_fnm_raw]),
          by = join_by(datetime)
        ) |>
        fill(sleep:invalid,
             .direction = "down") |>
        left_join(
          read_parquet(lst_out$raw[le_fnm_raw]) |> mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
          by = join_by(datetime)
        ) |>
        left_join(
          read_parquet(lst_out$oak.pre[le_fnm_raw]) |> mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
          by = join_by(datetime)
        ) |>
        left_join(
          read_parquet(lst_out$ox[le_fnm_raw]),
          by = join_by(datetime)
        ) |>
        # Update site and pid with de_identified one if necessary.
        mutate(
          site = df_pid[[vct_cols_fnm_write[1]]][i],
          pid  = df_pid[[vct_cols_fnm_write[2]]][i]
        ) |>
        # wrangle
        merge_wrangler() |>
        # write
        write_parquet(sink = fpa_write)
      vct_complete[i] <- TRUE

    }

    lst_fpa_write[["do"]] <- vct_fpa_write[vct_complete]

  }

  # Only do another merge if:
  # 1) Direct observation data was provided AND there are entries with secondary
  #    reference data remaining after removing entries with direct observation
  #    data. Have to do it this way as its possible an "observation" session is
  #    actually the free-living "visit", in which case it is assumed there would
  #    be no direct observation data.
  # 2) The (?<obs> ) pattern was NOT included within the raw AND secondary
  #    reference spo_pat, regardless if direct observation data was provided or
  #    not.

  if ("do" %in% names(lst_out.ref)) {
    if (!chk_obs_raw && any(!vct_chk_obs_ref)) {
      df_no_do <-
        df_pid |>
        select(-any_of(c("observation", "do"))) |>
        distinct()
      chk_no_do <- FALSE
    } else {
      df_no_do <-
        df_pid |>
        dplyr::filter(is.na(do))
      chk_no_do <- nrow(df_no_do) != 0
    }
  } else {
    df_no_do  <- df_pid
    chk_no_do <- FALSE
  }

  if (chk_no_do ||
      (!chk_obs_raw && any(!vct_chk_obs_ref))) {

    # Use wrist accelerometer data as "anchor", where all other data, including other
    # reference data, are left_joined to wrist accelerometer data.

    fs::dir_create(file.path(dir_merged, "ANCHOR_RAW"))
    vct_fpa_write <-
      file.path(
        dir_merged,
        "ANCHOR_RAW",
        paste0(
          df_no_do |>
            unite(
              col = fnm,
              all_of(vct_cols_fnm_write),
              sep = "_"
            ) |>
            pull(fnm),
          ".parquet"
          )
      )

    vct_complete <- vector("logical", length = length(vct_fpa_write))

    # Merge ----
    for (i in seq_along(vct_fpa_write)) {

      le_fnm_raw <- df_pid$fnm_write_raw[i]
      fpa_write  <- vct_fpa_write[i]
      lst_start_tz <-
        df_start_tz |>
        dplyr::filter(fnm == le_fnm_raw) |>
        as.list()

      # Commenting this out for now. Since the repository is constantly being
      # updated, I don't want a user to accidently send over merged data that
      # is incorrect somehow. So if there are upstream changes, always rerun
      # this step.
      # # Check if file was already created from a previous run of the pipeline.
      # if (file.exists(fpa_write)) {
      #   vct_complete[i] <- TRUE
      #   next
      # }

      cnt_read <- 0

      ## Merge reference data together ----
      for (ii in seq_along(vct_ref_secondary)) {

        chk_obs_ref <- vct_chk_obs_ref[ii]
        le_ref <- vct_ref_secondary[ii]

        if (chk_obs_ref) {
          le_fnm_ref <- paste(
            df_no_do$site[i],
            df_no_do$pid[i],
            df_no_do$observation[i],
            sep = "_"
          )
        } else {
          le_fnm_ref <- paste(
            df_no_do$site[i],
            df_no_do$pid[i],
            sep = "_"
          )
        }

        if (is.na(df_no_do[[le_ref]][i])) next

        if (cnt_read == 0) {
          df_ref <-
            lst_out.ref[[le_ref]] |>
            stri_subset(regex = le_fnm_ref) |>
            read_parquet()
        } else {
          df_ref <- full_join(
            df_ref,
            lst_out.ref[[le_ref]] |>
              stri_subset(regex = le_fnm_ref) |>
              read_parquet(),
            by = datetime
          )
        }

        cnt_read <- cnt_read + 1

      }

      # read & merge
      # Left join first reference output then with non-wear/sleep and cutpoint as
      # they are both based off M$metashort and can just do a down fill for non-wear/sleep.
      left_join(
        read_parquet(lst_out$cut[le_fnm_raw]),
        read_parquet(lst_out$nw.sleep[le_fnm_raw]),
        by = join_by(datetime)
      ) |>
        fill(sleep:invalid,
             .direction = "down") |>
        left_join(
          read_parquet(lst_out$raw[le_fnm_raw]) |> mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
          by = join_by(datetime)
        ) |>
        left_join(
          read_parquet(lst_out$oak.pre[le_fnm_raw]) |> mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
          by = join_by(datetime)
        ) |>
        left_join(
          read_parquet(lst_out$ox[le_fnm_raw]),
          by = join_by(datetime)
        ) |>
        left_join(
          df_ref,
          by = join_by(datetime)
        ) |>
        # add identifying information, date column and time column in timezone data was collected.
        mutate(
          site = df_no_do[[vct_cols_fnm_write[1]]][i],
          pid  = df_no_do[[vct_cols_fnm_write[2]]][i],
          if (chk_obs_raw) {observation =  df_no_do$observation[i]},
          date =
            with_tz(datetime, tzone = lst_start_tz$tz) |>
            date(),
          time =
            with_tz(datetime, tzone = lst_start_tz$tz) |>
            format("%H:%M:%S%z")
        ) |>
        # wrangle
        merge_wrangler() |>
        # write
        write_parquet(sink = fpa_write)
      vct_complete[i] <- TRUE

    }

    lst_fpa_write[["raw"]] <- vct_fpa_write[vct_complete]

  }

  return(unlist(lst_fpa_write))

}
pre_merge_config <- function(fpa) {
  fpa |>
    read_parquet() |>
    mutate(
      site = "WAVES",
      pid =
        basename(fpa) |>
        stri_extract(regex = "\\d+") |>
        as.integer(),
      .before = 1
    )
}
merge_output_config <- function(vct_nw.sleep,
                                vct_out.raw,
                                vct_out.oak.pre,
                                vct_out.cut,
                                vct_ox) {

  # First full join non-wear and sleep with cutpoint as they are both based off
  # M$metashort and can just do a down fill for non-wear/sleep.
  full_join(
    lapply(vct_out.cut, pre_merge_config) |>  rbindlist(),
    lapply(vct_nw.sleep, pre_merge_config) |>  rbindlist(),
    by = join_by(site, pid, datetime)
  ) |>
    fill(sleep:invalid,
         .direction = "down") |>
    # out.raw
    left_join(
      lapply(vct_out.raw, pre_merge_config) |>
        rbindlist() |>
        mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
      by = join_by(site, pid, datetime)
    ) |>
    # out.oak.pre
    left_join(
      lapply(vct_out.oak.pre, pre_merge_config) |>
        rbindlist() |>
        mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
      by = join_by(site, pid, datetime)
    ) |>
    # ox
    left_join(
      lapply(vct_ox, pre_merge_config) |>  rbindlist(),
      by = join_by(site, pid, datetime)
    ) |>
    mutate(
      date =
        with_tz(datetime, tzone = "UTC") |>
        date(),
      time =
        with_tz(datetime, tzone = "UTC") |>
        format("%H:%M:%S%z"),
      .after = datetime
    ) |>
    merge_wrangler()

}
