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
    select(id, datetime, starts_with("time"), invalid, sleep,
           starts_with("mets"),
           starts_with("intensity"),
           starts_with("steps"),
           starts_with("class")) |>
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
merge_output <- function(vct_nw.sleep,
                         vct_out.raw,
                         vct_out.oak.pre,
                         vct_out.cut,
                         vct_ox,
                         vct_out.ref,
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

  # Only merge files that have gone through all steps and have a reference file.
  vct_fnm <- Reduce(
    intersect,
    x = lapply(
      lst_out[c("nw.sleep", "raw", "oak.pre", "cut", "ox")],
      FUN = names
    )
  )

  # TODO: Work on a method that isn't dependent on id_pt being the same between
  # raw and reference data. Do so where the actual numeric participant ID location
  # for each id_pt string, then search for the numeric ID across sources.
  # Right now, assumes id_pt is exactly the same from all sources of data.
  # mtx_id_location <- matrix(
  #   c(
  #     stri_locate_first(
  #       lst_yaml$ref$pal$id_pt,
  #       regex = "\\\\d"
  #     )[, "start"],
  #     end = stri_locate_last(
  #       lst_yaml$ref$pal$id_pt,
  #       regex = "\\\\d"
  #     )[, "end"]
  #   ),
  #   nrow = 2,
  #   ncol = 2,
  #   byrow = TRUE,
  #   dimnames = list(c("start", "end"),
  #                   NULL)
  # )
  vct_id_ref <- stri_replace(
    basename(vct_out.ref),
    regex       = "(_[^_]*)$",
    replacement = ""
  )
  lst_out$ref <- setNames(
    vct_out.ref,
    vct_id_ref
  )

  # Files that don't have a corresponding reference file.
  # vct_fnm[grep(
  #   x = vct_fnm,
  #   pattern = paste0(vct_id_ref, collapse = "|"),
  #   invert = TRUE
  # )]
  vct_fnm <- vct_fnm[grep(
    x = vct_fnm,
    pattern = paste0(vct_id_ref, collapse = "|")
  )]

  vct_fpa_write <-
    file.path(
      dir_merged,
      paste0(
        # TODO: See above. Uses id_pt from reference.
        stri_extract(vct_fnm,
                     regex = paste0(vct_id_ref, collapse = "|")),
        ".parquet"
      )
    ) |>
    setNames(vct_fnm)
  vct_complete <- vector("logical", length = length(vct_fnm))

  # Merge ----
  for (i in seq_along(vct_fnm)) {

    le_fnm <- vct_fnm[i]
    fpa_write <- vct_fpa_write[i]
    le_id <-
      fpa_write |>
      basename() |>
      file_path_sans_ext()
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
    # Left join first reference output then with non-wear/sleep and cutpoint as
    # they are both based off M$metashort and can just do a down fill for non-wear/sleep.
    left_join(
      read_parquet(lst_out$ref[le_id]) |>
        mutate(id2 = id,
               id  = le_fnm,
               .after = id),
      read_parquet(lst_out$cut[le_fnm]),
      by = join_by(id, datetime)
    ) |>
    left_join(
      read_parquet(lst_out$nw.sleep[le_fnm]),
      by = join_by(id, datetime)
    ) |>
      fill(sleep:invalid,
           .direction = "down") |>
      left_join(
        read_parquet(lst_out$raw[le_fnm]) |> mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
        by = join_by(id, datetime)
      ) |>
      left_join(
        read_parquet(lst_out$oak.pre[le_fnm]) |> mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
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
merge_output_config <- function(vct_nw.sleep,
                                vct_out.raw,
                                vct_out.oak.pre,
                                vct_out.cut,
                                vct_ox) {

  # First full join non-wear and sleep with cutpoint as they are both based off
  # M$metashort and can just do a down fill for non-wear/sleep.
  full_join(
    lapply(vct_out.cut, read_parquet) |>  rbindlist(),
    lapply(vct_nw.sleep, read_parquet) |>  rbindlist(),
    by = join_by(id, datetime)
  ) |>
    fill(sleep:invalid,
         .direction = "down") |>
    # out.raw
    left_join(
      lapply(vct_out.raw, read_parquet) |>
        rbindlist() |>
        mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
      by = join_by(id, datetime)
    ) |>
    # out.oak.pre
    left_join(
      lapply(vct_out.oak.pre, read_parquet) |>
        rbindlist() |>
        mutate(datetime = as.POSIXct(datetime, tz = "UTC")),
      by = join_by(id, datetime)
    ) |>
    # ox
    left_join(
      lapply(vct_ox, read_parquet) |>  rbindlist(),
      by = join_by(id, datetime)
    ) |>
    merge_wrangler() |>
    mutate(
      date =
        with_tz(datetime, tzone = "UTC") |>
        date(),
      time =
        with_tz(datetime, tzone = "UTC") |>
        format("%H:%M:%S%z"),
      .after = datetime
    )

}
