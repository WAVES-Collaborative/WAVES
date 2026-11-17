process_ref_file <- function(lst_chk,
                             my_tz,
                             dir_out.ref) {

  lst_out.ref <-
    vector(mode = "list",
           length = length(lst_chk)) |>
    setNames(names(lst_chk))

  for (i in seq_along(lst_chk)) {

    le_ref <- names(lst_chk)[i]

    if (le_ref %in% c("do", "img", "pal", "pass")) {
      lst_out.ref[[le_ref]] <- switch(
        le_ref,
        "do"  = {lst_chk$do}, # No additional processing needed for DO reference files
        "img" = {lst_chk$img}, # No additional processing needed for IMG reference files
        "pal" = {
          lst_out <- purrr::pmap(
            lst_chk$pal,
            \(spo, fnm, fpa_epoch, fpa_event, ...) {rlang::try_fetch(
              # spo       = lst_chk$pal$spo[1]
              # fnm       = lst_chk$pal$fnm[1]
              # fpa_epoch = lst_chk$pal$fpa_epoch[1]
              # fpa_event = lst_chk$pal$fpa_event[1]
              process_activpal_file(spo,
                                    fnm,
                                    fpa_epoch,
                                    fpa_event,
                                    my_tz,
                                    dir_out.ref),
              error = \(cnd) {cnd}
            )}
          )
          chk_err <- any(sapply(lst_out, inherits, "error"))

          if (chk_err) {

            lst_error <- lst_out[sapply(lst_out, inherits, "error")]
            vct_class <- sapply(lst_out, \(x) class(x)[1])
            chk_same <- uniqueN(vct_class) == 1

            if (chk_same) {
              cli::cli_abort(
                message = c(
                  "Reference activPAL: Some files errored, with all errors being the same.",
                  "i" = "Below is one of the errors thrown out of {length(lst_error)} errors:"
                ),
                parent = lst_error[[1]],
                call   = rlang::caller_env()
              )
            } else if (length(lst_error) <= 5) {
              # If its <= 5 errors, display them.
              cli::cli_abort(
                message = c(
                  "Reference activPAL: Some files errored.",
                  "i" = "Below are the errors:",
                  lapply(lst_error, \(x) c(x$message, x$body, " " = "")) |> unlist()
                ),
                call   = rlang::caller_env()
              )
            } else {
              vct_error_type <- unique(vct_class)
              ind_error <- which(!duplicated(vct_class))
              lst_message <- vector(mode = "list", length = length(ind_error))
              for (i in seq_along(ind_error)) {
                lst_message[[i]] <- c(
                  ">" = paste0("Error class `", vct_error_type[i], "`:"),
                  lst_error[[ind_error[i]]]$message,
                  lst_error[[ind_error[i]]]$body,
                  " " = ""
                )
              }
              cli::cli_abort(
                message = c(
                  "Reference activPAL: Some files errored.",
                  "i" = "Because multiple types of error were found, only one of each error type are listed below:",
                  unlist(lst_message)
                ),
                call   = rlang::caller_env()
              )
            }
          } else {
            unlist(lst_out)
          }
        },
        "pass" = {} # TODO
      )
    } else {
      # TODO: Flush out more if people want to add own reference processing
      # functions
      # lst_yaml$ref[[le_ref]]$process_file_function(
      #   lst_param <- lst_yaml$ref[[le_ref]][
      #     !names(lst_yaml$ref[[le_ref]]) %in% c("process_meta_function",
      #                                           "process_file_function")
      #   ]
      # )
    }
  }

  return(lst_out.ref)

}
process_activpal_file <- function(spo,
                                  fnm,
                                  fpa_epoch,
                                  fpa_event,
                                  my_tz,
                                  dir_out.ref) {

  # EPOCH --------------------------------------------------------------------
  df_epoch <-
    fread(
      fpa_epoch,
      sep    = ";",
      header = TRUE,
      skip   = 12
    ) |>
    mutate(
      datetime =
        `Time(approx)` |>
        force_tz(tzone = my_tz) |>
        with_tz(tzone = "UTC"),
      mets_palp  = `Activity Score (MET.s)`,
      sed_time = `Sedentary Time (s)`,
      upr_time = `Upright Time (s)`,
      stp_time = `Stepping Time (s)`,
      cyc_time = `Cycling Time (s)`,
      ply_time = `Primary Lying Time (s)`,
      sly_time = `Secondary Lying Time (s)`,
      non_time = `Nonwear Time (s)`,
      tra_time = `Seated Transport Time (s)`,
      err_time = `Data Errors (s)`,
      intensity4_palp =
        case_when(
          non_time >= 0.5   ~ "non-wear",
          err_time >= 0.5   ~ "EMPTY",
          mets_palp == 0.0  ~ NA_character_,
          (sed_time >= 0.5 | ply_time >= 0.5 | sly_time >= 0.5) &
            mets_palp < 2.0 ~ "sedentary",
          mets_palp < 3.0   ~ "light",
          mets_palp < 6.0   ~ "moderate",
          mets_palp >= 6.0  ~ "vigorous",
          .default          = "CHECK ME"
        ) |>
        # For some reason, mets will be 0 at times even while activPAL is still
        # being worn. Just do a downward fill. TODO: This will fill in the non-wear times as well! Need to account for it in case_when
        # by making it a dummy value like "non-wear", do down fill, then make that value as NA.
        vctrs::vec_fill_missing(direction = "down") |>
        replace_values(
          "non-wear" ~ NA_character_,
          "EMPTY"    ~ NA_character_
        ) |>
        factor(
          levels = c("sedentary",
                     "light",
                     "moderate",
                     "vigorous",
                     "CHECK ME")
        ),
      intensity3_palp = factor(
        intensity4_palp,
        levels = c("sedentary",
                   "light",
                   "moderate",
                   "vigorous",
                   "CHECK ME"),
        labels = c("sedentary",
                   "light",
                   "mvpa",
                   "mvpa",
                   "CHECK ME")
      ),
      steps_palp = StepCount,
      .keep = "none"
    )

  if (any(df_epoch$intensity3_palp == "CHECK ME", na.rm = TRUE)) {
    cli::cli_abort(
      message = c(
        "`process_activpal` function:",
        "x" = "Epoch file intensity variable has a {.val CHECK ME} value",
        "i" = "ID: {spo}",
        "i" = "File: {basename(fpa_event)}"
      ),
      class = "waves_process_pal_epoch",
      call   = rlang::caller_env()
    )
  } else {
    df_epoch$intensity4_palp <- factor(
      df_epoch$intensity4_palp,
      levels = c("sedentary",
                 "light",
                 "moderate",
                 "vigorous")
    )
    df_epoch$intensity3_palp <- factor(
      df_epoch$intensity3_palp,
      levels = c("sedentary",
                 "light",
                 "mvpa")
    )
  }

  # EVENT --------------------------------------------------------------------
  df_event <-
    fread(
      fpa_event,
      sep    = ";",
      header = TRUE,
      skip   = 14,
      fill   = TRUE
    ) |>
    mutate(
      # Time variable is number of days since 1899-12-30.
      # 1) Change to seconds since 1899-12-30 (multiply by 60 * 60 * 24),
      # 2) convert to POSIXct,
      # 3) change to seconds since 1970-01-01,
      # 4) convert back to POSIXct,
      # 5) include floor_date to truncate seconds since `Time(approx)` variable
      #    goes off rounded time.
      # 6) force correct timezone
      # 7) convert back to POSIXct
      datetime =
        (Time * 86400) |>
        as.POSIXct(tz = "UTC", origin = "1899-12-30") |>
        as.numeric() |>
        as.POSIXct(tz = "UTC") |>
        floor_date(unit = "seconds") |>
        force_tz(tzone = my_tz) |>
        with_tz(tzone = "UTC"),
      # I don't know if the above method is better than using `Time(approx)`. For
      # now keep it in and from analytic pipeline, see if palv performs better
      # than palp since using `Time(approx)` is about the same as using `Time(approx)`
      # from epoch file.
      dttm_palv =
        `Time(approx)` |>
        force_tz(tzone = my_tz) |>
        with_tz(tzone = "UTC"),
      class_palv = factor(
        x = `Event Type`,
        levels = c(0, 1, 2, 2.1, 3, 3.1, 3.2, 4, 5),
        labels = c("sedentary",
                   "upright",
                   "stepping",
                   "cycling",
                   "lying",
                   "primary lying",
                   "seconday lying",
                   "non_wear",
                   "travelling")
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
      intensity4_palv = case_when(
        class_palv == "non_wear" ~ NA_character_,
        class_palv %in% c("sedentary", "primary lying", "secondary lying") &
          mets_palv < 2.0        ~ "sedentary",
        mets_palv < 3.0          ~ "light",
        mets_palv < 6.0          ~ "moderate",
        mets_palv >= 6.0         ~ "vigorous",
        .default                 = "CHECK ME"
      ) |>
        factor(
          levels = c("sedentary",
                     "light",
                     "moderate",
                     "vigorous",
                     "CHECK ME")
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
      duration = `Duration (s)`,
      .keep = "none"
    ) |>
    # At times, activPAL will have an error where there will be a repeated second
    # with no data. This is when err_time will be > 1 from the epoch file.
    # Just remove these rows.
    filter_out(is.na(class_palv)) |>
    # Remove duplicated datetimes, which occurs when a class occurs for less than
    # a second within a datetime.
    filter_out(duplicated(datetime, fromLast = TRUE))

  if (any(df_event$intensity3_palv == "CHECK ME", na.rm = TRUE)) {
    cli::cli_abort(
      message = c(
        "`process_activpal` function:",
        "x" = "Event file intensity variable has a {.val CHECK ME} value",
        "i" = "ID: {spo}",
        "i" = "File: {basename(fpa_event)}"
      ),
      class = "waves_process_pal_epoch",
      call   = rlang::caller_env()
    )
  } else {
    df_event$intensity4_palv <- factor(
      df_event$intensity4_palv,
      levels = c("sedentary",
                 "light",
                 "moderate",
                 "vigorous")
    )
    df_event$intensity3_palv <- factor(
      df_event$intensity3_palv,
      levels = c("sedentary",
                 "light",
                 "mvpa")
    )
  }

  # MERGE --------------------------------------------------------------------
  df_pal <- full_join(
    df_epoch,
    df_event,
    by = join_by(datetime)
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
      class_palv      = vctrs::vec_fill_missing(class_palv,
                                                direction = "down")
    ) |>
    select(
      datetime, dttm_palv, waking_day,
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
    paste0(fnm, "_", "pal.parquet")
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
