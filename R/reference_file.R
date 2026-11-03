process_ref_file <- function(lst_chk,
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
              process_activpal_file(spo,
                                    fnm,
                                    fpa_epoch,
                                    fpa_event,
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
      dttm_palp = `Time(approx)`,
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
    cli::cli_abort(
      message = c(
        "`process_activpal` function:",
        "x" = "Epoch file has a data error. Double-check events file to see what corresponding {.var class_palv} is.",
        "i" = "Can be fixed, please reach out to WAVES team.",
        "i" = glue::glue("ID: {spo}"),
        "i" = glue::glue("File: {basename(vct_event[fnm])}")
      ),
      class = "waves_process_pal_epoch",
      call   = rlang::caller_env()
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
      datetime =
        (Time * 86400) |>
        as.POSIXct(tz = "UTC", origin = "1899-12-30") |>
        as.numeric() |>
        as.POSIXct(tz = "UTC") |>
        floor_date(unit = "seconds"),
      dttm_palv = `Time(approx)`,
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
    cli::cli_abort(
      message = c(
        "`process_activpal` function:",
        "x" = "Events file has NA in events type column after renaming.",
        "i" = "Can be fixed, please reach out to WAVES team.",
        "i" = "ID: {spo}",
        "i" = "File: {basename(vct_event[fnm])}"
      ),
      class = "waves_process_pal_event",
      call   = rlang::caller_env()
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
      # steps_cum       = vctrs::vec_fill_missing(steps_cum,
      #                                           direction = "down"),
      class_palv      = vctrs::vec_fill_missing(class_palv,
                                                direction = "down")
    ) |>
    select(
      datetime, dttm_palp, dttm_palv, waking_day,
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
