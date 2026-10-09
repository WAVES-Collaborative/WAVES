process_meta_sup <- function(meta_fpa,
                             de_id,
                             site) {

  df <- fread(meta_fpa)

  # CHECK
  names(df) <-
    names(df) |>
    tolower()
  chk_names <-
    c("site", "pid", "age", "bmi", "gender", "device", "sampling", "location", "firmware") %in%
    names(df)

  if (!all(chk_names)) {
    # TODO: clean up and make as rlang::abort()
    stop("metadata variable missing or named incorrectly")
  }

  # TODO add more checks to make sure variables are numeric when needed and values in
  # gender, device, and location match required levels.

  if (de_id) {

    df$site <- site
    df$pid <-
      sample(
        seq_along(df$subject),
        size    = nrow(df),
        replace = FALSE
      ) |>
      formatC(
        width = 4,
        format = "d",
        flag = "0"
      )

  } else {

    df$site <- df$study
    df$pid <- df$subject

  }

  return(df)

}
process_de_id <- function(df_meta) {

  vct_id <- paste0(df_meta$study, "_", df_meta$subject)
  names(vct_id) <- paste0(df_meta$site, "_", df_meta$pid)

  return(vct_id)

}
process_meta_ref <- function(lst_chk) {

  lst_chk <- lst_chk[lengths(lst_chk) != 0]
  lst_meta <-
    vector(mode = "list",
           length = length(lst_chk)) |>
    setNames(names(lst_chk))

  for (i in seq_along(lst_chk)) {

    le_ref <- names(lst_chk)[i]

    if (le_ref %in% c("do", "img", "pal", "pass")) {
      lst_meta[[le_ref]] <- switch(
        le_ref,
        "do"  = process_meta_do(lst_chk$do),
        "img" = {}, # TODO
        "pal" = {
          purrr::pmap(
            lst_chk$pal,
            \(id, fpa_epoch, fpa_event) {tryCatch(
              process_meta_ap(id,
                              fpa_epoch,
                              fpa_event),
              error = \(cnd) {cnd}
            )}
          )
        },
        "pass" = {} # TODO
      )
    } else if (le_chk) {
      # TODO: Flush out more if people want to add own reference processing
      # functions
      # lst_meta[[le_ref]] <- lst_yaml$ref[[le_ref]]$process_meta_function(
      #   lst_param <- lst_yaml$ref[[le_ref]][
      #     !names(lst_yaml$ref[[le_ref]]) %in% c("process_meta_function",
      #                                           "process_file_function")
      #   ]
      # )
    }

  }

  return(lst_meta)

}
OLDprocess_meta_ref <- function(lst_yaml,
                                dir_meta) {

  vct_chk <-
    lapply(
      lst_yaml$ref,
      \(x) {
        !all(simplify_is_null(x))
      }
    ) |>
    unlist(use.names = TRUE)

  lst_meta <-
    vector(mode = "list",
           length = length(vct_chk)) |>
    setNames(names(vct_chk))

  for (i in seq_along(vct_chk)) {

    le_chk <- vct_chk[i]
    le_ref <- names(vct_chk)[i]

    if (le_chk &&
        le_ref %in% c("do", "img", "pal", "pass")) {
      lst_meta[[le_ref]] <- switch(
        le_ref,
        "do"  = process_meta_do(
          fpa_do   = lst_yaml$ref$do$fpa,
          dir_meta = dir_meta
        ),
        "img" = {}, # TODO
        "pal" = {
          le_out <- tryCatch(
            process_meta_ap(
              vct_epoch = lst_yaml$ref$pal$palp_fpa,
              vct_event = lst_yaml$ref$pal$palv_fpa,
              id_pt     = lst_yaml$ref$pal$id_pt
            ),
            error = \(e) e
          )
          if (class(le_out)[1] != "simpleError") {
            le_out
          } else {
            cli::cli_abort(
              message = "process_meta_ap errored",
              call = le_out$call
            )
          }
        },
        "pass" = {} # TODO
      )
    } else if (le_chk) {
      # TODO: Flush out more if people want to add own reference processing
      # functions
      # lst_meta[[le_ref]] <- lst_yaml$ref[[le_ref]]$process_meta_function(
      #   lst_param <- lst_yaml$ref[[le_ref]][
      #     !names(lst_yaml$ref[[le_ref]]) %in% c("process_meta_function",
      #                                           "process_file_function")
      #   ]
      # )
    }

  }



  unlist(lst_meta)

}
process_meta_do <- function(fpa_do) {

  df <- fread(fpa_do)
  names(df) <-
    names(df) |>
    tolower()

  df |>
    mutate(
      # date = lubridate::mdy(date),
      # datetime =
      #   paste0(date, "_", time) |>
      #   ymd_hms(tz = "America/Los_Angeles") |>
      #   with_tz(tzone = "UTC"),
      across(
        .cols = c(domain_do, posture_do, intensity3_do, intensity4_do, sedtype_do),
        .fns = tolower
      )
    ) |>
    summarise(
      duration_min   = difftime(
        time1 = last(datetime),
        time2 = first(datetime),
        units = "mins"
      ),
      chk_domain     = all(domain_do %in% c("household", "leisure", "occupation", "transportation", "other")),
      chk_posture    = all(posture_do %in% c("sedentary", "mixed_movement", "walking", "biking", "running")),
      chk_sedtype    = all(sedtype_do %in% c("non_sedentary", "sitting", "lying", "vehicle")),
      chk_intensity3 = all(intensity3_do %in% c("sedentary", "light", "mvpa", "non_codable")),
      chk_intensity4 = all(intensity4_do %in% c("sedentary", "light", "moderate", "vigorous", "non_codable")),
      chk_steps      = is.integer(steps_do),
      .by = c(study, subject, observation)
    )
    # arrow::write_parquet(sink = fpa_write)

  # return(fpa_write)

}
extract_pal_header <- function(fpa) {
  lst_header <-
    fread(
      fpa,
      sep    = ";",
      nrows  = 10,
      skip   = 1,
      header = FALSE
    ) |>
    data.table::transpose(
      make.names = "V1"
    ) |>
    as.list()

  # Check if header was included.
  chk_header <- any(
    !names(lst_header) %in% c("validation_algorithm_wear_time_protocol",
                              "analysis_algorithm_minimum_upright_seconds",
                              "analysis_algorithm_minimum_non_upright_seconds")
  )
  if (chk_header) {

    cli::cli_abort(
      message = c(
        "{fpa} was not exported with a header."
      ),
      class = "no_header",
      call = rlang::caller_env()
    )

  }
  lst_header$validation_algorithm_wear_time_protocol <- as.integer(
    lst_header$validation_algorithm_wear_time_protocol
  )
  lst_header$analysis_algorithm_minimum_upright_seconds <- as.integer(
    lst_header$analysis_algorithm_minimum_upright_seconds
  )
  lst_header$analysis_algorithm_minimum_non_upright_seconds <- as.integer(
    lst_header$analysis_algorithm_minimum_non_upright_seconds
  )
  lst_header$serial <- stri_extract(basename(fpa), regex = "AP\\d{6}")
  return(lst_header)
}
process_meta_ap <- function(le_id,
                            fpa_epoch,
                            fpa_event) {

  lst_header_epoch <- tryCatch(
    extract_pal_header(fpa_epoch),
    rlang_error = \(cnd) {cnd},
    error = \(cnd) {rlang::cnd_entrace(cnd)}

  )
  lst_header_event <- rlang::try_fetch(
    extract_pal_header(fpa_event),
    rlang_error = \(cnd) {cnd},
    error = \(cnd) {rlang::cnd_entrace(cnd)}
  )

  chk_epoch_err <- inherits(lst_header_epoch, "error")
  chk_event_err <- inherits(lst_header_event, "error")

  if (chk_epoch_err || chk_event_err) {

    # expected error or not?
    chk_epoch_no_header <- inherits(lst_header_epoch, "no_header")
    chk_event_no_header <- inherits(lst_header_event, "no_header")
    msg_epoch <-
      if (chk_epoch_no_header) (
        c(
          "x" = "Epoch file `{basename(fpa_epoch)}` was not exported with a header.",
          "i" = "Please reference WAVES documentation for how to export activPAL data from PAL Batch software."
        )
      ) else if (chk_epoch_err) {
        c(
          "x" = "Unepected error with epoch file `{basename(fpa_epoch)}`.",
          "!" = conditionMessage(lst_header_epoch),
          "i" = "Please contact WAVES team for troubleshooting."
        )
      } else {
        character()
      }
    msg_event <-
      if (chk_event_no_header) (
        c(
          "x" = "Event file `{basename(fpa_event)}` was not exported with a header.",
          "i" = "Please reference WAVES documentation for how to export activPAL data from PAL Batch software."
        )
      ) else if (chk_event_err) {
        c(
          "x" = "Unepected error with event file `{basename(fpa_event)}`:",
          "!" = conditionMessage(lst_header_event),
          "i" = "Please contact WAVES team for troubleshooting."
        )
      } else {
        character()
      }

    if (chk_epoch_no_header && chk_event_no_header) {
      # Since both have no header, show epoch
      le_condition <- lst_header_epoch
    } else if (chk_epoch_no_header && !chk_event_no_header) {
      le_condition <- lst_header_event
    } else if (!chk_epoch_no_header && chk_event_no_header) {
      le_condition <- lst_header_epoch
    } else {
      # show traceback for event and in message include traceback for epoch file.
      le_condition <- lst_header_event
      msg_epoch <- c(
        msg_epoch[1:2],
        "i" = "Traceback:",
        lst_header_epoch$trace,
        msg_epoch[3]
      )
    }

    cli::cli_abort(
      message = c(
        "ID `{le_id}`:",
        msg_epoch,
        msg_event
      ),
      parent = le_condition,
      call   = rlang::caller_env()
    )

  }

  chk_pal <-
    !all(unlist(lst_header_epoch) == unlist(lst_header_event))

  if (chk_pal) {
    vct_different <-
      data.frame(
        epoch  = unlist(
          lst_header_epoch[unlist(lst_header_epoch) != unlist(lst_header_event)]
        ),
        event  = unlist(
          lst_header_event[unlist(lst_header_epoch) != unlist(lst_header_event)]
        )
      ) |>
      rownames_to_column(var = "option") |>
      mutate(msg = paste0(
        option, ": epoch=", epoch, "; event=", event, "."
      )) |>
      pull(msg)
    cli::cli_abort(
      message = c(
        "{le_id} epoch and event files not processed with same software version or options.",
        "i" = "Recommend processing both files with exact same software version and options.",
        "i" = "The following options differ:",
        vct_different
      ),
      call = rlang::caller_env()
    )
  }

  return(lst_header_epoch)

}
