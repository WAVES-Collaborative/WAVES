process_meta_sup <- function(meta_fpa,
                             de_id,
                             site_deid) {

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

    df$site_deid <- site_deid
    df$pid_deid <-
      sample(
        seq_along(df$pid),
        size    = nrow(df),
        replace = FALSE
      ) |>
      formatC(
        width = 4,
        format = "d",
        flag = "0"
      )

  } else {
    df$site_deid <- df$site
    df$pid_deid  <- df$pid
  }

  return(df)

}
create_meta_share <- function(df_meta,
                              de_id,
                              dir_merged) {
  fpa <- file.path(dir_merged, "metadata_for_sharing.parquet")
  if (de_id) {
    df_meta |>
      select(
        site = site_deid,
        pid  = pid_deid,
        age:firmware
      ) |>
      write_parquet(sink = fpa)
  } else {
    df_meta |>
      select(-c(site_deid, pid_deid)) |>
      write_parquet(sink = fpa)
  }
  return(fpa)
}
create_deid_key <- function(df_meta) {
  fpa <- file.path("reports", "DE_IDENTIFY_KEY.csv")
  df_meta |>
    select(site, pid, site_deid, pid_deid) |>
    fwrite(file = fpa)
  return(fpa)
}
process_meta_ref <- function(lst_chk) {

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
          lst_out <- purrr::pmap(
            lst_chk$pal,
            \(spo, fnm, fpa_epoch, fpa_event, ...) {rlang::try_fetch(
              process_meta_ap(spo,
                              fnm,
                              fpa_epoch,
                              fpa_event),
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
            purrr::list_rbind(lst_out)
          }
        },
        "pass" = {} # TODO
      )
    } else {
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
process_meta_do <- function(df) {

  df |>
    mutate(
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
      .by = c(site, pid, observation)
    )

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
    !names(lst_header) %in% c("software_tool_name",
                              "software_tool_version",
                              "validation_algorithm_name",
                              "validation_algorithm_version",
                              "validation_algorithm_wear_time_protocol",
                              "analysis_algorithm_name",
                              "analysis_algorithm_version",
                              "analysis_algorithm_autocorrect_inverted",
                              "analysis_algorithm_minimum_upright_seconds",
                              "analysis_algorithm_minimum_non_upright_seconds")
  )
  if (chk_header) {
    cli::cli_abort(
      message = c(
        "{fpa} was not exported with a header."
      ),
      class = "waves_pal_no_header",
      call = rlang::caller_env()
    )
  }

  # Make usre CREA algorithm was used.
  chk_crea <- lst_header$analysis_algorithm_name != "CREA"
  if (chk_crea) {
    cli::cli_abort(
      message = c(
        "{fpa} was not analyzed with the CREA algorithm."
      ),
      class = "waves_pal_no_crea",
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
  lst_header$serial <-
    basename(fpa) |>
    stri_extract(regex = "AP\\d{6}") |>
    # Not all activPAL files will have the serial number in the filename. We don't
    # want an NA for future functions.
    stri_na2empty()
  return(lst_header)
}
process_meta_ap <- function(spo,
                            fnm,
                            fpa_epoch,
                            fpa_event) {

  lst_header_epoch <- rlang::try_fetch(
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
    chk_epoch_no_header <- inherits(lst_header_epoch, "waves_pal_no_header")
    chk_event_no_header <- inherits(lst_header_event, "waves_pal_no_header")
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
        "ID `{spo}`:",
        msg_epoch,
        msg_event
      ),
      class  = class(le_condition)[1],
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
        "{spo} epoch and event files not processed with same software version or options.",
        "i" = "Recommend processing both files with exact same software version and options.",
        "i" = "The following options differ:",
        vct_different
      ),
      call = rlang::caller_env()
    )
  }

  lst_header_epoch |>
    as_tibble() |>
    mutate(fnm = fnm,
           .before = 1)

}
