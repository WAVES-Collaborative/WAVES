check_input_ref <- function(lst_ref,
                            my_tz) {

  lst_ref <- lst_ref[sapply(
    lst_ref,
    \(x) {
      !all(simplify_is_null(x))
    }
  )]
  lst_chk <-
    vector(mode = "list",
           length = length(lst_ref)) |>
    setNames(names(lst_ref))

  for (i in seq_along(lst_ref)) {

    le_ref <- names(lst_ref)[i]

    if (le_ref %in% c("do", "img", "pal", "pass")) {
      lst_chk[[le_ref]] <- switch(
        le_ref,
        "do"  = {
          rlang::try_fetch(
            check_input_do(
              fpa_do = lst_ref$do,
              my_tz  = my_tz
            ),
            waves_chk_do_name = \(cnd) {cnd},
            waves_chk_do_site = \(cnd) {cnd},
            waves_chk_do_obs  = \(cnd) {cnd},
            waves_chk_do_dttm = \(cnd) {cnd},
            error = \(cnd) {
              cli::cli_abort(
                message = c(
                  "Unexpected Error.",
                  "i" = "Share with WAVES teams."
                ),
                parent  = cnd,
                call    = rlang::caller_env()
              )
            }
          )
        },
        "img" = {}, # TODO
        "pal" = {
          rlang::try_fetch(
            check_input_pal(
              vct_fpa_palp = lst_ref$pal$palp,
              vct_fpa_palv = lst_ref$pal$palv,
              spo_pat      = lst_ref$pal$spo_pat
            ),
            waves_chk_ap_miss  = \(cnd) {cnd},
            waves_chk_ap_id_pt = \(cnd) {cnd},
            error = \(cnd) {
              cli::cli_abort(
                message = c(
                  "Unexpected Error.",
                  "i" = "Share with WAVES teams."
                ),
                parent  = cnd,
                call    = rlang::caller_env()
              )
            }
          )
        },
        "pass" = {} # TODO
      )
    } else {
      # TODO: Flush out more if people want to add own reference processing
      # functions
      # lst_chk[[le_ref]] <- lst_ref[[le_ref]]$process_meta_function(
      #   lst_param <- lst_ref[[le_ref]][
      #     !names(lst_ref[[le_ref]]) %in% c("process_meta_function",
      #                                      "process_file_function")
      #   ]
      # )
    }
  }

  vct_error <- sapply(lst_chk, inherits, what = "error")

  if (any(vct_error)) {
    lst_error <- lst_chk[vct_error]
    vct_pretty_nm <- recode_values(
      names(lst_error),
      "do"   ~ "Direct Observation",
      "img"  ~ "Still-Images",
      "pal"  ~ "activPAL",
      "pass" ~ "ActiPass"
    )
    lst_message <- vector(mode = "list",
                          length = length(lst_error))

    for (i in seq_along(lst_error)) {
      lst_message[[i]] <- c(
        ">" = paste0("Reference ", vct_pretty_nm[i], ":"),
        "x" = lst_error[[i]]$message,
        lst_error[[i]]$body
      )
    }
    cli::cli_abort(
      message = c(
        "Input from the following reference's had user errors. See below.",
        unlist(lst_message),
        "i" = "NOTE: Disregard below text about debugging."
      ),
      call    = rlang::caller_env()
    )
    lapply(lst_chk[vct_error], conditionMessage)

  }

  # If activPAL data is provided alongside direct observation data, check if
  # "(?<obs>)" was present in site_pid_observation pattern.
  # If not, we can't do much checking until the merging step. We are assuming
  # all direct observation sessions occur within the activPAL file, and can only
  # check that once merging the data.
  if ("observation" %in% names(lst_chk$pal)) {
    chk_obs <- !any(
      unique(lst_chk$pal$observation) %in% unique(lst_chk$do$observation)
    )
    chk_spo <- any(
      lst_chk$do |>
        count(site, pid, observation) |>
        unite(col = spo,
              site, pid, observation,
              sep = "_") |>
        pull(spo) !=
        paste(
          lst_chk$do$site[1],
          lst_chk$pal$pid,
          lst_chk$pal$observation,
          sep = "_"
        )
    )

    # TODO: Figure if chk_obs or chk_spo is better. chk_spo is better
    # for giving more informative error, but still need to test with UWM
    # data.
    if (chk_obs) {
      cli::cli_abort(
        message = c(
          "Observations found from activPAL filenames do not match.",
          "WAVES team still working on providing which files do not match DO data.",
          "i" = "Please double-check both direct observation and activPAL data to ensure unique site, pid, and observation combinations match.",
          "i" = "NOTE: Disregard below text about debugging."
        ),
        call = rlang::caller_env()
      )
    }

  }

  return(lst_chk)

}
check_input_do <- function(fpa_do,
                           my_tz) {

  df <- fread(fpa_do)
  names(df) <-
    names(df) |>
    tolower()

  # Prior guiding documents had variable named as "date_time".
  if ("date_time" %in% names(df)) names(df)[names(df) == "date_time"] <- "datetime"

  vct_nm <- c(
    "site", "pid", "observation", "datetime", "date", "time", "domain_do",
    "posture_do", "intensity3_do", "intensity4_do", "steps_do", "sedtype_do"
  )
  chk_names <- !vct_nm %in% names(df)

  if (any(chk_names)) {
    vct_miss <- vct_nm[chk_names]
    cli::cli_abort(
      message = c(
        "User error with supplied Direct Observation data.",
        "x" = "The following variables are missing or named incorrectly:",
        indent_cli_message(vct_miss),
        "i" = "Please provide the variable or rename it.",
        "i" = "If it is a variable whos name ends with '_do' and that variable was not collected at all in your study, still provide as an empty column."
      ),
      class = "waves_chk_do_name",
      call = rlang::caller_env()
    )
  }

  df <-
    df |>
    mutate(
      date =
        date |>
        parse_date_time(orders = c("ymd", "mdy", "dmy"),
                        tz     = my_tz) |>
        as_date(),
      time =
        time |>
        parse_date_time(orders = c("HMS", "IMSp"),
                        tz = my_tz) |>
        format("%H:%M:%S")
    )

  # Site, Pid, Observation
  df_spo <- count(df, site, pid, observation)
  chk_site <- uniqueN(df_spo$site) > 1

  if (chk_site) {
    cli::cli_abort(
      message = c(
        "User error with supplied Direct Observation data.",
        "x" = "More than one value for {.var site} variable provided:",
        "i" = "Values provided:",
        indent_cli_message(unique(df_spo$site)),
        "i" = "There should only be one unique value for {.var site} variable."
      ),
      class = "waves_chk_do_site",
      call = rlang::caller_env()
    )
  }

  # From experience, observation sessions per participants are less than 5. Can
  # change this number if a site has more than 5
  chk_obs <- uniqueN(df_spo$observation) >= 5

  if (chk_obs) {
    vct_obs <- unique(df_spo$observation)

    if (length(vct_obs) > 10) {
      # Pretty printing, don't overload console.
      vct_obs <- c(
        vct_obs[1:10],
        paste0("...plus ", length(vct_obs) - 10, " unique values.")
      )
    }

    cli::cli_abort(
      message = c(
        "User error with supplied Direct Observation data.",
        "x" = "Five or more observation session values found.",
        "i" = "Unique observation sessions found:",
        indent_cli_message(vct_obs),
        "i" = "There are typically less than 5 sessions per participant in direct observation studies.",
        "i" = "Please double-check your DO file."
      ),
      class = "waves_chk_do_obs",
      call = rlang::caller_env()
    )
  }

  # Check if datetime is in ISO 8601 format of YYYY-mm-ddTHH:MM:SSZ.
  if (!is.POSIXct(df$datetime[1])) {
    df$datetime <-
      paste0(df$date, "_", df$time) |>
      ymd_hms(tz = my_tz) |>
      with_tz(tzone = "UTC")
  } else if (!my_tz %in% c("UTC", "GMT", "Etc/GMT+0")) {
    # Check if date + time together is = to datetime. It shouldn't be since
    # time is suppose to be in local time unless lst_yaml$my_tz is UTC as well.
    chk_dttm <-
      df |>
      summarise(
        chk = as.character(datetime[1]) == ymd_hms(paste(date[1], time[1]), tz = my_tz),
        .by = c(site, pid, observation)
      ) |>
      pull(chk) |>
      any()

    if (chk_dttm) {
      cli::cli_abort(
        message = c(
          "User error with supplied Direct Observation data.",
          "x" = "{.var datetime} values have the same value as {.var date} and {.var time} variables combined.",
          "i" = "{.var datetime} should only be in UTC time, while date and time variables are in local time.",
          "i" = "Please double-check your DO file."
        ),
        class = "waves_chk_do_dttm",
        call = rlang::caller_env()
      )
    }

  }

  df

}
check_input_pal <- function(vct_fpa_palp,
                            vct_fpa_palv,
                            spo_pat) {
  # Extract site, pid, observation and match files
  mtx_palp <-
    basename(vct_fpa_palp) |>
    stri_match(regex = spo_pat[1])
  colnames(mtx_palp)[1] <- "spo"
  df_palp <- as_tibble(mtx_palp)

  mtx_palv <-
    basename(vct_fpa_palv) |>
    stri_match(regex = spo_pat[1])
  colnames(mtx_palv)[1] <- "spo"
  df_palv <- as_tibble(mtx_palv)

  if ("obs" %in% names(df_palp)) {
    names(df_palp)[names(df_palp) == "obs"] <- "observation"
    names(df_palv)[names(df_palv) == "obs"] <- "observation"
  }

  # If the site_pid_observation from a filename is not extracted properly, then
  # the entire row will be empty. Just use the first column to check then.
  if (anyNA(c(df_palp$spo, df_palv$spo))) {

    vct_err_epoch <- basename(vct_fpa_palp)[is.na(df_palp$spo)]
    vct_err_event <- basename(vct_fpa_palv)[is.na(df_palv$spo)]
    spo_pat_pretty <-
      sub(
        x = spo_pat,
        pattern = "\\^",
        replacement = ""
      ) |>
      gsub(
        x = _,
        pattern = "\\\\d",
        replacement = "0"
      )
    cli::cli_abort(
      message = c(
        "User error with supplied activPAL data.",
        "The following activPAl files do not start with supplied {.envvar id_pt} '{id_pt_pretty}':",
        "i" = "1 second epoch files:",
        indent_cli_message(vct_err_epoch),
        "i" = "Event files:",
        indent_cli_message(vct_err_event)
      ),
      class = "waves_chk_ap_id_pt",
      call = rlang::caller_env()
    )

  }

  vct_nomatch_epoch <- setdiff(df_palv$spo, df_palp$spo)
  vct_nomatch_event <- setdiff(df_palp$spo, df_palv$spo)

  if (length(vct_nomatch_epoch) != 0 || length(vct_nomatch_event) != 0) {
    cli::cli_abort(
      message = c(
        "User error with supplied activPAL data.",
        "Not all activPAL files have a matching event OR matching 1sec file.",
        "i" = "Missing epoch files:",
        indent_cli_message(vct_nomatch_epoch),
        "i" = "Missing event files:",
        indent_cli_message(vct_nomatch_event),
        "i" = "Please either create missing files or remove extra files."
      ),
      class = "waves_chk_ap_miss",
      call = rlang::caller_env()
    )
  } else {
    df_palp |>
      unite(col = fnm,
            any_of(c("site", "pid", "observation")),
            sep = "_",
            remove = FALSE) |>
      mutate(
        pid       = as.integer(pid),
        fpa_epoch = vct_fpa_palp,
        fpa_event = vct_fpa_palv
      )
  }
}
#' Determine format of raw files
#'
#' @param vct_raw
#'
#' @returns
#' @export
#'
#' @examples
check_raw_type <- function(vct_raw) {

  vct_type <- vector(
    mode = "character",
    length = length(vct_raw)
  )

  for (i in seq_along(vct_raw)) {

    # Determine if input is "happily read by GGIR or if user is providing a
    # GENEActiv csv file w/ header or a no header csv.
    # https://github.com/wadpac/GGIR/issues/518
    I <- suppressWarnings(tryCatch(
      GGIR::g.inspectfile(
        datafile = vct_raw[i],
        params_rawdata =
          GGIR::extract_params(params2check = "rawdata")[["params_rawdata"]]
      ),
      error = \(e) e
    ))

    if (class(I)[1] == "simpleError") {

      chk_geneactiv_csv <-
        I$message == "The GENEActiv csv reading functionality is deprecated in GGIR from version 2.6-4 onwards. Please, use either the GENEActiv bin files or try to read the csv files with GGIR::read.myacc.csv"

      if (chk_geneactiv_csv) {
        vct_type[i] <- "GENEACTIV - CSV w/ HEADER"
      } else {
        vct_type[i] <- "UNKNOWN"
      }

    } else {

      # If GGIR determines its an ad-hoc csv, assume that it will be
      # GENEActiv csv file without header
      header_value <- tryCatch(I$header[1, 1], error = \(e) NA_character_)
      sf_value <- tryCatch(I$sf, error = \(e) NA_real_)
      chk_adhoc_csv <-
        identical(header_value, "file does not have header") &&
        identical(sf_value, 0)

      if (chk_adhoc_csv) {
        vct_type[i] <- "ADHOC"
      } else {
        monn <- tryCatch(I$monn, error = \(e) NA_character_)
        dformn <- tryCatch(I$dformn, error = \(e) NA_character_)

        if (anyNA(c(monn, dformn))) {
          vct_type[i] <- "UNKNOWN"
        } else {
          vct_type[i] <-
            paste0(monn, " - ", dformn) |>
            toupper()
        }
      }
    }
  }

  vct_type <-
    # Rename "GENEACTIVE" to "GENEACTIV".
    sub(
      x = vct_type,
      pattern = "GENEACTIVE",
      replacement = "GENEACTIV"
    ) |>
    # name with filename sans extension.
    setNames(
      vct_raw |>
        basename() |>
        file_path_sans_ext()
    )

  return(vct_type)

}
check_input_raw <- function(vct_raw,
                            spo_pat) {
  # Extract site, pid, observation and match files
  mtx_raw <-
    basename(vct_raw) |>
    stri_match(regex = spo_pat[1])
  colnames(mtx_raw)[1] <- "spo"
  df_raw <- as_tibble(mtx_raw)

  if ("obs" %in% names(df_raw)) names(df_raw)[names(df_raw) == "obs"] <- "observation"

  # If the site_pid_observation from a filename is not extracted properly, then
  # the entire row will be empty. Just use the first column to check then.
  if (anyNA(df_raw$spo)) {

    vct_err <- basename(vct_raw)[is.na(df_raw$spo)]
    spo_pat_pretty <-
      sub(
        x = spo_pat,
        pattern = "\\^",
        replacement = ""
      ) |>
      gsub(
        x = _,
        pattern = "\\\\d",
        replacement = "0"
      )
    cli::cli_abort(
      message = c(
        "User error with supplied wrist accelerometer data.",
        "i" = "The following files do not start with supplied {.envvar id_pt} '{id_pt_pretty}':",
        indent_cli_message(vct_err)
      ),
      class = "waves_chk_raw_id_pt",
      call = rlang::caller_env()
    )

  }

  df_raw |>
    unite(col = fnm,
          any_of(c("site", "pid", "observation")),
          sep = "_",
          remove = FALSE) |>
    mutate(
      pid     = as.integer(pid),
      fpa_raw = vct_raw,
      type    = check_raw_type(fpa_raw)
    )

}
check_input_pid <- function(df_meta,
                            df_chk_raw,
                            lst_chk,
                            trial_run) {

  lst_pid <- list(
    meta =
      df_meta |>
      as_tibble() |>
      mutate(
        site_deid,
        pid_deid,
        site,
        pid,
        meta = pid,
        .keep = "none"
      ),
    raw =
      df_chk_raw |>
      mutate(raw = pid) |>
      select(
        any_of("site"),
        pid,
        any_of("observation"),
        raw,
        fpa_raw,
        fnm_write_raw = fnm,
        type_raw = type
      )
  )
  vct_ref <- names(lst_chk)

  for (i in seq_along(lst_chk)) {

    le_ref <- vct_ref[i]

    if (le_ref %in% c("do", "img", "pal", "pass")) {
      lst_pid[[le_ref]] <- switch(
        le_ref,
        "do"  = {
          lst_chk$do |>
            summarise(
              do = pid[1],
              .by = c(site, pid, observation)
            )
        },
        "img" = {}, # TODO
        "pal" = {
          lst_chk$pal |>
            mutate(pal = pid) |>
            select(
              any_of("site"),
              pid,
              any_of("observation"),
              pal,
              fpa_epoch,
              fpa_event
            )
        },
        "pass" = {} # TODO
      )
    } else {
      # TODO: Flush out more if people want to add own reference processing
      # functions
      # lst_chk[[le_ref]] <- lst_ref[[le_ref]]$process_meta_function(
      #   lst_param <- lst_ref[[le_ref]][
      #     !names(lst_ref[[le_ref]]) %in% c("process_meta_function",
      #                                      "process_file_function")
      #   ]
      # )
    }
  }

  # lst_pid
  df_pid <-
    Reduce(
      \(x, y) full_join(x, y) |> suppressMessages(),
      # \(x, y) full_join(x, y, by = c("site", "pid")),
      lst_pid
    ) |>
    arrange(pid) |>
    select(
      site, pid,
      site_deid, pid_deid,
      any_of("observation"),
      meta, raw, all_of(vct_ref),
      type_raw, fnm_write_raw, fpa_raw,
      starts_with("fpa")
    )

  if (trial_run) {
    # Only look at raw accelerometer file going through trial run.
    df_pid <-
      df_pid |>
      dplyr::filter(!is.na(raw))
  }

  df_na <-
    df_pid |>
    mutate(across(
      .cols = c(meta, raw, all_of(vct_ref)),
      .fn = is.na
    ))

  # There should be metadata for every participant that has either raw and reference data.
  chk_meta <- any(df_na$meta)

  # There should be accelerometer data for every participant that has either raw and reference data.
  chk_raw <- any(df_na$raw)

  # For each participant, check there is data from at least one reference.
  vct_has_ref <-
    df_na |>
    select(all_of(vct_ref)) |>
    mutate(across(
      .cols = everything(),
      .fns = \(x) !x
    )) |>
    Reduce(x = _,
           f = `|`)
  chk_ref <- !all(vct_has_ref)

  if (chk_meta | chk_raw | chk_ref) {
    vct_filter <-
      df_na$meta | df_na$raw | !vct_has_ref
    df_pid <-
      df_pid |>
      dplyr::filter(!vct_filter)
    df_warn <-
      df_na |>
      dplyr::filter(vct_filter) |>
      select(
        any_of("site"), pid, any_of("observation"),
        meta, raw, all_of(vct_ref)
      ) |>
      mutate(across(
        .cols = meta:last_col(),
        .fns = ~ifelse(
          .x,
          yes = cli::symbol$cross,
          no  = cli::symbol$tick
        )
      ))
    vct_ref_pretty <-
      recode_values(
        vct_ref,
        "do"   ~ "Direct Observation",
        "img"  ~ "still-image",
        "pal"  ~ "activPAL",
        "pass" ~ "ActiPass"
      )
    vct_ref_message <-
      paste0(vct_ref, " = ", vct_ref_pretty) |>
      indent_cli_message()
    rlang::warn(
      message = c(
        "Input data was not found for certain participants.",
        "i" = paste0("The below table lists the following participants with missing input data (", cli::symbol$cross, " = missing data):"),
        df_warn |>
          kable() |>
          capture.output() |>
          vctrs::vec_slice(-1) |>
          indent_cli_message(),
        " " = "",
        " " = "where:",
        " " = "meta = metadata",
        " " = "raw  = wrist accelerometer data",
        vct_ref_message,
        " " = "",
        "i" = "These entries will not be run through the pipeline."
        # "i" = "Please ensure there is metadata, wrist accelerometer data and data from at least one reference for all participant's supplied or remove participants that:",
        # " " = "1) Are missing all metadata.",
        # " " = "2) Are missing a wrist accelerometer file.",
        # " " = "3) Have no reference data of any type."
      ),
      call = rlang::caller_env(),
      use_cli_format = FALSE
    )
  }

  df_pid

}
