check_input_ref <- function(lst_ref) {

  lst_ref <- lst_ref[lengths(lst_ref) != 0]
  lst_chk <-
    vector(mode = "list",
           length = length(lst_ref)) |>
    setNames(names(lst_ref))

  for (i in seq_along(lst_ref)) {

    le_ref <- names(lst_ref)[i]

    if (le_ref %in% c("do", "img", "pal", "pass")) {
      lst_chk[[le_ref]] <- switch(
        le_ref,
        "do"  = check_input_do(fpa_do   = lst_ref$do), # TODO
        "img" = {}, # TODO
        "pal" = {
          tryCatch(
            check_input_pal(
              vct_fpa_palp = lst_ref$pal$palp,
              vct_fpa_palv = lst_ref$pal$palv,
              id_pt        = lst_ref$pal$id_pt
            ),
            waves_chk_ap_miss = \(cnd) {
              cli::cli_abort(
                message = c(
                  "User Error.",
                  "i" = "See below for further details."
                ),
                parent  = cnd,
                call    = rlang::caller_env()
              )
            },
            waves_chk_ap_id_pt = \(cnd) {
              cli::cli_abort(
                message = c(
                  "User Error.",
                  "i" = "See below for further details."
                ),
                parent  = cnd,
                call    = rlang::caller_env()
              )
            },
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

  return(lst_chk)

}
check_input_do <- function(fpa_do) {
  # TODO: Check if datetime is in ISO 8601 format of YYYY-mm-ddTHH:MM:SSZ
  # TODO: Check if date + time together is = to datetime. It shouldn't be since
  #       time is suppose to be in local time unless lst_yaml$my_tz is UTC as well.
  # fpa_write <- file.path(dir_meta, "do.parquet")
  return(fpa_do)
}
check_input_pal <- function(vct_fpa_palp,
                            vct_fpa_palv,
                            id_pt) {
  # Extract ids and match files
  vct_nm_epoch <- stri_extract(
    basename(vct_fpa_palp),
    regex = paste0(id_pt, collapse = "|")
  )
  vct_nm_event <- stri_extract(
    basename(vct_fpa_palv),
    regex = paste0(id_pt, collapse = "|")
  )

  if (anyNA(c(vct_nm_epoch, vct_nm_event))){

    vct_err_epoch <- basename(vct_fpa_palp)[is.na(vct_nm_epoch)]
    vct_err_event <- basename(vct_fpa_palv)[is.na(vct_nm_event)]
    id_pt_pretty <-
      sub(
        x = id_pt,
        pattern = "\\^|\\\\d",
        replacement = ""
      ) |>
      gsub(
        x = _,
        pattern = "\\\\d",
        replacement = "0"
      )
    cli::cli_abort(
      message = c(
        "The following activPAl files do not start with supplied {.envvar id_pt} '{id_pt_pretty}':",
        "i" = "1 second epoch files:",
        vct_err_epoch,
        "i" = "Event files:",
        vct_err_event
      ),
      class = "waves_chk_ap_id_pt",
      call = rlang::caller_env()
    )

  }

  names(vct_fpa_palp) <- vct_nm_epoch
  names(vct_fpa_palv) <- vct_nm_event

  vct_nomatch_epoch <- setdiff(names(vct_fpa_palv), names(vct_fpa_palp))
  vct_nomatch_event <- setdiff(names(vct_fpa_palp), names(vct_fpa_palv))

  if (length(vct_nomatch_epoch) != 0 || length(vct_nomatch_event) != 0) {
    cli::cli_abort(
      message = c(
        "Not all activPAL files have a matching event OR matching 1sec file.",
        "i" = "Missing epoch files:",
        vct_nomatch_epoch,
        "i" = "Missing event files:",
        vct_nomatch_event,
        "i" = "Please either create missing files or remove extra files.",
        "i" = "NOTE: Disregard below text about debugging."
      ),
      class = "waves_chk_ap_miss",
      call = rlang::caller_env()
    )
  } else {
    tibble(
      id        = names(vct_fpa_palp),
      fpa_epoch = vct_fpa_palp,
      fpa_event = vct_fpa_palv
    )
  }
}
