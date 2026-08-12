process_reference_metadata <- function(lst_yaml,
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
        le_ref %in% c("do", "pal", "pass")) {
      lst_meta[[le_ref]] <- switch(
        le_ref,
        "do" = {}, # TODO
        "pal" = tryCatch(
          process_activpal_metadata(
            vct_epoch = lst_yaml$ref$pal$palp_fpa,
            vct_event = lst_yaml$ref$pal$palv_fpa,
            id_pt     = lst_yaml$ref$pal$id_pt,
            dir_meta  = dir_meta
          ),
          error = \(e) e
        ),
        "pass" = {} # TODO
      )
    } else if (le_chk) {
      # TODO: Flush out more if people want to add own reference processing
      # functions
      lst_meta[[le_ref]] <- lst_yaml$ref[[le_ref]]$process_meta_function(
        lst_param <- lst_yaml$ref[[le_ref]][
          !names(lst_yaml$ref[[le_ref]]) %in% c("process_meta_function",
                                                "process_file_function")
        ]
      )
    }

  }

  unlist(lst_meta)

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
process_activpal_metadata <- function(vct_epoch,
                                      vct_event,
                                      id_pt,
                                      dir_meta) {

  # IDs ------------------------------------------------------------------------
  ### Extract ids and match files
  names(vct_epoch) <- stri_extract(
    basename(vct_epoch),
    regex = paste0(id_pt, collapse = "|")
  )
  names(vct_event) <- stri_extract(
    basename(vct_event),
    regex = paste0(id_pt, collapse = "|")
  )

  vct_nomatch_epoch <- setdiff(names(vct_event), names(vct_epoch))
  vct_nomatch_event <- setdiff(names(vct_epoch), names(vct_event))

  if (length(vct_nomatch_epoch) != 0 || length(vct_nomatch_event) != 0) {

    if (length(vct_nomatch_epoch) != 0) {
      names(vct_nomatch_epoch) <- rep("i", times = length(vct_nomatch_epoch))
      cli::cli_warn(c(
        "x" = "The following ID's do not have an epoch file generated.",
        vct_nomatch_epoch,
        "i" = "Please use activPAL software to export a 1-second epoch file following WAVES instructions."
      ))
    }

    if (length(vct_nomatch_event) != 0) {
      names(vct_nomatch_event) <- rep("i", times = length(vct_nomatch_event))
      cli::cli_warn(c(
        "x" = "The following ID's do not have an df_event file generated.",
        vct_nomatch_event,
        "i" = "Please use activPAL software to export a df_event file following WAVES instructions."
      ))
    }

    cli::cli_abort("^^^^^^^^^^^^^")

  } else {

    vct_fpa_write <-
      character(length = length(vct_epoch)) |>
      setNames(names(vct_epoch))

    for (i in seq_along(vct_epoch)) {

      le_id <- names(vct_epoch)[i]

      message("Processing: ", le_id)

      # HEADER -----------------------------------------------------------------
      lst_header_epoch <- extract_pal_header(vct_epoch[le_id])
      lst_header_event <- extract_pal_header(vct_event[le_id])
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
        cli::cli_warn(c(
          "{le_id} epoch and event files not processed with same software tool.",
          "i" = "Recommend processing both files with exact same software version and options.",
          "i" = "The following options differ:",
          vct_different
        ))
        lst_header <- list(NULL)
      } else {
        lst_header <- lst_header_epoch
      }

      # WRITE ------------------------------------------------------------------
      fpa_write <- file.path(
        dir_meta,
        paste0(le_id, "_", "pal.qs")
      )
      qs2::qs_save(
        lst_header,
        file = fpa_write
      )

      if (file.exists(fpa_write)) vct_fpa_write[le_id] <- fpa_write

    }

    return(vct_fpa_write)

  }
}
