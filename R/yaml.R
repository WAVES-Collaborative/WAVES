get_id_regex <- function(id_pt) {
  stri_replace_all(
    id_pt,
    regex = "0",
    replacement = "\\\\d"
  ) |>
    paste0("^", ... = _)
}
simplify_is_null <- function(x) {
  sapply(
    x,
    FUN = \(.x) length(.x) == 0
  )
}
simplify_post_expr <- function(x) {
  if (is.list(x)) {
    chk_type <-
      sapply(x, typeof) |>
      vctrs::vec_duplicate_detect() |>
      all() |
      length(sapply(x, typeof)) == 1
    if (chk_type) unlist(x) else x
  } else {
    x
  }
}
format_abort_message <- function(x,
                                 msg_info) {
  if (length(x) != 0) {
    x <- c(
      x,
      msg_info,
      ""
    )
  }
}
parse_waves_yaml <- function(fpa_yaml = "_waves.yml") {

  # READ -----------------------------------------------------------------------
  lst_handlers <- list(
    "!expr"  = function(x) eval(str2lang(x), baseenv()),
    "!upper" = toupper
  )
  lst_yaml <- read_yaml(
    fpa_yaml,
    handlers = lst_handlers
  )

  # Make parameters into coding friendly names.
  names(lst_yaml) <- c(
    "my_tz",
    "sf",
    "meta_fpa",
    "de_id",
    "site",
    "raw",
    "ref"
  )
  names(lst_yaml$raw) <- c(
    "fdr",
    "fpa"
  )
  names(lst_yaml$ref) <- c(
    "do",
    "img",
    "pal",
    "pass"
  )
  names(lst_yaml$ref$do) <- c(
    "fpa"
  )
  names(lst_yaml$ref$img) <- c(
    "fpa"
  )
  names(lst_yaml$ref$pal) <- c(
    "palp_fdr",
    "palp_fpa",
    "palv_fdr",
    "palv_fpa",
    "id_pt"
  )
  names(lst_yaml$ref$pass) <- c(
    "fdr",
    "fpa",
    "id_pt"
  )

  # Even though "simplify = TRUE" is default in read_yaml, NULL parameters are
  # kept in list format. Truly simplify NULLs then.
  # raw
  if (all(simplify_is_null(lst_yaml$raw$fdr))) {
    lst_yaml$raw["fdr"] <- list(NULL)
  }
  if (all(simplify_is_null(lst_yaml$raw$fpa))) {
    lst_yaml$raw["fpa"] <- list(NULL)
  }
  # do
  if (all(simplify_is_null(lst_yaml$ref$do$fpa))) {
    lst_yaml$ref$do["fpa"] <- list(NULL)
  }
  # img
  if (all(simplify_is_null(lst_yaml$ref$img$fpa))) {
    lst_yaml$ref$img["fpa"] <- list(NULL)
  }
  # pal
  if (all(simplify_is_null(lst_yaml$ref$pal$palp_fdr))) {
    lst_yaml$ref$pal["palp_fdr"] <- list(NULL)
  }
  if (all(simplify_is_null(lst_yaml$ref$pal$palp_fpa))) {
    lst_yaml$ref$pal["palp_fpa"] <- list(NULL)
  }
  if (all(simplify_is_null(lst_yaml$ref$pal$palv_fdr))) {
    lst_yaml$ref$pal["palv_fdr"] <- list(NULL)
  }
  if (all(simplify_is_null(lst_yaml$ref$pal$palv_fpa))) {
    lst_yaml$ref$pal["palv_fpa"] <- list(NULL)
  }
  if (all(simplify_is_null(lst_yaml$ref$pal$id_pt))) {
    lst_yaml$ref$pal["id_pt"] <- list(NULL)
  }
  # pass
  if (all(simplify_is_null(lst_yaml$ref$pass$fdr))) {
    lst_yaml$ref$pass["fdr"] <- list(NULL)
  }
  if (all(simplify_is_null(lst_yaml$ref$pass$fpa))) {
    lst_yaml$ref$pass["fpa"] <- list(NULL)
  }
  if (all(simplify_is_null(lst_yaml$ref$pass$id_pt))) {
    lst_yaml$ref$pass["id_pt"] <- list(NULL)
  }

  # If !expr tag is used for directory/filepath, then handlers will still return
  # a list for list items that are all character.
  # raw
  if (length(lst_yaml$raw$fdr) != 0) {
    lst_yaml$raw$fdr <- simplify_post_expr(lst_yaml$raw$fdr)
  }
  if (length(lst_yaml$raw$fpa) != 0) {
    lst_yaml$raw$fpa <- simplify_post_expr(lst_yaml$raw$fpa)
  }
  # pal
  if (length(lst_yaml$ref$pal$palp_fdr) != 0) {
    lst_yaml$ref$pal$palp_fdr <- simplify_post_expr(lst_yaml$ref$pal$palp_fdr)
  }
  if (length(lst_yaml$ref$pal$palp_fpa) != 0) {
    lst_yaml$ref$pal$palp_fpa <- simplify_post_expr(lst_yaml$ref$pal$palp_fpa)
  }
  if (length(lst_yaml$ref$pal$palv_fdr) != 0) {
    lst_yaml$ref$pal$palv_fdr <- simplify_post_expr(lst_yaml$ref$pal$palv_fdr)
  }
  if (length(lst_yaml$ref$pal$palv_fpa) != 0) {
    lst_yaml$ref$pal$palv_fpa
    lst_yaml$ref$pal$palv_fpa <- simplify_post_expr(lst_yaml$ref$pal$palv_fpa)
  }
  # pass
  if (length(lst_yaml$ref$pass$fdr) != 0) {
    lst_yaml$ref$pass$fdr <- simplify_post_expr(lst_yaml$ref$pass$fdr)
  }
  if (length(lst_yaml$ref$pass$fpa) != 0) {
    lst_yaml$ref$pass$fpa <- simplify_post_expr(lst_yaml$ref$pass$fpa)
  }

  if (identical(Sys.getenv("TAR_PROJECT"), "config")) {
    lst_yaml$my_tz <- "Etc/UTC"
    lst_yaml$sf <- 100
    lst_yaml$meta <- "DANGEROUS/MANTIPORE.csv"
    lst_yaml$raw$fpa <- file.path(
      "data", "0_CONFIG", "RAW",
      c(
        "WAVES_10002_RAW.csv.gz",
        "WAVES_10003_RAW.csv.gz",
        "WAVES_10004_RAW.gt3x",
        "WAVES_10005_RAW.cwa",
        "WAVES_10006_RAW.csv.gz"
      )
    )
    # TODO: incorporate when testing analysis pipeline, specifically with
    # WAVES10006 from UWM.
    # lst_yaml$ref$do <- list(
    #   fdr = file.path("not", "a", "real", "path"),
    #   id_pat = "WAVES00000",
    #   map_fpa = ""
    # )
    return(lst_yaml)
  }

  # CHECK ----------------------------------------------------------------------
  lst_msg <- list()

  ## my_tz ----------------------------------------
  # Strictly accept `study_timezone`. Use system timezone if empty.
  chk_tz <- !lst_yaml$my_tz %in% grep(
    x = OlsonNames(),
    pattern = "/",
    value = TRUE
  )
  if (length(lst_yaml$my_tz) == 0) {
    lst_yaml$my_tz <- .sys.timezone
  } else if (stri_isempty(lst_yaml$my_tz)) {
    lst_msg[["my_tz"]] <-
      "`study_timezone` is a empty string."
  } else if (chk_tz) {
    lst_msg[["my_tz"]] <-
      "`study_timezone` is not in an accepted format."
  }
  lst_msg[["my_tz"]] <- format_abort_message(
    lst_msg[["my_tz"]],
    msg_info =
      "Please define as a string in format of '[CONTINENTorCOUNTRY/CITY]' or 'Etc/GMT[OFFSET]'."
  )

  ## sf -------------------------------------------
  # Make sure sampling frequency is betweem 10 and 100(???)
  if (length(lst_yaml$sf) == 0) {
    lst_msg[["sf"]] <-
      "`sampling_frequency` is not defined."
  } else if (length(lst_yaml$sf) > 1) {
    lst_msg[["sf"]] <-
      "`sampling_frequency` has more than one entry."
  } else if (!is.numeric(lst_yaml$sf)) {
    lst_msg[["sf"]] <-
      "`sampling_frequency` is not a number."
  }
  lst_msg[["sf"]] <- format_abort_message(
    lst_msg[["sf"]],
    msg_info =
      "Please define as one number."
  )

  ## meta_fpa -------------------------------------------
  if (length(lst_yaml$meta_fpa) == 0) {
    lst_msg[["meta"]] <-
      "`metadata_filepath` is not defined."
  } else if (length(lst_yaml$meta_fpa) > 1) {
    lst_msg[["meta"]] <-
      "`metadata_filepath` has more than one entry."
  } else if (!fs::is_file(lst_yaml$meta_fpa)) {
    lst_msg[["meta"]] <-
      "`metadata_filepath` is not a real filepath."
  }
  lst_msg[["meta"]] <- format_abort_message(
    lst_msg[["meta"]],
    msg_info =
      "Please define as one filepath."
  )

  ## de_id  -------------------------------------------
  if (length(lst_yaml$de_id) == 0) {
    lst_msg[["de_id"]] <-
      "`deidentify` is not defined."
  } else if (!is.logical(lst_yaml$de_id)) {
    lst_msg[["de_id"]] <-
      "`deidentify` is not a TRUE or FALSE value."
  }
  lst_msg[["de_id"]] <- format_abort_message(
    lst_msg[["de_id"]],
    msg_info =
      "Please define as a boolean TRUE or FALSE."
  )

  ## site  -------------------------------------------
  if (length(lst_yaml$site) == 0) {
    lst_msg[["site"]] <-
      "`site` is not defined."
  } else if (!is.character(lst_yaml$site)) {
    lst_msg[["site"]] <-
      "`deidentify` is not a string."
  }
  lst_msg[["site"]] <- format_abort_message(
    lst_msg[["site"]],
    msg_info =
      "Please define as the string. Will be given to you by WAVES team."
  )

  ## raw --------------------------------------
  if (length(lst_yaml$raw$fdr) == 0 &&
      length(lst_yaml$raw$fpa) == 0) {
    lst_msg[["vct_raw"]] <- c(
      "wrist_accelerometer `directories` or `filepaths` must be defined.",
      "Please define either wrist_accelerometer `directories` or `filepaths`, not both.",
      ""
    )
  } else if (length(lst_yaml$raw$fdr) != 0 &&
             length(lst_yaml$raw$fpa) != 0) {
    lst_msg[["vct_raw"]] <- c(
      "wrist_accelerometer `directories` and `filepaths` are both defined.",
      "Please define either wrist_accelerometer `directories` or `filepaths`, not both.",
      ""
    )
  } else if (length(lst_yaml$raw$fdr) != 0 &&
             !any(fs::is_dir(lst_yaml$raw$fdr))) {
    lst_msg[["vct_raw"]] <- c(
      "wrist_accelerometer `directories` contains a string that is NOT a file directory.",
      "Please define as one or more strings corresponding to directories.",
      ""
    )
  } else if (length(lst_yaml$raw$fpa) != 0 &&
             !any(fs::is_file(lst_yaml$raw$fpa))) {
    lst_msg[["vct_raw"]] <- c(
      "wrist_accelerometer `filepaths` contains a string that is NOT a filepath.",
      "Please define as one or more strings corresponding to filepaths.",
      ""
    )
  }

  # If raw$fdr is valid, return the files in the directories provided.
  if (length(lst_yaml$raw$fdr) != 0 &&
      all(fs::is_dir(lst_yaml$raw$fdr))) {
    lst_yaml$raw$fpa <- list.files(
      path       = lst_yaml$raw$fdr,
      pattern    = "\\.bin$|\\.csv$|\\.cwa$|\\.gt3x$",
      full.names = TRUE,
      recursive  = FALSE
    )
  }

  ## ref ------------------------------------------
  # Make sure values are present for at least one reference.
  chk_ref <- all(
    simplify_is_null(lst_yaml$ref$do),
    simplify_is_null(lst_yaml$ref$img),
    simplify_is_null(lst_yaml$ref$pal),
    simplify_is_null(lst_yaml$ref$pass)
  )

  if (chk_ref) {
    lst_msg[["ref"]] <- c(
      "Parameters for at least one reference must be defined.",
      "Please define ALL parameters for at least one reference.",
      ""
    )
  } else {
    ### do ----------------------------------------
    if (!all(simplify_is_null(lst_yaml$ref$do))) {
      if (length(lst_yaml$ref$do$fpa) == 0) {
        lst_msg[["ref_do_fpa"]] <-
          "Direct observation `filepath` is not defined."
      } else if (!any(fs::is_file(lst_yaml$ref$do$fpa))) {
        lst_msg[["ref_do_fpa"]] <-
          "Direct observation `filepath` contains a string that is NOT a file path."
      }
      lst_msg[["ref_do_fpa"]] <- format_abort_message(
        lst_msg[["ref_do_fpa"]],
        msg_info =
          "Please define as one or more strings corresponding to filepaths."
      )
    }

    ### img ----------------------------------------
    if (!all(simplify_is_null(lst_yaml$ref$img))) {
      if (length(lst_yaml$ref$img$fpa) == 0) {
        lst_msg[["ref_img_fpa"]] <-
          "Still-image `filepath` is not defined."
      } else if (!any(fs::is_file(lst_yaml$ref$img$fpa))) {
        lst_msg[["ref_img_fpa"]] <-
          "Still-image `filepath` contains a string that is NOT a file path."
      }
      lst_msg[["ref_img_fpa"]] <- format_abort_message(
        lst_msg[["ref_img_fpa"]],
        msg_info =
          "Please define as one or more strings corresponding to filepaths."
      )
    }

    ### pal ---------------------------------------
    if (!all(simplify_is_null(lst_yaml$ref$pal))) {
      #### vct_palp
      if (length(lst_yaml$ref$pal$palp_fdr) == 0 &&
          length(lst_yaml$ref$pal$palp_fpa) == 0) {
        lst_msg[["vct_palp"]] <- c(
          "`1secEpochs_directories` or `1secEpochs_filepaths` must be defined.",
          "Please define either `1secEpochs_directories` or `1secEpochs_filepaths`, not both.",
          ""
        )
      } else if (length(lst_yaml$ref$pal$palp_fdr) != 0 &&
                 length(lst_yaml$ref$pal$palp_fpa) != 0) {
        lst_msg[["vct_palp"]] <- c(
          "`1secEpochs_directories` and `1secEpochs_filepaths` are both defined.",
          "Please define either `1secEpochs_directories` or `1secEpochs_filepaths`, not both.",
          ""
        )
      } else if (length(lst_yaml$ref$pal$palp_fdr) != 0 &&
                 !any(fs::is_dir(lst_yaml$ref$pal$palp_fdr))) {
        lst_msg[["vct_palp"]] <- c(
          "`1secEpochs_directories` contains a string that is NOT a file directory.",
          "Please define as one or more strings corresponding to directories.",
          ""
        )
      } else if (length(lst_yaml$ref$pal$palp_fpa) != 0 &&
                 !any(fs::is_file(lst_yaml$ref$pal$palp_fpa))) {
        lst_msg[["vct_palp"]] <- c(
          "`1secEpochs_filepaths` contains a string that is NOT a filepath.",
          "Please define as one or more strings corresponding to filepaths.",
          ""
        )
      }

      # If palp_fdr is valid, return the files in the directories provided.
      if (length(lst_yaml$ref$pal$palp_fdr) != 0 &&
          all(fs::is_dir(lst_yaml$ref$pal$palp_fdr))) {
        lst_yaml$ref$pal$palp_fpa <- list.files(
          path       = lst_yaml$ref$pal$palp_fdr,
          pattern    = "Epochs1s\\.csv$",
          full.names = TRUE,
          recursive  = FALSE
        )
      }

      #### vct_palv
      if (length(lst_yaml$ref$pal$palv_fdr) == 0 &&
          length(lst_yaml$ref$pal$palv_fpa) == 0) {
        lst_msg[["vct_palv"]] <- c(
          "`events_directories` or `events_filepaths` must be defined.",
          "Please define either `events_directories` or `events_filepaths`, not both.",
          ""
        )
      } else if (length(lst_yaml$ref$pal$palv_fdr) != 0 &&
                 length(lst_yaml$ref$pal$palv_fpa) != 0) {
        lst_msg[["vct_palv"]] <- c(
          "`events_directories` and `events_filepaths` are both defined.",
          "Please define either `events_directories` or `events_filepaths`, not both.",
          ""
        )
      } else if (length(lst_yaml$ref$pal$palv_fdr) != 0 &&
                 !any(fs::is_dir(lst_yaml$ref$pal$palv_fdr))) {
        lst_msg[["vct_palv"]] <- c(
          "`events_directories` contains a string that is NOT a file directory.",
          "Please define as one or more strings corresponding to directories.",
          ""
        )
      } else if (length(lst_yaml$ref$pal$palv_fpa) != 0 &&
                 !any(fs::is_file(lst_yaml$ref$pal$palv_fpa))) {
        lst_msg[["vct_palv"]] <- c(
          "`events_filepaths` contains a string that is NOT a filepath.",
          "Please define as one or more strings corresponding to filepaths.",
          ""
        )
      }

      # If palv_fdr is valid, return the files in the directories provided.
      if (length(lst_yaml$ref$pal$palv_fdr) != 0 &&
          all(fs::is_dir(lst_yaml$ref$pal$palv_fdr))) {
        lst_yaml$ref$pal$palv_fpa <- list.files(
          path       = lst_yaml$ref$pal$palv_fdr,
          pattern    = "-EventsEx\\.csv$",
          full.names = TRUE,
          recursive  = FALSE
        )
      }

      #### id_pattern
      if (length(lst_yaml$ref$pal$id_pt) == 0) {
        lst_msg[["ref_pal_id_pat"]] <-
          "activPAL `id_pattern` is not defined."
      } else if (!any(stri_detect(lst_yaml$ref$pal$id_pt, regex = "0"))) {
        lst_msg[["ref_pal_id_pat"]] <-
          "activPAL `id_pattern` contains a string that does not have any number placeholders."
      }
      lst_msg[["ref_pal_id_pat"]] <- format_abort_message(
        lst_msg[["ref_pal_id_pat"]],
        msg_info =
          "Please define as one or more strings with '0' as a placeholder for numbers."
      )

      lst_yaml$ref$pal$id_pt <- get_id_regex(lst_yaml$ref$pal$id_pt)

    }

    ### pass ---------------------------------------
    if (!all(simplify_is_null(lst_yaml$ref$pass))) {
      if (length(lst_yaml$ref$pass$fdr) == 0 &&
          length(lst_yaml$ref$pass$fpa) == 0) {
        lst_msg[["vct_pass"]] <- c(
          "ActiPass `directories` or `filepaths` must be defined.",
          "Please define either ActiPass `directories` or `filepaths`, not both.",
          ""
        )
      } else if (length(lst_yaml$ref$pass$fdr) != 0 &&
                 length(lst_yaml$ref$pass$fpa) != 0) {
        lst_msg[["vct_pass"]] <- c(
          "ActiPass `directories` and `filepaths` are both defined.",
          "Please define either ActiPass `directories` or `filepaths`, not both.",
          ""
        )
      } else if (length(lst_yaml$ref$pass$fdr) != 0 &&
                 !any(fs::is_dir(lst_yaml$ref$pass$fdr))) {
        lst_msg[["vct_pass"]] <- c(
          "ActiPass `directories` contains a string that is NOT a file directory.",
          "Please define as one or more strings corresponding to directories.",
          ""
        )
      } else if (length(lst_yaml$ref$pass$fpa) != 0 &&
                 !any(fs::is_file(lst_yaml$ref$pass$fpa))) {
        lst_msg[["vct_pass"]] <- c(
          "ActiPass `filepaths` contains a string that is NOT a filepath.",
          "Please define as one or more strings corresponding to filepaths.",
          ""
        )
      }

      #### id_pattern
      if (length(lst_yaml$ref$pass$id_pt) == 0) {
        lst_msg[["ref_pass_id_pat"]] <-
          "ActiPass `id_pattern` is not defined."
      } else if (!any(stri_detect(lst_yaml$ref$pass$id_pt, regex = "0"))) {
        lst_msg[["ref_pass_id_pat"]] <-
          "ActiPass `id_pattern` contains a string that does not have any number placeholders."
      }
      lst_msg[["ref_pass_id_pat"]] <- format_abort_message(
        lst_msg[["ref_pass_id_pat"]],
        msg_info =
          "Please define as one or more strings with '0' as a placeholder for numbers."
      )

      lst_yaml$ref$pass$id_pt <- get_id_regex(lst_yaml$ref$pass$id_pt)

    }
  }

  # return error if any error message is present.
  if (length(lst_msg) > 0) {
    c(
      "One or more errors found in `_waves.yaml`",
      unlist(lst_msg) |>
        setNames(rep(c("x", "i", ""), times = length(lst_msg)))
    ) |>
      rlang::abort(message = _, class = "yaml_error")
  } else {return(lst_yaml)}
}
