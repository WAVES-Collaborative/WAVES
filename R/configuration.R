get_tbl_module <- function(df_module_WAVES,
                           conda_env) {

  le_primary_module <-
    sub(x = conda_env,
        pattern = "WHO_WAVES_",
        replacement = "")
  le_primary_module <- case_match(
    le_primary_module,
    "oak_1.0" ~ "beiwe-forest",
    "oak_pre" ~ "forest-analysis",
    .default = le_primary_module
  )

  df_module_installed <-
    full_join(
      df_module_WAVES,
      reticulate:::conda_list_packages(conda_env) |>
        mutate(
          Module = package,
          `Version Installed` = version,
          .keep = "none"
        ),
      by = join_by(Module)
    ) |>
    select(Module, starts_with("Version"), Channel) |>
    mutate(
      Module =
        factor(Module) |>
        relevel(ref = le_primary_module)
    ) |>
    arrange(Module) |>
    mutate(
      clr = case_when(
        # The below packages don't matter for version/date.
        Module %in% c("ca-certificates", "certifi") ~ "#FFFFFF",
        # Module appears in WAVES install but not your install.
        is.na(`Version Installed`)                   ~ "#ea9999",
        # Module appear in your install but not WAVES install.
        is.na(`Version WAVES`)                       ~ "#ea9999",
        # Module version for "primary module is different version.
        Module == le_primary_module &
          (`Version WAVES` != `Version Installed`)   ~ "#ea9999",
        # Module version for other modules do not match.
        `Version WAVES` != `Version Installed`       ~ "#FAFA8E",
        .default                                     = "#D9F1D5"
      )
    )
  tbl_module <-
    df_module_installed |>
    select(-clr) |>
    gt() |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = 1)
    )

  for (i in seq_len(nrow(df_module_installed))) {
    tbl_module <-
      tbl_module |>
      tab_style(
        style = cell_fill(color = df_module_installed$clr[i]),
        locations = cells_body(columns = starts_with("Version"),
                               rows    = i)
      )
  }

  tbl_module

}
get_msg_module <- function(tbl_module) {
  vct_color <- sapply(
    tbl_module$`_styles`$styles,
    FUN = \(x) x$cell_fill$color
  )
  chk_module_installed <- any(
    vct_color == "#EA9999"
  )
  chk_module_versions <- any(
    vct_color == "#FAFA8E"
  )
  case_when(
    chk_module_installed ~ 'Modules installation unsuccessful. Please refer to "Posting an Issue on GitHub" section of README to share issue with WAVES team.',
    chk_module_versions  ~ "Modules installation successful, but versions do not completely match WAVES configuration. This should not affect the pipeline, but is noted for thoroughness.",
    .default             = "Modules installation successful."
  )
}
# Code for getting module WAVES version
# reticulate:::conda_list_packages("WHO_WAVES_stepcount") |>
#   mutate(
#     Module = package,
#     `Version WAVES` = version,
#     requirement,
#     Channel = channel,
#     .keep = "none"
#   ) |>
#   fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_stepcount.csv"))
# reticulate:::conda_list_packages("WHO_WAVES_accelerometer") |>
#   mutate(
#     Module = package,
#     `Version WAVES` = version,
#     requirement,
#     Channel = channel,
#     .keep = "none"
#   ) |>
#   fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_walmsley.csv"))
# reticulate:::conda_list_packages("WHO_WAVES_actinet") |>
#   mutate(
#     Module = package,
#     `Version WAVES` = version,
#     requirement,
#     Channel = channel,
#     .keep = "none"
#   ) |>
#   fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_actinet.csv"))
# reticulate:::conda_list_packages("WHO_WAVES_oak_1.0") |>
#   mutate(
#     Module = package,
#     `Version WAVES` = version,
#     requirement,
#     Channel = channel,
#     .keep = "none"
#   ) |>
#   fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_oak1.0.csv"))
# reticulate:::conda_list_packages("WHO_WAVES_oak_pre") |>
#   mutate(
#     Module = package,
#     `Version WAVES` = version,
#     requirement,
#     Channel = channel,
#     .keep = "none"
#   ) |>
#   fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_oakpre.csv"))
config_miniconda <- function(df_module_stepcount,
                             df_module_walmsley,
                             df_module_actinet,
                             df_module_oak_1.0,
                             df_module_oak_pre) {

  chk_windows <- grepl(
    x = Sys.getenv(c("OS", "R_PLATFORM")),
    pattern = "windows",
    ignore.case = TRUE
  ) |>
    any()
  suffix_python <- if (reticulate:::is_windows()) "python.exe" else "bin/python"

  # conda ----
  chk_conda <-
    class(tryCatch(conda_version(), error = \(e) e))[1] == "simpleError"

  if (chk_conda) {

    install_miniconda()
    chk_successful <-
      class(tryCatch(conda_version(), error = \(e) e))[1] != "simpleError"

    if (chk_successful) {msg_conda <- paste0(
      "Not previously installed. Installed at ", '"',
      miniconda_path(), '"'
    )} else {
      stop(
        'Unsuccessful conda installation. If the environment variable "RETICULATE_MINICONDA_PATH" was changed, please make sure the directory exists. If it was not changed, please refer to "Posting an Issue on GitHub" section of README to share issue with WAVES team.',
        call. = FALSE
      )
    }
  } else {msg_conda <- paste0(
    "Already installed. Found at ", '"', miniconda_path(), '"'
  )}

  # setup ----
  # The "condaenv_exists()" function looks to see if there is an environment
  # with the supplied name in all conda installations, even if you specify
  # a specific conda binary with argument `conda`. Therefore just list
  # environments manually. This normally wouldn't be a problem but as the
  # entire WAVES project is still in pre-release, this can get messy really
  # quick if a user re-runs the pipeline while changing where conda is being
  # installed or the WAVES team makes changes to this function.
  vct_env_all <- list.files(
    file.path(miniconda_path(), "envs")
  )
  vct_env <- c(
    "WHO_WAVES_stepcount",
    "WHO_WAVES_accelerometer",
    "WHO_WAVES_actinet",
    "WHO_WAVES_oak_1.0",
    "WHO_WAVES_oak_pre"
  )
  lst_module_creation <-
    list("openjdk",
         # For WAVES_accelerometer, install heavy deps via conda first (ARM64
         # binaries available across platforms). Without this, pip tries to
         # build pandas/numpy from source on macOS ARM64 and fails due to
         # setuptools 80+ dropping pkg_resources.
         c("openjdk", "numpy==1.21", "pandas==1.3", "scipy==1.7"),
         "openjdk",
         "timezonefinder==8.1.0",
         NULL) |>
    setNames(vct_env)
  vct_python_version <-
    c(3.9,
      3.9,
      3.9,
      3.12,
      3.11) |>
    setNames(vct_env)
  vct_module_primary <-
    c(
      "stepcount==3.17.0",
      "accelerometer==7.3.0",
      "actinet==0.4.2",
      "git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
      "git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317"
    ) |>
    setNames(vct_env)
  lst_df_module <-
    list(df_module_stepcount,
         df_module_walmsley,
         df_module_actinet,
         df_module_oak_1.0,
         df_module_oak_pre) |>
    setNames(vct_env)
  vct_msg_env <- vct_msg_module <-
    vector("character", length = 5) |>
    setNames(vct_env)
  lst_tbl_module <-
    vector("list", length = 5) |>
    setNames(vct_env)

  for (i in seq_along(vct_env)) {

    le_env <- vct_env[i]
    chk_env <- !le_env %in% vct_env_all

    if (chk_env) {

      ## env create ----
      conda_create(
        envname        = le_env,
        packages       = lst_module_creation[[le_env]],
        python_version = vct_python_version[le_env]
      )
      chk_successful <- dir.exists(
        file.path(miniconda_path(), "envs", le_env)
      )

      if (chk_successful) {

        vct_msg_env[le_env] <- "Successfuly created."
        reticulate:::pip_install(
          python   = file.path(miniconda_path(), "envs", le_env, suffix_python),
          packages = vct_module_primary[le_env],
          envname  = le_env
        )
        lst_tbl_module[[le_env]] <- get_tbl_module(
          df_module_WAVES = lst_df_module[[le_env]],
          conda_env       = le_env
        )
        vct_msg_module[le_env] <- get_msg_module(lst_tbl_module[[le_env]])

      } else {
        vct_msg_env[le_env] <-
          'Not created. Please refer to "Posting an Issue on GitHub" section of README to share issue with WAVES team.'
        vct_msg_module[le_env] <-
          "No modules installed."
      }

    } else {

      ## env exists ----
      vct_msg_env[le_env] <- "Already exists."

      # Check packages are installed.
      lst_tbl_module[[le_env]] <- get_tbl_module(
        lst_df_module[[le_env]],
        conda_env = le_env
      )
      vct_msg_module[le_env] <- get_msg_module(lst_tbl_module[[le_env]])
      chk_package <-
        vct_msg_module[le_env] == 'Modules installation unsuccessful. Please refer to "Posting an Issue on GitHub" section of README to share issue with WAVES team.'

      if (chk_package) {

        # Install modules and check one more time afterwards.
        reticulate:::pip_install(
          python   = file.path(miniconda_path(), "envs", le_env, suffix_python),
          packages = vct_module_primary[le_env],
          envname  = le_env
        )
        lst_tbl_module[[le_env]] <- get_tbl_module(
          df_module_WAVES = lst_df_module[[le_env]],
          conda_env       = le_env
        )
        vct_msg_module[le_env] <- get_msg_module(lst_tbl_module[[le_env]])

      } else {
        vct_msg_module[le_env] <- "Modules already installed. Versions for some modules may or may not match WAVES configuration, check below. Version differences should not affect pipeline, but is noted for thoroughness."
      }

    }
  }

  # return ----
  df_msg <- data.frame(
    Item = c(
      "Status",
      "Version",
      "Environment",
      "Modules",
      "Environment",
      "Modules",
      "Environment",
      "Modules",
      "Environment",
      "Modules",
      "Environment",
      "Modules"
    ),
    Message = c(
      msg_conda,
      conda_version(),
      vct_msg_env["WHO_WAVES_stepcount"],
      vct_msg_module["WHO_WAVES_stepcount"],
      vct_msg_env["WHO_WAVES_accelerometer"],
      vct_msg_module["WHO_WAVES_accelerometer"],
      vct_msg_env["WHO_WAVES_actinet"],
      vct_msg_module["WHO_WAVES_actinet"],
      vct_msg_env["WHO_WAVES_oak_1.0"],
      vct_msg_module["WHO_WAVES_oak_1.0"],
      vct_msg_env["WHO_WAVES_oak_pre"],
      vct_msg_module["WHO_WAVES_oak_pre"]
    )
  )
  rownames(df_msg) <- c(
    "miniconda_install",
    "miniconda_version",
    "step_env",
    "step_module",
    "wlms_env",
    "wlms_module",
    "acti_env",
    "acti_module",
    "oak_1.0_env",
    "oak_1.0_module",
    "oak_pre_env",
    "oak_pre_module"
  )
  return(list(
    df_msg   = df_msg,
    tbl_step = lst_tbl_module[["WHO_WAVES_stepcount"]],
    tbl_wlms = lst_tbl_module[["WHO_WAVES_accelerometer"]],
    tbl_acti = lst_tbl_module[["WHO_WAVES_actinet"]],
    tbl_oak1 = lst_tbl_module[["WHO_WAVES_oak_1.0"]],
    tbl_oakp = lst_tbl_module[["WHO_WAVES_oak_pre"]]
  ))

}
#' Determine format of raw files
#'
#' @param vct_raw
#'
#' @returns
#' @export
#'
#' @examples
config_raw_type <- function(vct_raw) {

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
