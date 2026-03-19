get_tbl_pkgs <- function(df_pkgs_WAVES,
                         conda_env) {

  le_primary_module <-
    sub(x = conda_env,
        pattern = "WHO_WAVES_",
        replacement = "")
  le_primary_module <- ifelse(
    grepl(x = le_primary_module, pattern = "oak"),
    yes = "forest",
    no  = le_primary_module
  )

  df_pkgs_installed <-
    left_join(
      df_pkgs_WAVES,
      py_list_packages(conda_env) |>
        mutate(
          Package = package,
          `Version Installed` = version,
          .keep = "none"
        ),
      by = join_by(Package)
    ) |>
    select(Package, starts_with("Version"), Channel = channel) |>
    mutate(
      clr = case_when(
        # The below packages don't matter for version/date.
        Package %in% c("ca-certificates", "certifi") ~ "#FFFFFF",
        # Package appears in WAVES install but not your install.
        is.na(`Version Installed`)                   ~ "#ea9999",
        # Package appear in your install but not WAVES install.
        is.na(`Version WAVES`)                       ~ "#ea9999",
        # Package version for "primary module is different version.
        Package == le_primary_module &
          (`Version WAVES` != `Version Installed`)   ~ "#ea9999",
        # Package version for other modules do not match.
        `Version WAVES` != `Version Installed`       ~ "#FAFA8E",
        .default                                     = "#D9F1D5"
      )
    )
  tbl_pkgs <-
    df_pkgs_installed |>
    select(-clr) |>
    gt()

  for (i in seq_len(nrow(df_pkgs_installed))) {
    tbl_pkgs <-
      tbl_pkgs |>
      tab_style(
        style = cell_fill(color = df_pkgs_installed$clr[i]),
        locations = cells_body(columns = starts_with("Version"),
                               rows    = i)
      )
  }

  tbl_pkgs

}
config_miniconda <- function(df_pkgs_stepcount,
                             df_pkgs_walmsley,
                             df_pkgs_actinet,
                             df_pkgs_oak_1.0,
                             df_pkgs_oak_pre) {

  chk_windows <- grepl(
    x = Sys.getenv(c("OS", "R_PLATFORM")),
    pattern = "windows",
    ignore.case = TRUE
  ) |>
    any()

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
        'Unsuccessful installation. If the environment variable "RETICULATE_MINICONDA_PATH" was changed, please make sure the directory exisits. If it was not changed, Share report with WHO_WAVES team.',
        call. = FALSE
      )
    }
  } else {msg_conda <- paste0(
    "Already installed. Found at ", '"', miniconda_path(), '"'
  )}

  # setup environment ----
  # The "condaenv_exists()" function looks to see if there is an environment
  # with the supplied name in all conda installations, even if you specify
  # a specific conda binary with argument `conda`. Therefore just list
  # environments manually. This normally wouldn't be a problem but as the
  # entire WAVES project is still in pre-release, this can get messy really
  # quick if a user re-runs the pipeline while changing where conda is being
  # installed or the WAVES team makes changes to this function.
  vct_env <- list.files(
    file.path(miniconda_path(), "envs")
  )
  chk_stepcount <- !"WHO_WAVES_stepcount" %in% vct_env
  chk_accelerometer <- !"WHO_WAVES_accelerometer" %in% vct_env
  chk_actinet <- !"WHO_WAVES_actinet" %in% vct_env
  chk_oak_1.0 <- !"WHO_WAVES_oak_1.0" %in% vct_env
  chk_oak_pre <- !"WHO_WAVES_oak_pre" %in% vct_env

  # stepcount ----
  if (chk_stepcount) {

    conda_create(
      envname = "WHO_WAVES_stepcount",
      packages = "openjdk",
      forge = FALSE,
      python_version = 3.9,
      pip = TRUE
    )
    chk_successful <-
      list.files(
        file.path(miniconda_path(), "envs")
      ) |> grepl(x = _,
                 pattern = "WHO_WAVES_stepcount") |>
      any()

    if (chk_successful) {

      msg_step_env <- "Successfuly created"
      conda_install(
        envname  = "WHO_WAVES_stepcount",
        packages = "stepcount==3.17.0",
        forge    = FALSE,
        pip      = TRUE
      )
      tbl_pkgs_stepcount <- get_tbl_pkgs(
        df_pkgs_stepcount,
        conda_env = "WHO_WAVES_stepcount"
      )
      msg_step_pkg <- ifelse(
        all(tbl_pkgs_stepcount$`_data`$`Version WAVES` == tbl_pkgs_stepcount$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    } else {
      msg_step_env <- "Not created. Share report with WHO_WAVES team."
      msg_step_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_step_env <- "Already exists."

    # Check packages are installed.
    tbl_pkgs_stepcount <- get_tbl_pkgs(
      df_pkgs_stepcount,
      conda_env = "WHO_WAVES_stepcount"
    )
    chk_package <-
      all(tbl_pkgs_stepcount$`_data`$`Version WAVES` == tbl_pkgs_stepcount$`_data`$`Version Installed`)

    if (chk_package) {
      msg_step_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      conda_install(
        envname  = "WHO_WAVES_stepcount",
        packages = "stepcount==3.17.0",
        forge    = FALSE,
        pip      = TRUE
      )
      tbl_pkgs_stepcount <- get_tbl_pkgs(
        df_pkgs_stepcount,
        conda_env = "WHO_WAVES_stepcount"
      )
      msg_step_pkg <- ifelse(
        all(tbl_pkgs_stepcount$`_data`$`Version WAVES` == tbl_pkgs_stepcount$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    }
  }

  # accelerometer ----
  if (chk_accelerometer) {

    conda_create(
      envname = "WHO_WAVES_accelerometer",
      packages = "openjdk",
      forge = FALSE,
      python_version = 3.9,
      pip = TRUE
    )
    chk_successful <-
      list.files(
        file.path(miniconda_path(), "envs")
      ) |> grepl(x = _,
                 pattern = "WHO_WAVES_accelerometer") |>
      any()

    if (chk_successful) {

      msg_acc_env <- "Successfuly created"
      conda_install(
        envname  = "WHO_WAVES_accelerometer",
        packages = "accelerometer==7.3.0",
        forge    = FALSE,
        pip      = TRUE
      )
      tbl_pkgs_walmsley <- get_tbl_pkgs(
        df_pkgs_walmsley,
        conda_env = "WHO_WAVES_accelerometer"
      )
      msg_acc_pkg <- ifelse(
        all(tbl_pkgs_walmsley$`_data`$`Version WAVES` == tbl_pkgs_walmsley$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    } else {
      msg_acc_env <- "Not created. Share report with WHO_WAVES team."
      msg_acc_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_acc_env <- "Already exists."

    # Check packages are installed.
    tbl_pkgs_walmsley <- get_tbl_pkgs(
      df_pkgs_walmsley,
      conda_env = "WHO_WAVES_accelerometer"
    )
    chk_package <-
      all(tbl_pkgs_walmsley$`_data`$`Version WAVES` == tbl_pkgs_walmsley$`_data`$`Version Installed`)

    if (chk_package) {
      msg_acc_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      conda_install(
        envname  = "WHO_WAVES_accelerometer",
        packages = "accelerometer==7.3.0",
        forge    = FALSE,
        pip      = TRUE
      )
      tbl_pkgs_walmsley <- get_tbl_pkgs(
        df_pkgs_walmsley,
        conda_env = "WHO_WAVES_accelerometer"
      )
      msg_acc_pkg <- ifelse(
        all(tbl_pkgs_walmsley$`_data`$`Version WAVES` == tbl_pkgs_walmsley$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    }
  }

  # actinet ----
  if (chk_actinet) {

    conda_create(
      envname = "WHO_WAVES_actinet",
      packages = "openjdk",
      forge = FALSE,
      python_version = 3.9,
      pip = TRUE
    )
    chk_successful <-
      list.files(
        file.path(miniconda_path(), "envs")
      ) |> grepl(x = _,
                 pattern = "WHO_WAVES_actinet") |>
      any()

    if (chk_successful) {

      msg_acti_env <- "Successfuly created"
      conda_install(
        envname  = "WHO_WAVES_actinet",
        packages = "actinet==0.4.2",
        forge    = FALSE,
        pip      = TRUE
      )
      tbl_pkgs_actinet <- get_tbl_pkgs(
        df_pkgs_actinet,
        conda_env = "WHO_WAVES_actinet"
      )
      msg_acti_pkg <- ifelse(
        all(tbl_pkgs_actinet$`_data`$`Version WAVES` == tbl_pkgs_actinet$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    } else {
      msg_acti_env <- "Not created. Share report with WHO_WAVES team."
      msg_acti_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_acti_env <- "Already exists."

    # Check packages are installed.
    tbl_pkgs_actinet <- get_tbl_pkgs(
      df_pkgs_actinet,
      conda_env = "WHO_WAVES_actinet"
    )
    chk_package <-
      all(tbl_pkgs_actinet$`_data`$`Version WAVES` == tbl_pkgs_actinet$`_data`$`Version Installed`)

    if (chk_package) {
      msg_acti_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      conda_install(
        envname  = "WHO_WAVES_actinet",
        packages = "actinet==0.4.2",
        forge    = FALSE,
        pip      = TRUE
      )
      tbl_pkgs_actinet <- get_tbl_pkgs(
        df_pkgs_actinet,
        conda_env = "WHO_WAVES_actinet"
      )
      msg_acti_pkg <- ifelse(
        all(tbl_pkgs_actinet$`_data`$`Version WAVES` == tbl_pkgs_actinet$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    }
  }

  # oak_1.0 ----
  if (chk_oak_1.0) {

    # Install's of forest from  at least commit adada3f onwards don't work unless
    # timezonefinder module is installed beforehand. To lazy to open an issue
    # for it.
    conda_create(
      envname = "WHO_WAVES_oak_1.0",
      # packages = "timezonefinder==8.1.0",
      forge = FALSE,
      python_version = 3.12,
      pip = TRUE
    )
    conda_install(
      envname  = "WHO_WAVES_oak_1.0",
      packages = "timezonefinder==8.1.0",
      forge    = FALSE,
      channel  = "conda-forge"
    )
    chk_successful <-
      list.files(
        file.path(miniconda_path(), "envs")
      ) |> grepl(x = _,
                 pattern = "WHO_WAVES_oak_1.0") |>
      any()

    if (chk_successful) {

      msg_oak_env <- "Successfuly created"

      # The Forest module in the `Walking` R package from Muscheli is from
      # Dec 13, 2024 https://github.com/onnela-lab/forest/commit/45fb41038bd46c25d9e6a4442aa74fa03b501317
      # There has been one change since then regarding oak, where peak
      # identification logic was changed to better reflect the algorithm in the paper.
      # Issue: https://github.com/onnela-lab/forest/issues/290
      # Pull Request: https://github.com/onnela-lab/forest/pull/291

      # Commit: https://github.com/onnela-lab/forest/commit/adada3f1fb8d43b4d2c2a3451dbcbedcb3b52be4
      # Therefore, download from the creation of this function, 2025-11-18.
      # ffb36be508d6161e8fbfe70a27048e218cc9394d

      if (chk_windows) {
        system2(
          command = file.path(miniconda_path(), "Scripts", "activate.bat"),
          args = paste(
            "activate WHO_WAVES_oak_1.0",
            "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
            sep = " & "
          )
        )
      } else {
        # TODO CHECK
        # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_oak_1.0 ; pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d
        system2(
          command = "source",
          args = paste(
            paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
            "conda activate WHO_WAVES_oak_1.0",
            "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
            sep = " ; "
          )
        )
        # system2(
        #   command = paste0('source "', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        #   args = paste(
        #     "conda activate WHO_WAVES_oak_1.0",
        #     "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
        #     sep = " ; "
        #   )
        # )
      }

      tbl_pkgs_oak_1.0 <- get_tbl_pkgs(
        df_pkgs_oak_1.0,
        conda_env = "WHO_WAVES_oak_1.0"
      )
      msg_oak_pkg <- ifelse(
        all(tbl_pkgs_oak_1.0$`_data`$`Version WAVES` == tbl_pkgs_oak_1.0$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    } else {
      msg_oak_env <- "Not created. Share report with WHO_WAVES team."
      msg_oak_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_oak_env <- "Already exists."

    # Check packages are installed.
    tbl_pkgs_oak_1.0 <- get_tbl_pkgs(
      df_pkgs_oak_1.0,
      conda_env = "WHO_WAVES_oak_1.0"
    )
    chk_package <-
      all(tbl_pkgs_oak_1.0$`_data`$`Version WAVES` == tbl_pkgs_oak_1.0$`_data`$`Version Installed`)

    if (chk_package) {
      msg_oak_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      if (chk_windows) {
        system2(
          command = file.path(miniconda_path(), "Scripts", "activate.bat"),
          args = paste(
            "activate WHO_WAVES_oak_1.0",
            "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
            sep = " & "
          )
        )
      } else {
        # TODO CHECK
        # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_oak_1.0 ; pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d
        system2(
          command = "source",
          args = paste(
            paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
            "conda activate WHO_WAVES_oak_1.0",
            "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
            sep = " ; "
          )
        )
        # system2(
        #   command = paste0('source "', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        #   args = paste(
        #     "conda activate WHO_WAVES_oak_1.0",
        #     "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
        #     sep = " ; "
        #   )
        # )
      }

      tbl_pkgs_oak_1.0 <- get_tbl_pkgs(
        df_pkgs_oak_1.0,
        conda_env = "WHO_WAVES_oak_1.0"
      )
      msg_oak_pkg <- ifelse(
        all(tbl_pkgs_oak_1.0$`_data`$`Version WAVES` == tbl_pkgs_oak_1.0$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    }
  }

  # oak_pre-release ----
  if (chk_oak_pre) {

    # Install's of forest from  at least commit adada3f onwards don't work unless
    # timezonefinder module is installed beforehand. To lazy to open an issue
    # for it.
    conda_create(
      envname = "WHO_WAVES_oak_pre",
      forge = FALSE,
      python_version = 3.11, # match version in walking R package
      pip = TRUE
    )
    chk_successful <-
      list.files(
        file.path(miniconda_path(), "envs")
      ) |> grepl(x = _,
                 pattern = "WHO_WAVES_oak_pre") |>
      any()

    if (chk_successful) {

      msg_oak_pre_env <- "Successfuly created"

      # The Forest module in the `Walking` R package from Muscheli is from
      # Dec 13, 2024 https://github.com/onnela-lab/forest/commit/45fb41038bd46c25d9e6a4442aa74fa03b501317
      if (chk_windows) {
        system2(
          command = file.path(miniconda_path(), "Scripts", "activate.bat"),
          args = paste(
            "activate WHO_WAVES_oak_pre",
            "pip install git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317",
            sep = " & "
          )
        )
      } else {
        # TODO CHECK
        # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_oak_pre ; pip install git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317
        system2(
          command = "source",
          args = paste(
            paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
            "conda activate WHO_WAVES_oak_pre",
            "pip install git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317",
            sep = " ; "
          )
        )
        # system2(
        #   command = paste0('source "', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        #   args = paste(
        #     "conda activate WHO_WAVES_oak_pre",
        #     "pip install git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317",
        #     sep = " ; "
        #   )
        # )
      }

      tbl_pkgs_oak_pre <- get_tbl_pkgs(
        df_pkgs_oak_pre,
        conda_env = "WHO_WAVES_oak_pre"
      )
      msg_oak_pre_pkg <- ifelse(
        all(tbl_pkgs_oak_pre$`_data`$`Version WAVES` == tbl_pkgs_oak_pre$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

    } else {
      msg_oak_pre_env <- "Not created. Share report with WHO_WAVES team."
      msg_oak_pre_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_oak_pre_env <- "Already exists."

    # Check packages are installed.
    tbl_pkgs_oak_pre <- get_tbl_pkgs(
      df_pkgs_oak_pre,
      conda_env = "WHO_WAVES_oak_pre"
    )
    chk_package <-
      all(tbl_pkgs_oak_pre$`_data`$`Version WAVES` == tbl_pkgs_oak_pre$`_data`$`Version Installed`)

    if (chk_package) {
      msg_oak_pre_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      if (chk_windows) {
        system2(
          command = file.path(miniconda_path(), "Scripts", "activate.bat"),
          args = paste(
            "activate WHO_WAVES_oak_pre",
            "pip install git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317",
            sep = " & "
          )
        )
      } else {
        # TODO CHECK
        # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_oak_pre ; pip install git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317
        system2(
          command = "source",
          args = paste(
            paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
            "conda activate WHO_WAVES_oak_pre",
            "pip install git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317",
            sep = " ; "
          )
        )
        # system2(
        #   command = paste0('source "', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        #   args = paste(
        #     "conda activate WHO_WAVES_oak_pre",
        #     "pip install git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317",
        #     sep = " ; "
        #   )
        # )
      }

      tbl_pkgs_oak_pre <- get_tbl_pkgs(
        df_pkgs_oak_pre,
        conda_env = "WHO_WAVES_oak_pre"
      )
      msg_oak_pre_pkg <- ifelse(
        all(tbl_pkgs_oak_pre$`_data`$`Version WAVES` == tbl_pkgs_oak_pre$`_data`$`Version Installed`),
        yes = "Modules successfully installed.",
        no  = "Modules installated do not completely match WAVES configuration. Please see the table below for module versions that do not match or are completely missing."
      )

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
      msg_step_env,
      msg_step_pkg,
      msg_acc_env,
      msg_acc_pkg,
      msg_acti_env,
      msg_acti_pkg,
      msg_oak_env,
      msg_oak_pkg,
      msg_oak_pre_env,
      msg_oak_pre_pkg
    )
  )
  rownames(df_msg) <- c(
    "miniconda_install",
    "miniconda_version",
    "step_env",
    "step_pkg",
    "wlms_env",
    "wlms_pkg",
    "acti_env",
    "acti_pkg",
    "oak_1.0_env",
    "oak_1.0_pkg",
    "oak_pre_env",
    "oak_pre_pkg"
  )
  return(list(
    df_msg   = df_msg,
    tbl_step = tbl_pkgs_stepcount,
    tbl_wlms = tbl_pkgs_walmsley,
    tbl_acti = tbl_pkgs_actinet,
    tbl_oak1 = tbl_pkgs_oak_1.0,
    tbl_oakp = tbl_pkgs_oak_pre
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
        isTRUE(identical(header_value, "file does not have header")) &&
        isTRUE(identical(sf_value, 0))

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

  # Rename "GENEACTIVE" to "GENEACTIV".
  vct_type <- sub(
    x = vct_type,
    pattern = "GENEACTIVE",
    replacement = "GENEACTIV"
  )

  return(vct_type)

}
