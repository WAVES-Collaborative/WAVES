config_miniconda <- function(...) {

  # conda ----
  chk_conda <-
    class(tryCatch(conda_version(), error = \(e) e)) == "simpleError"

  if (chk_conda) {

    install_miniconda()
    chk_successful <-
      class(tryCatch(conda_version(), error = \(e) e)) != "simpleError"

    if (chk_successful) {msg_conda <- paste0(
      "Not previously installed. Installed at ", '"',
      miniconda_path(), '"'
    )} else {msg_conda <- paste0(
      'Unsuccessful installation. Share report with WHO_WAVES team.'
    )}

  } else {

    # If they already have miniconda installed, user may have defined their
    # own path to conda.
    chk_path <-
      Sys.getenv("RETICULATE_MINICONDA_PATH") == ""

    if (chk_path) {msg_conda <- paste0(
      "Already installed. Found at ", '"', miniconda_path(), '"'
    )} else {msg_conda <- paste0(
      "Already installed. Found at ", '"', Sys.getenv("RETICULATE_MINICONDA_PATH"), '"'
    )}

  }

  # setup environment ----
  chk_stepcount <- !condaenv_exists("WHO_WAVES_stepcount")
  chk_accelerometer <- !condaenv_exists("WHO_WAVES_accelerometer")
  chk_actinet <- !condaenv_exists("WHO_WAVES_actinet")
  vct_pkg_step <- c(
    "actipy_3.8.0",
    "bzip2_1.0.8",
    "ca-certificates_2025.11.12",
    "certifi_2025.11.12",
    "charset-normalizer_3.4.4",
    "colorama_0.4.6",
    "hmmlearn_0.3.3",
    "idna_3.11",
    "imbalanced-learn_0.9.1",
    "joblib_1.2.0",
    "libexpat_2.7.1",
    "libffi_3.5.2",
    "liblzma_5.8.1",
    "libsqlite_3.51.0",
    "libzlib_1.3.1",
    "numpy_1.24.4",
    "openjdk_25.0.1",
    "openssl_3.6.0",
    "packaging_25.0",
    "pandas_2.0.3",
    "patsy_1.0.2",
    "pillow_11.3.0",
    "pip_25.2",
    "python_3.9.23",
    "requests_2.32.5",
    "scikit-learn_1.1.1",
    "scipy_1.10.1",
    "setuptools_80.9.0",
    "statsmodels_0.14.5",
    "stepcount_3.5.0",
    "symlink-exe-runtime_1.0",
    "tk_8.6.13",
    "torch_1.13.1",
    "torchvision_0.14.1",
    "tqdm_4.64.1",
    "transforms3d_0.4.2",
    "typing-extensions_4.15.0",
    "tzdata_2025.2",
    "ucrt_10.0.26100.0",
    "urllib3_2.5.0",
    "vc_14.3",
    "vc14_runtime_14.44.35208",
    "vcomp14_14.44.35208",
    "vs2015_runtime_14.44.35208",
    "wheel_0.45.1"
  )
  vct_pkg_acc <- c(
    "accelerometer_7.3.0",
    "bzip2_1.0.8",
    "ca-certificates_2025.11.12",
    "colorama_0.4.6",
    "cycler_0.12.1",
    "fonttools_4.60.1",
    "imbalanced-learn_0.8.1",
    "joblib_1.1.1",
    "kiwisolver_1.4.7",
    "libexpat_2.7.1",
    "libffi_3.5.2",
    "liblzma_5.8.1",
    "libsqlite_3.51.0",
    "libzlib_1.3.1",
    "matplotlib_3.5.3",
    "numpy_1.21.6",
    "openjdk_25.0.1",
    "openssl_3.6.0",
    "packaging_25.0",
    "pandas_1.3.5",
    "patsy_1.0.2",
    "pillow_11.3.0",
    "pip_25.2",
    "pyparsing_3.2.5",
    "python_3.9.23",
    "scikit-learn_1.0.2",
    "scipy_1.7.3",
    "setuptools_80.9.0",
    "statsmodels_0.13.5",
    "symlink-exe-runtime_1.0",
    "tk_8.6.13",
    "tqdm_4.65.2",
    "tzdata_2025b",
    "ucrt_10.0.26100.0",
    "vc_14.3",
    "vc14_runtime_14.44.35208",
    "vcomp14_14.44.35208",
    "vs2015_runtime_14.44.35208",
    "wheel_0.45.1"
  )
  vct_pkg_acti <- c(
    "actinet_0.4.2",
    "actipy_3.8.0",
    "bzip2_1.0.8",
    "ca-certificates_2025.11.12",
    "certifi_2025.11.12",
    "charset-normalizer_3.4.4",
    "colorama_0.4.6",
    "cycler_0.12.1",
    "fonttools_4.60.1",
    "idna_3.11",
    "imbalanced-learn_0.9.1",
    "joblib_1.2.0",
    "kiwisolver_1.4.7",
    "libexpat_2.7.1",
    "libffi_3.5.2",
    "liblzma_5.8.1",
    "libsqlite_3.51.0",
    "libzlib_1.3.1",
    "matplotlib_3.5.3",
    "numpy_1.24.4",
    "openjdk_25.0.1",
    "openssl_3.6.0",
    "packaging_25.0",
    "pandas_2.0.3",
    "patsy_1.0.2",
    "pillow_11.3.0",
    "pip_25.2",
    "pyparsing_3.2.5",
    "python_3.9.23",
    "requests_2.32.5",
    "scikit-learn_1.1.1",
    "scipy_1.10.1",
    "setuptools_80.9.0",
    "statsmodels_0.14.5",
    "symlink-exe-runtime_1.0",
    "tk_8.6.13",
    "torch_1.13.1",
    "torchvision_0.14.1",
    "tqdm_4.64.1",
    "transforms3d_0.4.2",
    "typing-extensions_4.15.0",
    "tzdata_2025.2",
    "ucrt_10.0.26100.0",
    "urllib3_2.5.0",
    "vc_14.3",
    "vc14_runtime_14.44.35208",
    "vcomp14_14.44.35208",
    "vs2015_runtime_14.44.35208",
    "wheel_0.45.1"
  )

  # stepcount ----
  if (chk_stepcount) {

    conda_create(
      envname = "WHO_WAVES_stepcount",
      packages = "openjdk",
      forge = TRUE,
      python_version = 3.9,
      pip = TRUE
    )
    chk_successful <- condaenv_exists("WHO_WAVES_stepcount")

    if (chk_successful) {

      msg_step_env <- "Successfuly created"
      conda_install(
        envname  = "WHO_WAVES_stepcount",
        packages = "stepcount==3.5",
        forge    = FALSE,
        pip      = TRUE
      )
      vct_installed <-
        py_list_packages("WHO_WAVES_stepcount") |>
        unite(col = "pkg", package, version) |>
        pull(pkg)
      chk_package <- all(
        vct_pkg_step %in% vct_installed
      )

      if (chk_package) {
        msg_step_pkg <- "Modules successfully installed."
      } else {
        vct_fudge <- vct_installed[!vct_pkg_step %in% vct_installed]
        msg_step_pkg <- paste0(
          "Unsuccessful module installation. The following modules were not installed: ",
          paste0('"', vct_fudge, '"',  collapse = " "), ". Share with WHO WAVES team."
        )
      }

    } else {
      msg_step_env <- "Not created. Share report with WHO_WAVES team."
      msg_step_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_step_env <- "Already exists."

    # Check packages are installed.
    vct_installed <-
      py_list_packages("WHO_WAVES_stepcount") |>
      unite(col = "pkg", package, version) |>
      pull(pkg)
    chk_package <- all(
      vct_pkg_step %in% vct_installed
    )

    if (chk_package) {
      msg_step_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      conda_install(
        envname  = "WHO_WAVES_stepcount",
        packages = "stepcount==3.5",
        forge    = TRUE,
        pip      = TRUE
      )

      vct_installed <-
        py_list_packages("WHO_WAVES_stepcount") |>
        unite(col = "pkg", package, version) |>
        pull(pkg)
      chk_package <- all(
        vct_pkg_step %in% vct_installed
      )

      if (chk_package) {
        msg_step_pkg <- "Modules successfully installed."
      } else {
        vct_fudge <- vct_installed[!vct_pkg_step %in% vct_installed]
        msg_step_pkg <- paste0(
          "Unsuccessful module installation. The following modules were not installed: ",
          paste0('"', vct_fudge, '"',  collapse = " "), ". Share with WHO WAVES team."
        )

      }
    }
  }

  # accelerometer ----
  if (chk_accelerometer) {

    conda_create(
      envname = "WHO_WAVES_accelerometer",
      packages = "openjdk",
      forge = TRUE,
      python_version = 3.9,
      pip = TRUE
    )
    chk_successful <- condaenv_exists("WHO_WAVES_accelerometer")

    if (chk_successful) {

      msg_acc_env <- "Successfuly created"
      conda_install(
        envname  = "WHO_WAVES_accelerometer",
        packages = "accelerometer==7.3.0",
        forge    = FALSE,
        pip      = TRUE
      )
      vct_installed <-
        py_list_packages("WHO_WAVES_accelerometer") |>
        unite(col = "pkg", package, version) |>
        pull(pkg)
      chk_package <- all(
        vct_pkg_acc %in% vct_installed
      )

      if (chk_package) {
        msg_acc_pkg <- "Modules successfully installed."
      } else {
        vct_fudge <- vct_installed[!vct_pkg_acc %in% vct_installed]
        msg_acc_pkg <- paste0(
          "Unsuccessful module installation. The following modules were not installed: ",
          paste0('"', vct_fudge, '"',  collapse = " "), ". Share with WHO WAVES team."
        )
      }

    } else {
      msg_acc_env <- "Not created. Share report with WHO_WAVES team."
      msg_acc_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_acc_env <- "Already exists."

    # Check packages are installed.
    vct_installed <-
      py_list_packages("WHO_WAVES_accelerometer") |>
      unite(col = "pkg", package, version) |>
      pull(pkg)
    chk_package <- all(
      vct_pkg_acc %in% vct_installed
    )

    if (chk_package) {
      msg_acc_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      conda_install(
        envname  = "WHO_WAVES_accelerometer",
        packages = "accelerometer==7.3.0",
        forge    = TRUE,
        pip      = TRUE
      )

      vct_installed <-
        py_list_packages("WHO_WAVES_accelerometer") |>
        unite(col = "pkg", package, version) |>
        pull(pkg)
      chk_package <- all(
        vct_pkg_acc %in% vct_installed
      )

      if (chk_package) {
        msg_acc_pkg <- "Modules successfully installed."
      } else {
        vct_fudge <- vct_installed[!vct_pkg_acc %in% vct_installed]
        msg_acc_pkg <- paste0(
          "Unsuccessful module installation. The following modules were not installed: ",
          paste0('"', vct_fudge, '"',  collapse = " "), ". Share with WHO WAVES team."
        )

      }
    }
  }

  # actinet ----
  if (chk_actinet) {

    conda_create(
      envname = "WHO_WAVES_actinet",
      packages = "openjdk",
      forge = TRUE,
      python_version = 3.9,
      pip = TRUE
    )
    chk_successful <- condaenv_exists("WHO_WAVES_actinet")

    if (chk_successful) {

      msg_acti_env <- "Successfuly created"
      conda_install(
        envname  = "WHO_WAVES_actinet",
        packages = "actinet==0.4.2",
        forge    = FALSE,
        pip      = TRUE
      )
      vct_installed <-
        py_list_packages("WHO_WAVES_actinet") |>
        unite(col = "pkg", package, version) |>
        pull(pkg)
      chk_package <- all(
        vct_pkg_acti %in% vct_installed
      )

      if (chk_package) {
        msg_acti_pkg <- "Modules successfully installed."
      } else {
        vct_fudge <- vct_installed[!vct_pkg_acti %in% vct_installed]
        msg_acti_pkg <- paste0(
          "Unsuccessful module installation. The following modules were not installed: ",
          paste0('"', vct_fudge, '"',  collapse = " "), ". Share with WHO WAVES team."
        )
      }

    } else {
      msg_acti_env <- "Not created. Share report with WHO_WAVES team."
      msg_acti_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_acti_env <- "Already exists."

    # Check packages are installed.
    vct_installed <-
      py_list_packages("WHO_WAVES_actinet") |>
      unite(col = "pkg", package, version) |>
      pull(pkg)
    chk_package <- all(
      vct_pkg_acti %in% vct_installed
    )

    if (chk_package) {
      msg_acti_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      conda_install(
        envname  = "WHO_WAVES_actinet",
        packages = "actinet==0.4.2",
        forge    = TRUE,
        pip      = TRUE
      )

      vct_installed <-
        py_list_packages("WHO_WAVES_actinet") |>
        unite(col = "pkg", package, version) |>
        pull(pkg)
      chk_package <- all(
        vct_pkg_acti %in% vct_installed
      )

      if (chk_package) {
        msg_acti_pkg <- "Modules successfully installed."
      } else {
        vct_fudge <- vct_installed[!vct_pkg_acti %in% vct_installed]
        msg_acti_pkg <- paste0(
          "Unsuccessful module installation. The following modules were not installed: ",
          paste0('"', vct_fudge, '"',  collapse = " "), ". Share with WHO WAVES team."
        )

      }
    }
  }

  # return ----
  data.frame(
    Item = c(
      "Miniconda",
      "Miniconda Version",
      "stepcount environment",
      "stepcount modules",
      "accelerometer environment",
      "accelerometer modules",
      "actinet environment",
      "actinet modules"
    ),
    `Message/Value` = c(
      msg_conda,
      conda_version(),
      msg_step_env,
      msg_step_pkg,
      msg_acc_env,
      msg_acc_pkg,
      msg_acti_env,
      msg_acti_pkg
    )
  )

}
