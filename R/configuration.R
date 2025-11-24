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
  chk_oak <- !condaenv_exists("WHO_WAVES_oak")
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
  vct_pkg_oak <- c(
    "audioread_3.1.0",
    "beiwe-forest_1.0",
    "bzip2_1.0.8",
    "ca-certificates_2025.11.12",
    "certifi_2025.11.12",
    "cffi_2.0.0",
    "charset-normalizer_3.4.4",
    "decorator_5.2.1",
    "h3_4.3.0",
    "h3-py_4.3.0",
    "holidays_0.85",
    "idna_3.11",
    "joblib_1.5.2",
    "lazy-loader_0.4",
    "libblas_3.11.0",
    "libcblas_3.11.0",
    "libexpat_2.7.3",
    "libffi_3.5.2",
    "libhwloc_2.12.1",
    "libiconv_1.18",
    "liblapack_3.11.0",
    "liblzma_5.8.1",
    "librosa_0.11.0",
    "libsqlite_3.51.0",
    "libwinpthread_12.0.0.r4.gg4f2fc60ca",
    "libxml2_2.15.1",
    "libxml2-16_2.15.1",
    "libzlib_1.3.1",
    "llvm-openmp_21.1.6",
    "llvmlite_0.45.1",
    "mkl_2025.3.0",
    "msgpack_1.1.2",
    "numba_0.62.1",
    "numpy_2.3.5",
    "openrouteservice_2.3.3",
    "openssl_3.6.0",
    "packaging_25.0",
    "pandas_2.3.3",
    "pip_25.3",
    "platformdirs_4.5.0",
    "pooch_1.8.2",
    "pycparser_2.22",
    "pyproj_3.7.2",
    "python_3.12.12",
    "python-dateutil_2.9.0.post0",
    "python-flatbuffers_25.9.23",
    "python_abi_3.12",
    "pytz_2025.2",
    "ratelimit_2.2.1",
    "requests_2.32.5",
    "scikit-learn_1.7.2",
    "scipy_1.16.3",
    "setuptools_80.9.0",
    "shapely_2.1.2",
    "six_1.17.0",
    "soundfile_0.13.1",
    "soxr_1.0.0",
    "ssqueezepy_0.6.6",
    "tbb_2022.3.0",
    "threadpoolctl_3.6.0",
    "timezonefinder_8.1.0",
    "tk_8.6.13",
    "typing-extensions_4.15.0",
    "tzdata_2025.2",
    "ucrt_10.0.26100.0",
    "urllib3_2.5.0",
    "vc_14.3",
    "vc14_runtime_14.44.35208",
    "vcomp14_14.44.35208",
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

  # oak ----
  if (chk_oak) {

    # Install's of forest from  at least commit adada3f onwards don't work unless
    # timezonefinder module is installed beforehand. To lazy to open an issue
    # for it.
    conda_create(
      envname = "WHO_WAVES_oak",
      packages = "timezonefinder==8.1.0",
      forge = TRUE,
      python_version = 3.12,
      pip = TRUE
    )
    chk_successful <- condaenv_exists("WHO_WAVES_oak")

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

      chk_windows <- grepl(
        x = Sys.getenv("OS"),
        pattern = "windows",
        ignore.case = TRUE
      )

      if (chk_windows) {
        system2(
          command = file.path(miniconda_path(), "Scripts", "activate.bat"),
          args = paste(
            "activate WHO_WAVES_oak",
            "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
            sep = " & "
          )
        )
      } else {
        # TODO CHECK
        # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_oak ; pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d
        system2(
          command = "source",
          args = paste(
            paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
            "conda activate WHO_WAVES_oak",
            "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
            sep = " ; "
          )
        )
        # system2(
        #   command = paste0('source "', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        #   args = paste(
        #     "conda activate WHO_WAVES_oak",
        #     "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
        #     sep = " ; "
        #   )
        # )
      }

      vct_installed <-
        py_list_packages("WHO_WAVES_oak") |>
        unite(col = "pkg", package, version) |>
        pull(pkg)
      chk_package <- all(
        vct_pkg_oak %in% vct_installed
      )

      if (chk_package) {
        msg_oak_pkg <- "Modules successfully installed."
      } else {
        vct_fudge <- vct_installed[!vct_pkg_oak %in% vct_installed]
        msg_oak_pkg <- paste0(
          "Unsuccessful module installation. The following modules were not installed: ",
          paste0('"', vct_fudge, '"',  collapse = " "), ". Share with WHO WAVES team."
        )
      }

    } else {
      msg_oak_env <- "Not created. Share report with WHO_WAVES team."
      msg_oak_pkg <- "Environment not created, no packages installed."
    }

  } else {

    msg_oak_env <- "Already exists."

    # Check packages are installed.
    vct_installed <-
      py_list_packages("WHO_WAVES_oak") |>
      unite(col = "pkg", package, version) |>
      pull(pkg)
    chk_package <- all(
      vct_pkg_oak %in% vct_installed
    )

    if (chk_package) {
      msg_oak_pkg <- "Modules already installed."
    } else {

      # Install modules and check one more time afterwards.
      chk_windows <- grepl(
        x = Sys.getenv("OS"),
        pattern = "windows",
        ignore.case = TRUE
      )

      if (chk_windows) {
        system2(
          command = file.path(miniconda_path(), "Scripts", "activate.bat"),
          args = paste(
            "activate WHO_WAVES_oak",
            "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
            sep = " & "
          )
        )
      } else {
        # TODO CHECK
        # source C:/Users/marti994/AppData/Local/r-miniconda/etc/profile.d/conda.sh ; conda activate WHO_WAVES_oak ; pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d
        system2(
          command = "source",
          args = paste(
            paste0('"', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
            "conda activate WHO_WAVES_oak",
            "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
            sep = " ; "
          )
        )
        # system2(
        #   command = paste0('source "', file.path(miniconda_path(), "etc", "profile.d", "conda.sh"), '"'),
        #   args = paste(
        #     "conda activate WHO_WAVES_oak",
        #     "pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d",
        #     sep = " ; "
        #   )
        # )
      }

      vct_installed <-
        py_list_packages("WHO_WAVES_oak") |>
        unite(col = "pkg", package, version) |>
        pull(pkg)
      chk_package <- all(
        vct_pkg_oak %in% vct_installed
      )

      if (chk_package) {
        msg_oak_pkg <- "Modules successfully installed."
      } else {
        vct_fudge <- vct_installed[!vct_pkg_oak %in% vct_installed]
        msg_oak_pkg <- paste0(
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
      "actinet modules",
      "oak environment",
      "oak modules"
    ),
    `Message/Value` = c(
      msg_conda,
      conda_version(),
      msg_step_env,
      msg_step_pkg,
      msg_acc_env,
      msg_acc_pkg,
      msg_acti_env,
      msg_acti_pkg,
      msg_oak_env,
      msg_oak_pkg
    )
  )

}
config_pipeline <- function(vct_raw,
                            vct_basic,
                            vct_cal,
                            lst_out.raw,
                            lst_out.cut,
                            vct_ox_step,
                            vct_ox_wlms,
                            vct_ox_acti,
                            fpa_merged) {
  tibble(
    file =
      basename(vct_raw) |>
      file_path_sans_ext(),
    GGIR = grepl(
      x = vct_basic,
      pattern = paste0(file, collapse = "|")
    ),
    calibration = grepl(
      x = vct_cal,
      pattern = paste0(file, collapse = "|")
    ),
    `raw methods` = grepl(
      x = sapply(lst_out.raw, \(.x) .x$id[1]),
      pattern = paste0(file, collapse = "|")
    ),
    `cutpoints` = grepl(
      x = sapply(lst_out.cut, \(.x) .x$id[1]),
      pattern = paste0(file, collapse = "|")
    ),
    stepcount = grepl(
      x = vct_ox_step,
      pattern = paste0(file, collapse = "|")
    ),
    walmsley = grepl(
      x = vct_ox_wlms,
      pattern = paste0(file, collapse = "|")
    ),
    actinet = grepl(
      x = vct_ox_acti,
      pattern = paste0(file, collapse = "|")
    ),
    merged = grepl(
      x =
        read_parquet(fpa_merged, col_select = "id") |>
        unique() |>
        unlist(),
      pattern = paste0(file, collapse = "|")
    )
  )
}
