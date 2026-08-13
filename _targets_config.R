####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                      INPUT                                ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(targets)
library(tarchetypes)
library(crew)
library(autometric)
Sys.setenv(
  TAR_PROJECT = "config",
  # change below environment variable if you already have a conda installation readily accessible.
  RETICULATE_MINICONDA_PATH = reticulate::miniconda_path()
)
n_workers <- 2 # future::availableCores() - 1

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
#                                      LOAD                                  ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load packages required to define the pipeline:
source("packages.R") |>
  suppressMessages() |>
  suppressWarnings()

# Run the R scripts in the R/ folder with your custom functions:
list.files(
  path       = "R",
  pattern    = "\\.R$",
  full.names = TRUE
) |>
  grep(x       = _,
       pattern = "^R\\/_",
       value   = TRUE,
       invert  = TRUE) |>
  sapply(FUN = source) |>
  invisible()

# Decompress config files.
chk_10004 <- !file.exists("data/0_CONFIG/RAW/WAVES_10004_RAW.gt3x")
chk_10005 <- !file.exists("data/0_CONFIG/RAW/WAVES_10005_RAW.cwa")

if (chk_10004) {R.utils::decompressFile(
  filename = "data/0_CONFIG/RAW/WAVES_10004_RAW.gt3x.xz",
  ext = "xz",
  FUN = base::xzfile,
  remove = FALSE
)}
if (chk_10005) {R.utils::decompressFile(
  filename = "data/0_CONFIG/RAW/WAVES_10005_RAW.cwa.xz",
  ext = "xz",
  FUN = base::xzfile,
  remove = FALSE
)}

# Make sure conda isn't being used by another process: TEST ON MAC
chk_conda <-
  reticulate::miniconda_path() |>
  dirname() |>
  file.path("Temp", "1") |>
  list.files(pattern = "conda_tmp",
             full.names = TRUE)

if (length(chk_conda) != 0) {
  stop(c(
    "A process within a conda environment was interrupted or is currently ongoing.
    Please only run the pipeline without any other processes running in the background
    or delete the temp file if a conda environment process was interrupted.",
    paste0('"', chk_conda, '"\n')
  ))
}

# Define directories
fdr_logs <- file.path(
  "logs"
)
fdr_ggir <- file.path(
  "data", "0_CONFIG", "GGIR"
)
fdr_calibrated <- file.path(
  "data", "0_CONFIG", "RAW-CALIBRATED_QS2"
)
fdr_nonwear.sleep <- file.path(
  "data", "0_CONFIG", "NONWEAR-SLEEP_PARQUET"
)
fdr_output.oxwearable <- file.path(
  "data", "0_CONFIG", "OUTPUT-OXWEARABLE_PARQUET"
)
fdr_output.cutpoint <- file.path(
  "data", "0_CONFIG", "OUTPUT-CUTPOINT_PARQUET"
)
fdr_output.raw <- file.path(
  "data", "0_CONFIG", "OUTPUT-RAW_PARQUET"
)
fdr_output.oak.pre <- file.path(
  "data", "0_CONFIG", "OUTPUT-OAK-PRE_PARQUET"
)
fdr_stepcount <- file.path(
  "data", "0_CONFIG", "stepcount"
)
fdr_walmsley <- file.path(
  "data", "0_CONFIG", "walmsley"
)
fdr_actinet <- file.path(
  "data", "0_CONFIG", "actinet"
)
fdr_merged <- file.path(
  "data", "0_CONFIG", "MERGED"
)
fdr_reports <- file.path(
  "reports"
)
# fdr_merged_
fs::dir_create(c(
  fdr_logs,
  fdr_ggir,
  fdr_calibrated,
  fdr_nonwear.sleep,
  fdr_output.oxwearable,
  fdr_output.cutpoint,
  fdr_output.raw,
  fdr_output.oak.pre,
  fdr_stepcount,
  fdr_walmsley,
  fdr_actinet,
  fdr_merged,
  fdr_reports
))

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
#                                    OPTIONS                                 ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
options(warnPartialMatchDollar = TRUE)
options(warnPartialMatchArgs = TRUE)
options(datatable.print.class = TRUE)
options(datatable.print.keys = TRUE)

# Set target options:
tar_option_set(
  format     = "qs",
  controller = crew_controller_local(
    name = "my_controller",
    workers = n_workers,
    options_metrics = crew_options_metrics(
      path = "/dev/stdout",
      seconds_interval = 1
    ),
    options_local = crew_options_local(
      log_directory = "logs/workers_config/"
    )
  )
  # trust_timestamps = TRUE
)

# Sys.setenv(OMP_NUM_THREADS = 1)   # For OpenMP-based libs, testing Oak parallel processing
# Sys.setenv(OPENBLAS_NUM_THREADS = 1)
# Sys.setenv(MKL_NUM_THREADS = 1)
# Sys.setenv(NUMEXPR_NUM_THREADS = 1)
# Sys.setenv(NUMBA_NUM_THREADS = 1)
# Sys.setenv(SSQ_PARALLEL = 0)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
#                                  DEBUGGING                                 ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# If there are warnings, run the following in the console:
# df_warnings <-
#   tar_meta(fields = warnings, complete_only = TRUE) |>
#   dplyr::reframe(
#     warnings =
#       warnings |>
#       stringi::stri_replace_all(regex = "\\d{1,3}m\\d{1,3}m",
#                                 replacement = "") |>
#       # stringi::stri_replace_all(regex = "36mâ„¹39m",
#       #                           replacement = "") |>
#       stringi::stri_split(regex = "\\.\\. ") |>
#       unlist(),
#     .by = name
#   )
# data.table::fwrite(df_warnings,
#                    file = "_targets_warnings.csv",
#                    sep = ",",
#                    bom = TRUE)
# This will give you the warnings per target that occurred.

# After running INPUT and un-commenting the `cue` and `debug` arguments within
# tar_option_set(), run the below code if debugging.
# tar_make(callr_function = NULL, use_crew = FALSE, as_job = FALSE)

# Run tar_workspaces() to see which branch(s) errored.
# After finding the branch that failed, run tar_workspace([YOUR_BRANCH]) where
# [YOUR_BRANCH] is  one of the branches from tar_workspaces().
# For example: tar_workspace(vct_out.raw_dafe56fca98478db)

# The pipeline will run each file in a "branch" for each target. Because the
# pipeline will not know ahead of time which files will be moving forward throughout
# the pipeline, each branch will have a unique name that isn't interpretable.
# If a branch errors and we want to figure out which file did not make it through,
# we can refer to "summary_pipeline_config.html" generated by the pipeline.

# If for some reason the pipeline stops completely and "summary_pipeline_config.html"
# is not created, see if the following works (using the `vct_out.raw` target
# as an example with the branch name as `vct_out.raw_d52af82974edb9df`):
# # 1st: load up the workspace for the branch, which will load all packages, functions
# # and upstream targets.
# tar_workspace(vct_out.raw_d52af82974edb9df)

# # `tar_workspace()` unfortunately doesn't have the upstream targets named after
# # the arguments within the target command. Need to do this manually:
# fpa_read      = vct_cal
# vct_fpa_basic = vct_basic
# dir_models    = dir_models
# dir_write     = dir_out.raw
# df_start_tz   = df_start_tz # technically don't need to do this since the argument name matches the upstream target name!
# lst_miniconda = lst_miniconda # technically don't need to do this since the argument name matches the upstream target name!

# # From there, run the function in the vct_out.raw target (apply_methods_raw()
# # in the R/apply_methods_raw.R script) until the error pops up!

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
#                                    TARGETS                                 ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Start logging.
if (tar_active()) {
  log_start(
    path = file.path("logs", "pipeline_config.log"), # Statistics on the main process go here.
    seconds = 1
  )
}

tar_plan(
  lst_yaml = parse_waves_yaml(),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             MINICONDA SETUP                            ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_file_read(
    name    = df_module_stepcount,
    command = file.path("data", "0_CONFIG", "RAW", "df_modules_stepcount.csv"),
    read    = fread(!!.x, sep = ","),
    format  = "parquet"
  ),
  tar_file_read(
    name    = df_module_walmsley,
    command = file.path("data", "0_CONFIG", "RAW", "df_modules_walmsley.csv"),
    read    = fread(
      !!.x,
      sep        = ",",
      colClasses = list(character=c("Module","Version WAVES","Channel"))
    ),
    format  = "parquet"
  ),
  tar_file_read(
    name    = df_module_actinet,
    command = file.path("data", "0_CONFIG", "RAW", "df_modules_actinet.csv"),
    read    = fread(
      !!.x,
      sep        = ",",
      colClasses = list(character=c("Module","Version WAVES","Channel"))
    ),
    format  = "parquet"
  ),
  tar_file_read(
    name    = df_module_oak_1.0,
    command = file.path("data", "0_CONFIG", "RAW", "df_modules_oak1.0.csv"),
    read    = fread(
      !!.x,
      sep        = ",",
      colClasses = list(character=c("Module","Version WAVES","Channel"))
    ),
    format  = "parquet"
  ),
  tar_file_read(
    name    = df_module_oak_pre,
    command = file.path("data", "0_CONFIG", "RAW", "df_modules_oakpre.csv"),
    read    = fread(
      !!.x,
      sep        = ",",
      colClasses = list(character=c("Module","Version WAVES","Channel"))
    ),
    format  = "parquet"
  ),
  tar_qs(
    name = lst_miniconda,
    command = config_miniconda(
      df_module_stepcount,
      df_module_walmsley,
      df_module_actinet,
      df_module_oak_1.0,
      df_module_oak_pre
    ),
    cue = tar_cue(mode = "always")
  ),
  if (reticulate:::is_linux()) {
    tar_render(
      name = minconda_summary,
      path = "quarto/config_miniconda.qmd",
      output_file = file.path(getwd(), fdr_reports, "summary_miniconda.html")
    )
  } else {
    tar_quarto(
      name = minconda_summary,
      path = "quarto/config_miniconda.qmd",
      output_file = file.path(getwd(), fdr_reports, "summary_miniconda.html")
    )
  },
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             FILE DIRECTORIES                           ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  dir_models      = "models",
  dir_logs        = fdr_logs,
  dir_cal         = fdr_calibrated,
  dir_nw.sleep    = fdr_nonwear.sleep,
  dir_out.ox      = fdr_output.oxwearable,
  dir_out.cut     = fdr_output.cutpoint,
  dir_out.raw     = fdr_output.raw,
  dir_out.oak.pre = fdr_output.oak.pre,
  dir_stepcount   = fdr_stepcount,
  dir_walmsley    = fdr_walmsley,
  dir_actinet     = fdr_actinet,
  dir_merged      = fdr_merged,
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                                FILE PATHS                              ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_files(
    name    = vct_raw,
    command = lst_yaml$vct_raw_fpa
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                               DEMOGRAPHICS                             ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                              PROCESS - GGIR                            ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Reading the geneactiv csv files fails at part 1 of GGIR. From my testing,
  # the csv files can be read successfully with GGIR's `read.myacc.csv` function
  # but somewhere else along Part 1 doesn't like what `read.myacc.csv` outputs
  # or I am specifying an argument wrong.
  vct_raw_type = config_raw_type(vct_raw),
  tar_files(
    name    = vct_basic,
    command = wrapper_GGIR_config(vct_raw,
                                  vct_raw_type)
  ),
  tar_parquet(
    name    = df_start_tz,
    command = get_start_tz_df(vct_basic,
                              my_tz = lst_yaml$my_tz)
  ),
  tar_file(
    name      = vct_nw.sleep,
    command   = get_nonwear_sleep(
      fpa_basic   = vct_basic,
      df_start_tz = df_start_tz,
      fdr_write   = dir_nw.sleep
    ),
    pattern   = map(vct_basic),
    iteration = "vector"
  ),
  tar_file(
    name      = vct_out.cut,
    command   = apply_methods_cutpoints(
      fpa_basic = vct_basic,
      dir_write = dir_out.cut
    ),
    pattern   = map(vct_basic),
    iteration = "vector"
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                           PROCESS - OXWEARABLES                        ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  vct_ox_input = prepare_ox_input(
    vct_raw,
    vct_raw_type,
    vct_basic
  ),
  tar_file(
    name    = vct_ox_step,
    command = apply_ox_stepcount(
      ox_input      = vct_ox_input,
      fdr_write     = file.path(getwd(), dir_stepcount),
      fdr_log       = dir_logs,
      log_prefix    = "config_",
      lst_miniconda = lst_miniconda
    ),
    pattern   = map(vct_ox_input),
    iteration = "vector"
  ),
  tar_file(
    name    = vct_ox_wlms,
    command = apply_ox_walmsley(
      ox_input      = vct_ox_input,
      fdr_write     = file.path(getwd(), dir_walmsley),
      fdr_log       = dir_logs,
      log_prefix    = "config_",
      lst_miniconda = lst_miniconda
    ),
    pattern   = map(vct_ox_input),
    iteration = "vector"
  ),
  tar_file(
    name    = vct_ox_acti,
    command = apply_ox_actinet(
      ox_input      = vct_ox_input,
      fdr_write     = file.path(getwd(), dir_actinet),
      fdr_log       = dir_logs,
      log_prefix    = "config_",
      lst_miniconda = lst_miniconda
    ),
    pattern   = map(vct_ox_input),
    iteration = "vector"
  ),
  tar_file(
    name    = vct_ox,
    command = merge_ox(
      vct_ox_step  = vct_ox_step,
      vct_ox_wlms  = vct_ox_wlms,
      vct_ox_acti  = vct_ox_acti,
      dir_write    = dir_out.ox,
      vct_raw_type = vct_raw_type,
      df_start_tz  = df_start_tz
    )
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             PROCESS - CUSTOM                           ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_file(
    name      = vct_cal,
    command   = read_acc_raw(
      fpa_read      = vct_raw,
      le_type       = vct_raw_type,
      vct_fpa_basic = vct_basic,
      dir_cal       = dir_cal
    ),
    pattern   = map(vct_raw, vct_raw_type),
    iteration = "vector"
  ),
  tar_file(
    name      = vct_out.raw,
    command   = apply_methods_raw(
      fpa_read      = vct_cal,
      vct_fpa_basic = vct_basic,
      dir_models    = dir_models,
      dir_write     = dir_out.raw,
      df_start_tz   = df_start_tz,
      lst_miniconda = lst_miniconda
    ),
    pattern   = map(vct_cal),
    iteration = "vector",
    error = "null"
  ),
  tar_file(
    name      = vct_out.oak.pre,
    command   = apply_oak.pre(
      fpa_read      = vct_cal,
      vct_fpa_basic = vct_basic,
      dir_write     = dir_out.oak.pre,
      df_start_tz   = df_start_tz,
      lst_miniconda = lst_miniconda
    ),
    pattern   = map(vct_cal),
    iteration = "vector",
    error = "null",
    deployment = "main" # in order to avoid error of loading two conda environments within the same R session
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                                   MERGE                                ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_parquet(
    name    = df_pipe,
    command = merge_output_config(
      vct_nw.sleep    = vct_nw.sleep,
      vct_out.raw     = vct_out.raw,
      vct_out.oak.pre = vct_out.oak.pre,
      vct_out.cut     = vct_out.cut,
      vct_ox          = vct_ox
    )
  ),
  if (reticulate:::is_linux()) {
    tar_render(
      name = pipeline_summary,
      path = "quarto/pipeline_config.qmd",
      output_file = file.path(getwd(), fdr_reports, "summary_pipeline_config.html")
    )
  } else {
    tar_quarto(
      name = pipeline_summary,
      path = "quarto/pipeline_config.qmd",
      output_file = file.path(getwd(), fdr_reports, "summary_pipeline_config.html")
    )
  }
)
