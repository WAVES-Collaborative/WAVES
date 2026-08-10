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
  TAR_PROJECT = "main",
  # change below environment variable if you already have a conda installation readily accessible.
  RETICULATE_MINICONDA_PATH = reticulate::miniconda_path()
)
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
fdr_calibrated <- file.path(
  "data", "2_INTERIM", "RAW-CALIBRATED_QS2"
)
fdr_nonwear.sleep <- file.path(
  "data", "2_INTERIM", "NONWEAR-SLEEP_PARQUET"
)
fdr_output.oxwearable <- file.path(
  "data", "2_INTERIM", "OUTPUT-OXWEARABLE_PARQUET"
)
fdr_output.cutpoint <- file.path(
  "data", "2_INTERIM", "OUTPUT-CUTPOINT_PARQUET"
)
fdr_output.raw <- file.path(
  "data", "2_INTERIM", "OUTPUT-RAW_PARQUET"
)
fdr_output.oak.pre <- file.path(
  "data", "2_INTERIM", "OUTPUT-OAK-PRE_PARQUET"
)
fdr_stepcount <- file.path(
  "data", "stepcount"
)
fdr_walmsley <- file.path(
  "data", "walmsley"
)
fdr_actinet <- file.path(
  "data", "actinet"
)
fdr_merged <- file.path(
  "data", "3_MERGED"
)
fdr_reports <- file.path(
  "reports"
)
# fdr_merged_
fs::dir_create(c(
  fdr_logs,
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
      path = "logs/",
      seconds_interval = 1
    ),
    options_local = crew_options_local(
      log_directory = "logs/"
    )
  )
  # trust_timestamps = TRUE
)


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
    path = file.path("logs", "main_process.log"), # Statistics on the main process go here.
    seconds = 1
  )
}

tar_plan(
  lst_yaml = parse_waves_yaml(),
  tar_file_read(
    name    = lst_miniconda,
    command = file.path("_targets_config", "objects", "lst_miniconda"),
    read    = qs2::qs_read(!!.x),
    format  = "qs"
  ),
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
  vct_raw_type = config_raw_type(vct_raw),
  tar_files(
    name    = vct_basic,
    command = wrapper_GGIR(vct_raw,
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
      log_prefix    = "main_",
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
      log_prefix    = "main_",
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
      log_prefix    = "main_",
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
  tar_target(
    name    = vct_merge,
    command = merge_output(
      vct_nw.sleep    = vct_nw.sleep,
      vct_out.raw     = vct_out.raw,
      vct_out.oak.pre = vct_out.oak.pre,
      vct_out.cut     = vct_out.cut,
      vct_ox          = vct_ox,
      dir_merged      = dir_merged,
      df_start_tz     = df_start_tz
    ),
    format = "file"
  ),
  if (reticulate:::is_linux()) {
    tar_render(
      name = pipeline_summary,
      path = "quarto/pipeline_main.qmd",
      output_file = file.path(getwd(), fdr_reports, "summary_pipeline_main.html")
    )
  } else {
    tar_quarto(
      name = pipeline_summary,
      path = "quarto/pipeline_main.qmd",
      output_file = file.path(getwd(), fdr_reports, "summary_pipeline_main.html")
    )
  }
)
