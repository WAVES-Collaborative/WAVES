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

study_timezone     <- Sys.timezone()
sampling_frequency <- 100
n_workers          <- 2 # future::availableCores() - 1

vct_raw_fpa <- file.path(
  "data", "0_CONFIG", "RAW",
  c(
    "WAVES_10002_RAW.csv.gz",
    "WAVES_10003_RAW.csv.gz",
    "WAVES_10004_RAW.gt3x",
    "WAVES_10005_RAW.cwa",
    "WAVES_10006_RAW.csv.gz"
  )
)

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

# Decompress config files.
chk_10004 <- !file.exists(vct_raw_fpa[3])
chk_10005 <- !file.exists(vct_raw_fpa[4])

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
    path = file.path("logs", "config_process.log"), # Statistics on the main process go here.
    seconds = 1
  )
}

tar_plan(
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             MINICONDA SETUP                            ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_file_read(
    name    = df_pkgs_stepcount,
    command = file.path("data", "0_CONFIG", "RAW", "df_pkgs_stepcount.csv"),
    read    = fread(!!.x, sep = ","),
    format  = "parquet"
  ),
  tar_file_read(
    name    = df_pkgs_walmsley,
    command = file.path("data", "0_CONFIG", "RAW", "df_pkgs_walmsley.csv"),
    read    =
      fread(!!.x, sep = ",") |>
      mutate(
        `Version WAVES` = case_when(
          Package %in% c("packaging", "symlink-exe-runtime") ~ paste0(`Version WAVES`, ".0"),
          .default = `Version WAVES`
        )
      ),
    format  = "parquet"
  ),
  tar_file_read(
    name    = df_pkgs_actinet,
    command = file.path("data", "0_CONFIG", "RAW", "df_pkgs_actinet.csv"),
    read    =
      fread(!!.x, sep = ",") |>
      mutate(
        `Version WAVES` = case_when(
          Package %in% c("packaging", "symlink-exe-runtime") ~ paste0(`Version WAVES`, ".0"),
          .default = `Version WAVES`
        )
      ),
    format  = "parquet"
  ),
  tar_file_read(
    name    = df_pkgs_oak_1.0,
    command = file.path("data", "0_CONFIG", "RAW", "df_pkgs_oak1.0.csv"),
    read    =
      fread(!!.x, sep = ",") |>
      mutate(
        `Version WAVES` = case_when(
          Package %in% c("packaging", "symlink-exe-runtime") ~ paste0(`Version WAVES`, ".0"),
          .default = `Version WAVES`
        )
      ),
    format  = "parquet"
  ),
  tar_file_read(
    name    = df_pkgs_oak_pre,
    command = file.path("data", "0_CONFIG", "RAW", "df_pkgs_oakpre.csv"),
    read    =
      fread(!!.x, sep = ",") |>
      mutate(
        `Version WAVES` = case_when(
          Package %in% c("packaging", "symlink-exe-runtime") ~ paste0(`Version WAVES`, ".0"),
          .default = `Version WAVES`
        )
      ),
    format  = "parquet"
  ),
  tar_qs(
    name = lst_miniconda,
    command = config_miniconda(
      df_pkgs_stepcount,
      df_pkgs_walmsley,
      df_pkgs_actinet,
      df_pkgs_oak_1.0,
      df_pkgs_oak_pre
    ),
    cue = tar_cue(mode = "always")
  ),
  tar_render(
    name = minconda_summary,
    path = "quarto/config_miniconda.qmd",
    output_file = file.path(getwd(), fdr_reports, "summary_miniconda.html")
  ),
  my_tz = study_timezone,
  my_sf = sampling_frequency,
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             FILE DIRECTORIES                           ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  dir_models      = "models",
  dir_logs        = fdr_logs,
  dir_cal         = fdr_calibrated,
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
  tar_files_input(
    name  = vct_raw,
    files = vct_raw_fpa
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
  tar_qs(
    name      = lst_out.cut,
    command   = apply_methods_cutpoints(
      fpa_basic = vct_basic,
      dir_write = dir_out.cut,
      my_tz     = my_tz
    ),
    pattern   = map(vct_basic),
    iteration = "list"
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                           PROCESS - OXWEARABLES                        ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  vct_ox_input = prepare_ox_input(
    vct_raw,
    vct_raw_type,
    vct_basic
  ),
  tar_target(
    name    = vct_ox_step,
    command = apply_ox_stepcount(
      vct_ox_input = vct_ox_input,
      fdr_write    = file.path(getwd(), dir_stepcount),
      fdr_log      = dir_logs,
      log_prefix   = "config_",
      lst_miniconda = lst_miniconda
    ),
    format = "file"
  ),
  tar_target(
    name    = vct_ox_wlms,
    command = apply_ox_walmsley(
      vct_ox_input = vct_ox_input,
      fdr_write    = file.path(getwd(), dir_walmsley),
      fdr_log      = dir_logs,
      my_tz        = my_tz,
      log_prefix   = "config_",
      lst_miniconda = lst_miniconda
    ),
    format = "file"
  ),
  tar_target(
    name    = vct_ox_acti,
    command = apply_ox_actinet(
      vct_ox_input = vct_ox_input,
      fdr_write    = file.path(getwd(), dir_actinet),
      fdr_log      = dir_logs,
      log_prefix   = "config_",
      lst_miniconda = lst_miniconda
    ),
    format = "file"
  ),
  lst_ox = merge_ox(
    vct_ox_step,
    vct_ox_wlms,
    vct_ox_acti,
    my_tz
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             PROCESS - CUSTOM                           ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_qs(
    name      = vct_cal,
    command   = read_acc_raw(
      fpa_read      = vct_raw,
      le_type       = vct_raw_type,
      vct_fpa_basic = vct_basic,
      dir_cal       = dir_cal,
      my_tz         = my_tz
    ),
    pattern   = map(vct_raw, vct_raw_type),
    iteration = "vector"
  ),
  tar_qs(
    name      = lst_out.raw,
    command   = apply_methods_raw(
      fpa_read      = vct_cal,
      vct_fpa_basic = vct_basic,
      dir_models    = dir_models,
      dir_write     = dir_out.raw,
      my_tz         = my_tz,
      lst_miniconda = lst_miniconda
    ),
    pattern   = map(vct_cal),
    iteration = "list",
    error = "null"
  ),
  tar_qs(
    name      = lst_out.oak.pre,
    command   = apply_oak.pre(
      fpa_read      = vct_cal,
      vct_fpa_basic = vct_basic,
      dir_write     = dir_out.oak.pre,
      my_tz         = my_tz,
      lst_miniconda = lst_miniconda
    ),
    pattern   = map(vct_cal),
    iteration = "list",
    error = "null",
    deployment = "main" # in order to avoid error of loading two conda environments within the same R session
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                                   MERGE                                ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_target(
    name    = fpa_merged,
    command = merge_output_config(
      lst_out.raw     = lst_out.raw,
      lst_out.oak.pre = lst_out.oak.pre,
      lst_out.cut     = lst_out.cut,
      lst_ox          = lst_ox,
      dir_merged      = dir_merged,
      my_tz           = my_tz
    ),
    format = "file"
  ),
  tar_render(
    name = pipeline_summary,
    path = "quarto/pipeline_config.qmd",
    output_file = file.path(getwd(), fdr_reports, "summary_pipeline_config.html")
  )
)
