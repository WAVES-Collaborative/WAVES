# TODO
# - Have a function to convert criterion data into a parquet for sending to JM?
#   My idea is to have site users provide a file path to the csv that holds the
#   DO/activPAL/WearableCamera/whatever criterion measures for ALL visit/field
#   data where the function just simply converts it to parquet file format to
#   send to JM, along with "WAVES_OUTPUT-[VISIT/FIELD].parquet" and Python output
#
# - Control/checks if field and visit data have same filenames?
#   The whole reason why there is visit and field distinctions is due to the FLAC
#   UWM dataset (Strath) has both DO "visit" data and 7-day "field" data where
#   participants where also asked to wear activpal. Visit and 7day data are
#   named distinctly from each other, and will not overlap. If another site that
#   contributes to WAVES also have visit and field data, the filenames should
#   also be different...hopefully.
#
# - Create "user-facing" readme with the following:
# 1. Install R
# 2. Install RStudio
# 3. Install RTools
# 4. Download WAVES repository.
# 5. Open WAVES.RProj
# 6. In Console, run renv::restore()
# 7. In console, type `tar_make()` and make sure pipeline works against test
#    data (TODO).
# 8. Put in directory paths to raw accelerometer data within "INPUT" section.
# 9. Change study_sampling_frequency and my_tz as needed.
# 10. Select all lines within "INPUT" section, then Ctrl+Enter.
# 11. In console, type `tar_make()` and press Enter.
# 12. Share `WAVES_OUTPUT` file(s) with WAVES working group, criterion
#     data, and demographic data
#
# - Work on making a WAVES website using pkgdown? Doesn't have to be fancy but
#   I can see it being really helpful than a readme, or making a private youtube
#   video?
# - Create "technical-facing" readme
#
# - Have site provide a key for breaking down ID?
#   The ID for now is the filename of the raw accelerometer data. FLAC UWM data
#   follows a naming scheme of "[STUDY]_[MONITOR]_[LOCATION]_[SUBJECT]_[VISIT]RAW"
#   where I think it would be nice to identify all data from a site with at least
#   [STUDY] and [SUBJECT]. Maybe ask for a key to get this from ID/filenames? Or
#   just ask in an email?

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
study_timezone <-
  Sys.timezone()
vct_raw_fpa_visit <- list.files(
  path = file.path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "4_Research",
    "2017 Strath R01 FLAC", "Data", "ActiGraph_GT3X", c("V1", "V2", "V3"),
    "LW", "LW_RAW"
  ),
  pattern    = ".*",
  full.names = TRUE,
  recursive  = TRUE
)
vct_raw_fpa_field <- list.files(
  path = file.path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "4_Research",
    "2017 Strath R01 FLAC", "Data", "ActiGraph_GT3X", "7 Day",
    "LW", "LW_RAW"
  ),
  pattern    = ".*",
  full.names = TRUE,
  recursive  = TRUE
)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
#                                      LOAD                                  ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load packages required to define the pipeline:
pkgs <- c(
  # "adept",
  # "adeptdata",
  "arrow",
  "C50",
  "crew",
  "data.table",
  "dplyr",
  "GGIR",
  "kernlab",
  "lubridate",
  "nnet",
  "qs2",
  "randomForest",
  "reticulate",
  "stringi",
  "tarchetypes",
  "targets",
  "tidyr",
  "tzdb",
  # sydney --
  "HMM",
  "rattle",
  #"GENEAread",
  "signal",
  "tools"
)
# Interactive use
# for (package in pkgs) library(package, character.only = TRUE)

# Define directories
fdr_calibrated <- file.path(
  "data", "2_INTERIM", "RAW-CALIBRATED_QS2"
)
fdr_output.cutpoint <- file.path(
  "data", "2_INTERIM", "OUTPUT-CUTPOINT_PARQUET"
)
fdr_output.raw <- file.path(
  "data", "2_INTERIM", "OUTPUT-RAW_PARQUET"
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
# fdr_merged_
fs::dir_create(c(
  fdr_calibrated,
  fdr_output.cutpoint,
  fdr_output.raw,
  fdr_stepcount,
  fdr_walmsley,
  fdr_actinet,
  fdr_merged
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
  packages   = pkgs,
  format     = "qs",
  controller = crew_controller_local(workers = 2)
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
tar_plan(
  my_tz = study_timezone,
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                             FILE DIRECTORIES                           ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  dir_models  = "models",
  dir_cal     = fdr_calibrated,
  dir_out.cut = fdr_output.cutpoint,
  dir_out.raw = fdr_output.raw,
  dir_stepcount = fdr_stepcount,
  dir_walmsley  = fdr_walmsley,
  dir_actinet   = fdr_actinet,
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                                FILE PATHS                              ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_files_input(
    name  = vct_raw,
    files = vct_raw_fpa_visit
    # files = vct_raw_fpa_visit[3] # For testing on one file
    # files = vct_raw_fpa_field
    # files = vct_raw_fpa_field[3] # For testing on one file
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                               DEMOGRAPHICS                             ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                           PROCESS - OXWEARABLES                        ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_target(
    name    = vct_ox_step,
    command = apply_ox_stepcount(
      vct_raw = vct_raw,
      fdr_write = file.path(getwd(), dir_stepcount)
    ),
    format = "file"
  ),
  tar_target(
    name    = vct_ox_wlms,
    command = apply_ox_walmsley(
      vct_raw = vct_raw,
      fdr_write = file.path(getwd(), dir_walmsley),
      my_tz = my_tz
    ),
    format = "file"
  ),
  tar_target(
    name    = vct_ox_acti,
    command = apply_ox_stepcount(
      vct_raw = vct_raw,
      fdr_write = file.path(getwd(), dir_actinet)
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
  ##                              PROCESS - GGIR                            ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_files(
    name    = vct_basic,
    command = wrapper_GGIR(vct_raw)
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
  ##                             PROCESS - CUSTOM                           ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_qs(
    name      = vct_cal,
    command   = read_acc_raw(
      fpa_read      = vct_raw,
      vct_fpa_basic = vct_basic,
      dir_cal       = dir_cal,
      my_tz         = my_tz
    ),
    pattern   = map(vct_raw),
    iteration = "vector"
  ),
  tar_qs(
    name      = lst_out.raw,
    command   = apply_methods_raw(
      fpa_read      = vct_cal,
      vct_fpa_basic = vct_basic,
      dir_models    = dir_models,
      dir_write     = dir_out.raw,
      my_tz         = my_tz
    ),
    pattern   = map(vct_cal_visit),
    iteration = "list"
  ),
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                                   MERGE                                ----
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tar_target(
    name    = fpa_merged,
    command = merge_output(
      lst_out.raw = lst_out.raw,
      lst_out.cut = lst_out.cut,
      dir_merged  = dir_merged,
      le_type     = "visit"
      # le_type     = "field"
    ),
    format = "file"
  )
)
