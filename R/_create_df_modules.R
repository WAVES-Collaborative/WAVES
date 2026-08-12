library(targets)
library(tarchetypes)
library(crew)
library(autometric)
Sys.setenv(
  TAR_PROJECT = "config",
  # change below environment variable if you already have a conda installation readily accessible.
  RETICULATE_MINICONDA_PATH = file.path(
    getwd() |>
      dirname(),
    "conda",
    "r-miniconda"
  )
)
source("packages.R") |>
  suppressMessages() |>
  suppressWarnings()

# Code for getting module WAVES version
reticulate:::conda_list_packages("WAVES_stepcount") |>
  mutate(
    Module = package,
    `Version WAVES` = version,
    Channel = channel,
    .keep = "none"
  ) |>
  fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_stepcount.csv"))
reticulate:::conda_list_packages("WAVES_accelerometer") |>
  mutate(
    Module = package,
    `Version WAVES` = version,
    Channel = channel,
    .keep = "none"
  ) |>
  fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_walmsley.csv"))
reticulate:::conda_list_packages("WAVES_actinet") |>
  mutate(
    Module = package,
    `Version WAVES` = version,
    Channel = channel,
    .keep = "none"
  ) |>
  fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_actinet.csv"))
reticulate:::conda_list_packages("WAVES_oak_1.0") |>
  mutate(
    Module = package,
    `Version WAVES` = version,
    Channel = channel,
    .keep = "none"
  ) |>
  fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_oak1.0.csv"))
reticulate:::conda_list_packages("WAVES_oak_pre") |>
  mutate(
    Module = package,
    `Version WAVES` = version,
    Channel = channel,
    .keep = "none"
  ) |>
  fwrite(file = file.path("data", "0_CONFIG", "RAW", "df_modules_oakpre.csv"))
