library(reticulate)

# Create conda environments and install OxWearable modules.
install_miniconda()
conda_create(
  envname = "WHO_WAVES_stepcount",
  packages = "openjdk",
  forge = TRUE,
  python_version = 3.9,
  pip = TRUE
)
conda_create(
  envname = "WHO_WAVES_actinet",
  packages = "openjdk",
  forge = TRUE,
  python_version = 3.9,
  pip = TRUE
)
conda_create(
  envname = "WHO_WAVES_accelerometer",
  packages = "openjdk",
  forge = TRUE,
  python_version = 3.9,
  pip = TRUE
)
conda_install(
  envname  = "WHO_WAVES_stepcount",
  packages = "stepcount==3.5",
  forge    = FALSE,
  pip      = TRUE
)
conda_install(
  envname  = "WHO_WAVES_actinet",
  packages = "actinet==0.4.2",
  forge    = FALSE,
  pip      = TRUE
)
conda_install(
  envname  = "WHO_WAVES_accelerometer",
  packages = "accelerometer==7.3.0",
  forge    = FALSE,
  pip      = TRUE
)
