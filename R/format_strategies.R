#' @title Start-time / timezone extraction strategies for custom CSV inputs.
#' @description Each strategy is a function(fpa, my_tz, ...) that returns a
#'  list with fields `start_dttm`, `offset`, `tz`. `...` receives extra
#'  fields from `format$start_extraction_args` in config.yml.
lst_start_strategies <- list(

  first_data_row = function(fpa, my_tz, time_col = 1, time_format = NULL, ...) {
    first <- data.table::fread(fpa, nrows = 1, header = TRUE)
    raw <- first[[time_col]]
    if (!is.null(time_format)) {
      dttm <- as.POSIXct(strptime(raw, format = time_format, tz = "UTC"))
    } else {
      dttm <- as.POSIXct(raw, tz = "UTC")
    }
    list(
      start_dttm = dttm,
      offset     = find_offset(my_tz),
      tz         = my_tz
    )
  },

  header_field = function(fpa, my_tz, field, time_format = "%Y-%m-%d %H:%M:%OS", ...) {
    I <- GGIR::g.inspectfile(
      datafile = fpa,
      params_rawdata =
        GGIR::extract_params(params2check = "rawdata")[["params_rawdata"]]
    )
    raw <- I$header[field, "value"]
    list(
      start_dttm = strptime(raw, format = time_format, tz = "UTC"),
      offset     = find_offset(my_tz),
      tz         = my_tz
    )
  }
)


#' @title Load and validate config.yml.
#' @description Reads `config.yml` from the project root and runs schema
#'  checks. Errors with a copy-from-example hint if missing.
load_pipeline_config <- function(path = "config.yml",
                                 example_path = "config.example.yml") {
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "Pipeline config file not found at {.path {path}}.",
      "i" = "Copy {.path {example_path}} to {.path {path}} and edit for your dataset."
    ))
  }
  cfg <- yaml::read_yaml(path)
  validate_pipeline_config(cfg)
  cfg
}


#' @title Validate config.yml schema.
validate_pipeline_config <- function(cfg) {

  require_keys <- function(obj, keys, where) {
    missing <- setdiff(keys, names(obj))
    if (length(missing)) {
      cli::cli_abort(c(
        "Missing required key{?s} in {.field {where}}: {.val {missing}}",
        "i" = "See config.example.yml for the expected schema."
      ))
    }
  }

  require_keys(cfg, c("study", "data", "format"), "config.yml")
  require_keys(cfg$study, c("name", "timezone", "sampling_frequency"), "study")
  require_keys(cfg$data, c("raw_path", "raw_pattern"), "data")
  require_keys(cfg$format, c("type"), "format")

  valid_types <- c("binary", "custom_csv")
  if (!cfg$format$type %in% valid_types) {
    cli::cli_abort(c(
      "Invalid {.field format.type}: {.val {cfg$format$type}}",
      "i" = "Must be one of: {.val {valid_types}}"
    ))
  }

  if (cfg$format$type == "custom_csv") {
    require_keys(cfg$format,
                 c("label", "csv_spec", "start_extraction"),
                 "format (custom_csv)")
    require_keys(cfg$format$csv_spec,
                 c("col_acc", "col_time", "unit_acc", "unit_time",
                   "format_time", "sf"),
                 "format.csv_spec")

    strat <- cfg$format$start_extraction
    if (!strat %in% names(lst_start_strategies)) {
      cli::cli_abort(c(
        "Unknown {.field format.start_extraction}: {.val {strat}}",
        "i" = "Available: {.val {names(lst_start_strategies)}}"
      ))
    }
  }

  invisible(NULL)
}


#' @title Convert snake_case csv_spec to rmc.* args for GGIR.
#' @description Drops NULL values and restricts to GGIR's rawdata
#'  whitelist. Pass `target_fn` to further restrict to a specific
#'  function's formals (e.g. read.myacc.csv accepts a subset).
build_rmc_args <- function(csv_spec, target_fn = NULL) {
  raw <- list(
    rmc.col.acc                = csv_spec$col_acc,
    rmc.col.temp               = csv_spec$col_temp,
    rmc.col.time               = csv_spec$col_time,
    rmc.unit.acc               = csv_spec$unit_acc,
    rmc.unit.temp              = csv_spec$unit_temp,
    rmc.unit.time              = csv_spec$unit_time,
    rmc.format.time            = csv_spec$format_time,
    rmc.sf                     = csv_spec$sf,
    rmc.firstrow.acc           = csv_spec$firstrow_acc,
    rmc.firstrow.header        = csv_spec$firstrow_header,
    rmc.header.length          = csv_spec$header_length,
    rmc.headername.sf          = csv_spec$headername_sf,
    rmc.headername.sn          = csv_spec$headername_sn,
    rmc.headername.recordingid = csv_spec$headername_recordingid,
    rmc.dec                    = csv_spec$dec,
    rmc.bitrate                = csv_spec$bitrate,
    rmc.dynamic_range          = csv_spec$dynamic_range,
    rmc.noise                  = csv_spec$noise,
    rmc.col.wear               = csv_spec$col_wear,
    rmc.doresample             = csv_spec$doresample,
    rmc.scalefactor.acc        = csv_spec$scalefactor_acc,
    rmc.unsignedbit            = csv_spec$unsignedbit,
    rmc.check4timegaps         = csv_spec$check4timegaps
  )
  raw <- raw[!vapply(raw, is.null, logical(1))]

  ggir_keys <- grep(
    "^rmc",
    names(GGIR::extract_params(params2check = "rawdata")$params_rawdata),
    value = TRUE
  )
  raw <- raw[names(raw) %in% ggir_keys]

  if (!is.null(target_fn)) {
    fn_formals <- names(formals(target_fn))
    if (!"..." %in% fn_formals) {
      raw <- raw[names(raw) %in% fn_formals]
    }
  }
  raw
}


#' @title Run the configured start_extraction strategy for one file.
run_start_strategy <- function(cfg, fpa, my_tz) {
  fn   <- lst_start_strategies[[cfg$format$start_extraction]]
  args <- c(list(fpa = fpa, my_tz = my_tz),
            cfg$format$start_extraction_args %||% list())
  do.call(fn, args)
}


`%||%` <- function(a, b) if (is.null(a)) b else a
