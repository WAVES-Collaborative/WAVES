#' @title Apply cutpoints.
#'
#' @description
#' Applies 13 cutpoints. Most follow the GGIR article on cutpoints
#' titled \href(https://wadpac.github.io/GGIR/articles/CutPoints.html){Published cut-points and how to use them in GGIR}.
#'
#' @section How each cutpoint was extracted/implemented:
#'
#' All cutpoints are for the non-dominant wrist. Brand specific cutpoints were used when possible, with the
#' GENEActiv cutpoint used if the supplied data comes from a non-specific brand (i.e. user supplies Axivity
#' data, the Hildebrand GENEActiv cutpoint will be used).
#'
#' Euclidean Norm Minus One (ENMO) methods:
#'
#' * bakrania.enmo.simple: Table 5 contains wrist cutpoints for GT3X+ and GENEactiv
#' monitors, where the cutpoint for the "washing pots" activity was used to
#' separate sedentary and light intensity.
#'
#' * bakrania.enmo.average: Table 5, averages the cutpoints for washing pots
#' and dusting, as done in \href(https://doi.org/10.1123/jmpb.2024-0051){Matthews et al., 2025}
#'
#' * hildebrand: Sedentary threshold cutpoint from Table 3 of Hildebrand et al., 2017,
#' moderate intensity threshold cutpoint from Table 4 of Hildebrand et al., 2014.
#'
#' * mielke: Table 2
#'
#' * white.enmo.lin: Table S1, in using derived regression model 1, which estimates
#' PAEE in J/min/kg from ENMO, we solved for ENMO when substituting PAEE to
#' 35.6125 J/min/kg (0.5 marginal METs due to PA, 1.5 gross METs) and 142.45 J/min/kg
#' (2 marginal METs due to PA, 3 gross METs) to get sedentary threshold and light
#' threshold cutpoints.
#'
#' $$PAEE = 5.01 + 1.000(ENMO)$$
#'
#' $$35.6125 = 5.01 + 1.000(ENMO)$$
#'
#' $$142.45 = 5.01 + 1.000(ENMO)$$
#'
#' * white.enmo.pol: Table S2, uses the same procedure for model 1
#'
#' $$PAEE = −10.58 + 1.1176(ENMO) + 2.9418(\sq{ENMO}) − 0.00059277(ENMO^2)$$
#'
#' $$35.6125 = -10.58 + 1.1176(ENMO) + 2.9418(\sq{ENMO}) − 0.00059277(ENMO^2)$$
#'
#' $$142.45 = -10.58 + 1.1176(ENMO) + 2.9418(\sq{ENMO}) − 0.00059277(ENMO^2)$$
#'
#' ENMOa methods:
#'
#' * esliger: GGIR scaled
#'
#' * fraysee: TODO
#'
#' High-Pass Filtered Vector Magnitude (HPFVM) methods:
#'
#' * white.hpfvm.lin: TODO
#'
#' * white.hpfvm.pol: TODO
#'
#' MAD cutpoints:
#'
#' * bakrania.mad.simple: Table 7 contains MAD wrist cutpoints for GT3X+ and GENEActiv
#' monitors, where the cutpoint for the "washing pots" activity was used to
#' separate sedentary and light intensity.
#'
#' * bakrania.mad.average: Table 7, averages the cutpoints for washing pots
#' and dusting, similar to the ENMO method described in \href(https://doi.org/10.1123/jmpb.2024-0051){Matthews et al., 2025}
#'
#' @section References:
#'
#' ENMO methods:
#'
#' * \href(https://doi.org/10.1371/journal.pone.0164045){Bakrania et al., 2016}
#'
#' * \href(https://doi.org/10.1249/mss.0000000000000289){Hildebrand et al., 2014 (mvpa cutpoint)}
#'
#' * \href(https://doi.org/10.1111/sms.12795){Hildebrand et al., 2017 (sedentary cutpoint)}
#'
#' * \href(https://doi.org/10.1111/sms.14416){Mielke et al., 2023}
#'
#' * \href(https://doi.org){}
#'
#' * \href(https://doi.org){}
#'
#' * \href(https://doi.org){}
#'
#' * \href(https://doi.org){}
#'
#' * \href(https://doi.org){}
#'
#' * \href(https://doi.org){}
#'
#' * \href(https://doi.org){}
#'
#' @param fpa_basic
#' @param dir_write
#' @param my_tz
#'
#' @returns
#' @export
#'
#' @examples
apply_methods_cutpoints <- function(fpa_basic,
                                    dir_write,
                                    my_tz) {

  if (is.null(fpa_basic)) return(NULL)

  load(fpa_basic)
  fnm_sans_ext <-
    fpa_basic |>
    basename() |>
    tools::file_path_sans_ext() |>
    tools::file_path_sans_ext() |>
    sub(x = _,
        pattern = "meta_",
        replacement = "")
  brand <- case_when(
    I$monn != c("actigraph") ~ "other",
    .default = I$monn
  )
  df_cutpoint <-
    M$metashort |>
    mutate(
      datetime = ymd_hms(timestamp,
                         tz = my_tz),
      # To millig
      ENMO = ENMO * 1000,
      ENMOa = ENMOa * 1000,
      HFEN = HFEN * 1000,
      MAD = MAD * 1000,
      # ENMO cutpoints
      intensity_bakrania.enmo.simple = switch(
        brand,
        "actigraph" = {cut(
          ENMO,
          breaks = c(-Inf, 25.8, Inf),
          labels = c("sedentary", "light")
        )},
        "other" = {cut(
          ENMO,
          breaks = c(-Inf, 30.7, Inf),
          labels = c("sedentary", "light")
        )}
      ),
      intensity_bakrania.enmo.average = switch(
        brand,
        "actigraph" = {cut(
          ENMO,
          breaks = c(-Inf, 26.85, Inf),
          labels = c("sedentary", "light")
        )},
        "other" = {cut(
          ENMO,
          breaks = c(-Inf, 32.55, Inf),
          labels = c("sedentary", "light")
        )}
      ),
      intensity_hildebrand = switch(
        brand,
        "actigraph" = {cut(
          ENMO,
          breaks = c(-Inf, 44.8, 100.6, # 428.8,
                     Inf),
          labels = c("sedentary", "light", # "moderate", "vigorous",
                     "mvpa")
        )},
        "other" = {cut(
          ENMO,
          breaks = c(-Inf, 45.8, 93.2, # 418.3,
                     Inf),
          labels = c("sedentary", "light", # "moderate", "vigorous",
                     "mvpa")
        )}
      ),
      intensity_mielke = switch(
        brand,
        "actigraph" = {cut(
          ENMO,
          breaks = c(-Inf, 25.0, 78.0, # 249.0,
                     Inf),
          labels = c("sedentary", "light", # "moderate", "vigorous",
                     "mvpa")
        )},
        "other" = {cut(
          ENMO,
          breaks = c(-Inf, 36.0, 92.0, # 283.0,
                     Inf),
          labels = c("sedentary", "light", # "moderate", "vigorous",
                     "mvpa")
        )}
      ),
      intensity_white.enmo.lin = cut(
        ENMO,
        breaks = c(-Inf, 30.6, 137.4, # 351.1,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_white.enmo.pol = cut(
        ENMO,
        breaks = c(-Inf, 27.8, 115.7, # 341.2,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      # ENMOa cutpoints
      intensity_esliger = cut(
        ENMOa,
        breaks = c(-Inf, 45.0, 134.0, # 377.0,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_fraysee = cut(
        ENMOa,
        breaks = c(-Inf, 42.5, 98.0, Inf),
        labels = c("sedentary", "light", "mvpa")
      ),
      # HPFVM cutpoints
      intensity_white.hpfvm.lin = cut(
        HFEN,
        breaks = c(-Inf, 47.1, 172.3, # 422.6,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_white.hpfvm.pol = cut(
        HFEN,
        breaks = c(-Inf, 48.1, 163.3, # 421.8,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      # MAD cutpoints
      intensity_bakrania.mad.simple = switch(
        brand,
        "actigraph" = {cut(
          MAD,
          breaks = c(-Inf, 33.4, Inf),
          labels = c("sedentary", "light")
        )},
        "other" = {cut(
          MAD,
          breaks = c(-Inf, 39.6, Inf),
          labels = c("sedentary", "light")
        )}
      ),
      intensity_bakrania.mad.average = switch(
        brand,
        "actigraph" = {cut(
          MAD,
          breaks = c(-Inf, 34.65, Inf),
          labels = c("sedentary", "light")
        )},
        "other" = {cut(
          MAD,
          breaks = c(-Inf, 42.4, Inf),
          labels = c("sedentary", "light")
        )}
      )
    ) |>
    reframe(
      id = fnm_sans_ext,
      datetime = seq.POSIXt(
        from = datetime[1],
        to = last(datetime) + 4,
        by = "1 sec"
      ),
      across(
        .cols = starts_with("intensity"),
        .fns = ~rep(.x, each = 5)
      )
    )

  # Return ----
  fpa_write <- file.path(dir_write, paste0(fnm_sans_ext, ".parquet"))
  arrow::write_parquet(df_cutpoint, sink = fpa_write)
  return(df_cutpoint)

}
