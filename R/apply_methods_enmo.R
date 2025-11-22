# fpa_basic = tar_read(vct_basic)[1]
# dir_write = tar_read(dir_out.cut)
# my_tz    = tar_read(my_tz)
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
  df_cutpoint <-
    M$metashort |>
    mutate(
      id = fnm_sans_ext,
      datetime = ymd_hms(timestamp,
                         tz = my_tz),
      intensity_bakrania.enmo.simple = switch(
        I$monn,
        "actigraph" = {cut(
          ENMO * 1000,
          breaks = c(-Inf, 25.8, Inf),
          labels = c("sedentary", "light")
        )},
        "geneactive" = {cut(
          ENMO * 1000,
          breaks = c(-Inf, 30.7, Inf),
          labels = c("sedentary", "light")
        )}
      ),
      intensity_bakrania.enmo.average = switch(
        I$monn,
        "actigraph" = {cut(
          ENMO * 1000,
          breaks = c(-Inf, 26.85, Inf),
          labels = c("sedentary", "light")
        )},
        "geneactive" = {cut(
          ENMO * 1000,
          breaks = c(-Inf, 32.55, Inf),
          labels = c("sedentary", "light")
        )}
      ),
      intensity_hildebrand = cut(
        ENMO * 1000,
        breaks = c(-Inf, 44.8, 100.6, # 428.8,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_mielke = cut(
        ENMO * 1000,
        breaks = c(-Inf, 25.0, 78.0, # 249.0,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_white.enmo.lin = cut(
        ENMO * 1000,
        breaks = c(-Inf, 30.6, 137.4, # 351.1,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_white.enmo.pol = cut(
        ENMO * 1000,
        breaks = c(-Inf, 27.8, 115.7, # 341.2,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_esliger = cut(
        ENMOa * 1000,
        breaks = c(-Inf, 45.0, 134.0, # 377.0,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_fraysee = cut(
        ENMOa * 1000,
        breaks = c(-Inf, 42.5, 98.0, Inf),
        labels = c("sedentary", "light", "mvpa")
      ),
      intensity_white.hpfvm.lin = cut(
        HFEN * 1000,
        breaks = c(-Inf, 47.1, 172.3, # 422.6,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_white.hpfvm.pol = cut(
        HFEN * 1000,
        breaks = c(-Inf, 48.1, 163.3, # 421.8,
                   Inf),
        labels = c("sedentary", "light", # "moderate", "vigorous",
                   "mvpa")
      ),
      intensity_bakrania.mad.simple = switch(
        I$monn,
        "actigraph" = {cut(
          MAD * 1000,
          breaks = c(-Inf, 33.4, Inf),
          labels = c("sedentary", "light")
        )},
        "geneactive" = {cut(
          MAD * 1000,
          breaks = c(-Inf, 39.6, Inf),
          labels = c("sedentary", "light")
        )}
      ),
      intensity_bakrania.mad.average = switch(
        I$monn,
        "actigraph" = {cut(
          MAD * 1000,
          breaks = c(-Inf, 34.65, Inf),
          labels = c("sedentary", "light")
        )},
        "geneactive" = {cut(
          MAD * 1000,
          breaks = c(-Inf, 42.4, Inf),
          labels = c("sedentary", "light")
        )}
      ),
      .keep = "none"
    )

  # Return ----
  fpa_write <- file.path(dir_write, paste0(fnm_sans_ext, ".parquet"))
  arrow::write_parquet(df_cutpoint, sink = fpa_write)
  return(df_cutpoint)

}
