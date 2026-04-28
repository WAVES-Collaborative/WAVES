summarize_major_steps <- function(vct_raw,
                                  vct_basic,
                                  vct_cal,
                                  lst_out.raw,
                                  lst_out.oak.pre,
                                  lst_out.cut,
                                  vct_ox_step,
                                  vct_ox_wlms,
                                  vct_ox_acti) {

  lst_out.raw[sapply(lst_out.raw, is.null)] <- NULL
  lst_out.oak.pre[sapply(lst_out.oak.pre, is.null)] <- NULL
  lst_out.cut[sapply(lst_out.cut, is.null)] <- NULL

  df <- tibble(
    file =
      basename(vct_raw),
    file_noext =
      file_path_sans_ext(file) |>
      file_path_sans_ext()
  )

  vct_nm_ggir <-
    basename(vct_basic) |>
    gsub(x = _,
         pattern = "meta_|\\.RData",
         replacement = "")
  vct_nm_cal <-
    basename(vct_cal) |>
    file_path_sans_ext() |>
    file_path_sans_ext()
  vct_nm_raw <-
    sapply(lst_out.raw, \(.x) .x$id[1]) |>
    file_path_sans_ext()
  vct_nm_oak.pre <-
    sapply(lst_out.oak.pre, \(.x) .x$id[1]) |>
    file_path_sans_ext()
  vct_nm_cut <-
    sapply(lst_out.cut, \(.x) .x$id[1]) |>
    file_path_sans_ext()
  vct_nm_stp <-
    basename(vct_ox_step) |>
    gsub(x = _,
         pattern = "-StepTimes\\.csv\\.gz",
         replacement = "")
  vct_nm_wlm <-
    basename(vct_ox_wlms) |>
    gsub(x = _,
         pattern = "-timeSeries\\.csv\\.gz",
         replacement = "")
  vct_nm_act <-
    basename(vct_ox_acti) |>
    gsub(x = _,
         pattern = "-timeSeries\\.csv\\.gz",
         replacement = "")

  df |>
    mutate(
    GGIR          = file %in% vct_nm_ggir,
    calibration   = file_noext %in% vct_nm_cal,
    `raw methods` = file_noext %in% vct_nm_raw,
    oak.pre       = file_noext %in% vct_nm_oak.pre,
    `cutpoints`   = file_noext %in% vct_nm_cut,
    stepcount     = file_noext %in% vct_nm_stp,
    walmsley      = file_noext %in% vct_nm_wlm,,
    actinet       = file_noext %in% vct_nm_act,,
    file_noext    = NULL
  )
}
make_table_agr.cor <- function(mtx_yours,
                               mtx_waves,
                               df_agr_waves) {

  df_agr <-
    mtx_yours |>
    as.data.frame() |>
    rownames_to_column(var = "method") |>
    pivot_longer(cols = !method,
                 names_to = "method2",
                 values_to = "yours",
                 values_drop_na = TRUE) |>
    dplyr::filter(method != method2)
  df_equal <-
    left_join(
      df_agr,
      df_agr_waves,
      by = join_by(method, method2)
    ) |>
    mutate(
      rowid = rleid(method) + 1,
      colid =
        factor(method2) |>
        as.integer(),
      diff = round(abs(yours - waves), digits = 2),
      clr = case_when(
        yours == waves ~ "#D9F1D5",
        diff <= 0.01 ~ "#FAFA8E",
        .default = "#ea9999"
      )
    )
  tbl_agr <-
    pmax(mtx_yours, mtx_waves, na.rm = TRUE) |>
    as.data.frame() |>
    rownames_to_column(var = "method") |>
    gt()
  ind_equal <- which(df_equal$clr == "#D9F1D5")
  ind_near <- which(df_equal$clr == "#FAFA8E")
  ind_unequal <- which(df_equal$clr == "#ea9999")

  for (i in seq_along(df_equal$clr)) {
    tbl_agr <-
      tbl_agr |>
      tab_style(
        style = cell_fill(color = df_equal$clr[i]),
        locations = cells_body(columns = df_equal$method2[i],
                               rows = df_equal$rowid[i])
      )
  }

  for (ind in ind_near) {
    tbl_agr <-
      tbl_agr |>
      tab_style(
        style = cell_fill(color = df_equal$clr[ind]),
        locations = cells_body(columns = df_equal$method[ind],
                               rows = df_equal$colid[ind])
      )
  }

  for (ind in ind_unequal) {
    tbl_agr <-
      tbl_agr |>
      tab_style(
        style = cell_fill(color = df_equal$clr[ind]),
        locations = cells_body(columns = df_equal$method[ind],
                               rows = df_equal$colid[ind])
      )
  }

  return(tbl_agr)

}
summarize_metrics_config <- function(fpa_merged) {

  load("data/0_CONFIG/MERGED/WAVES_ALL_TEST.RData")
  df_pipe <- read_parquet(fpa_merged)

  # total ----
  df_sed <-
    df_pipe |>
    select(id, starts_with("intensity")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "intensity_", replacement = "")) |>
    summarise(across(
      .cols = everything(),
      .fns = ~sum(.x == "sedentary", na.rm = TRUE) / 60
    ), .by = id)
  df_sed <- df_sed[
    , c(names(df_sed)[1], sort(names(df_sed[-1])))
  ]
  df_mvpa <-
    df_pipe |>
    select(id, starts_with("intensity")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "intensity_", replacement = "")) |>
    select(id, !starts_with("bakrania")) |>
    summarise(across(
      .cols = everything(),
      .fns = ~sum(.x == "mvpa", na.rm = TRUE) / 60
    ), .by = id)
  df_mvpa <- df_mvpa[
    , c(names(df_mvpa)[1], sort(names(df_mvpa[-1])))
  ]
  df_step <-
    df_pipe |>
    select(id, starts_with("steps")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "steps_", replacement = "")) |>
    summarise(across(
      .cols = everything(),
      .fns = ~sum(.x, na.rm = TRUE)
    ), .by = id)
  df_step <- df_step[
    , c(names(df_step)[1], sort(names(df_step[-1])))
  ]

  # Compare total ----
  ## sed ----
  df_sed[, -1] <- round(df_sed[, -1], digits = 1)
  df_test_sed[, -1] <- round(df_test_sed[, -1], digits = 1)

  # First see if they are equal to each other at the tenth level, then see if they
  # are near each other +/- 0.01.
  df_equal_sed <-
    (df_sed[, -1] == df_test_sed[, -1]) |>
    as.data.frame() |>
    mutate(id = df_sed$id, .before = 1) |>
    pivot_longer(cols = !id,
                 names_to = "method",
                 values_to = "equal") |>
    mutate(
      rowid = rleid(id),
      yours = unlist(transpose(df_sed[, -1])),
      waves = unlist(transpose(df_test_sed[, -1])),
      diff = round(abs(yours - waves), digits = 1),
      clr = case_when(
        equal ~ "#D9F1D5",
        diff <= 0.1 ~ "#FAFA8E",
        .default = "#ea9999"
      ),
      equal = NULL,
      id = NULL
    )
  tbl_sed <-
    left_join(
      df_sed |>
        rename_with(
          .cols = !id,
          .fn = ~paste0("Yours_", .x)
        ),
      df_test_sed |>
        rename_with(
          .cols = !id,
          .fn = ~paste0("WAVES_", .x)
        ),
      by = join_by(id)
    ) |>
    gt() |>
    tab_spanner(
      label = "Actinet", columns = ends_with("actinet")
    ) |>
    tab_spanner(
      label = "Bakrania (ENMO Average)", columns = ends_with("bakrania.enmo.average")
    ) |>
    tab_spanner(
      label = "Bakrania (ENMO Simple)", columns = ends_with("bakrania.enmo.simple")
    ) |>
    tab_spanner(
      label = "Bakrania (MAD Average)", columns = ends_with("bakrania.mad.average")
    ) |>
    tab_spanner(
      label = "Bakrania (MAD Simple)", columns = ends_with("bakrania.mad.simple")
    ) |>
    tab_spanner(
      label = "Ellis", columns = ends_with("ellis")
    ) |>
    tab_spanner(
      label = "Esliger", columns = ends_with("esliger")
    ) |>
    tab_spanner(
      label = "Fraysee", columns = ends_with("fraysee")
    ) |>
    tab_spanner(
      label = "Hildrebrand", columns = ends_with("hildebrand")
    ) |>
    tab_spanner(
      label = "Mielke", columns = ends_with("mielke")
    ) |>
    tab_spanner(
      label = "Montoye (DT)", columns = ends_with("montoye.dt")
    ) |>
    tab_spanner(
      label = "Montoye (NN)", columns = ends_with("montoye.nn")
    ) |>
    tab_spanner(
      label = "Montoye (RF)", columns = ends_with("montoye.rf")
    ) |>
    tab_spanner(
      label = "Montoye (SVM)", columns = ends_with("montoye.svm")
    ) |>
    tab_spanner(
      label = "Trost", columns = ends_with("trost")
    ) |>
    tab_spanner(
      label = "Walmsley", columns = ends_with("walmsley")
    ) |>
    tab_spanner(
      label = "White (ENMO Linear)", columns = ends_with("white.enmo.lin")
    ) |>
    tab_spanner(
      label = "White (ENMO Polynomial)", columns = ends_with("white.enmo.pol")
    ) |>
    tab_spanner(
      label = "White (HPFVM Linear)", columns = ends_with("white.hpfvm.lin")
    ) |>
    tab_spanner(
      label = "White (HPFVM Polynomial)", columns = ends_with("white.hpfvm.pol")
    )

  for (i in seq_len(nrow(df_equal_sed))) {
    tbl_sed <-
      tbl_sed |>
      tab_style(
        style = cell_fill(color = df_equal_sed$clr[i]),
        locations = cells_body(columns = ends_with(df_equal_sed$method[i]),
                               rows = df_equal_sed$rowid[i])
      )
  }

  tbl_sed <-
    tbl_sed |>
    cols_label_with(
      columns = !id,
      fn = ~sub(x = .x,
                pattern = "_.*",
                replacement = "")
    ) |>
    fmt_number(
      columns = !id,
      decimals = 1
    )

  ## mvpa ----
  df_mvpa[, -1] <- round(df_mvpa[, -1], digits = 1)
  df_test_mvpa[, -1] <- round(df_test_mvpa[, -1], digits = 1)
  df_equal_mvpa <-
    (df_mvpa[, -1] == df_test_mvpa[, -1]) |>
    as.data.frame() |>
    mutate(id = df_mvpa$id, .before = 1) |>
    pivot_longer(cols = !id,
                 names_to = "method",
                 values_to = "equal") |>
    mutate(
      rowid = rleid(id),
      yours = unlist(transpose(df_mvpa[, -1])),
      waves = unlist(transpose(df_test_mvpa[, -1])),
      diff = round(abs(yours - waves), digits = 1),
      clr = case_when(
        equal ~ "#D9F1D5",
        diff <= 0.1 ~ "#FAFA8E",
        .default = "#ea9999"
      ),
      equal = NULL,
      id = NULL
    )
  tbl_mvpa <-
    left_join(
      df_mvpa |>
        rename_with(
          .cols = !id,
          .fn = ~paste0("Yours_", .x)
        ),
      df_test_mvpa |>
        rename_with(
          .cols = !id,
          .fn = ~paste0("WAVES_", .x)
        ),
      by = join_by(id)
    ) |>
    gt() |>
    tab_spanner(
      label = "Actinet", columns = ends_with("actinet")
    ) |>
    tab_spanner(
      label = "Ellis", columns = ends_with("ellis")
    ) |>
    tab_spanner(
      label = "Esliger", columns = ends_with("esliger")
    ) |>
    tab_spanner(
      label = "Fraysee", columns = ends_with("fraysee")
    ) |>
    tab_spanner(
      label = "Hildrebrand", columns = ends_with("hildebrand")
    ) |>
    tab_spanner(
      label = "Mielke", columns = ends_with("mielke")
    ) |>
    tab_spanner(
      label = "Montoye (DT)", columns = ends_with("montoye.dt")
    ) |>
    tab_spanner(
      label = "Montoye (NN)", columns = ends_with("montoye.nn")
    ) |>
    tab_spanner(
      label = "Montoye (RF)", columns = ends_with("montoye.rf")
    ) |>
    tab_spanner(
      label = "Montoye (SVM)", columns = ends_with("montoye.svm")
    ) |>
    tab_spanner(
      label = "Trost", columns = ends_with("trost")
    ) |>
    tab_spanner(
      label = "Walmsley", columns = ends_with("walmsley")
    ) |>
    tab_spanner(
      label = "White (ENMO Linear)", columns = ends_with("white.enmo.lin")
    ) |>
    tab_spanner(
      label = "White (ENMO Polynomial)", columns = ends_with("white.enmo.pol")
    ) |>
    tab_spanner(
      label = "White (HPFVM Linear)", columns = ends_with("white.hpfvm.lin")
    ) |>
    tab_spanner(
      label = "White (HPFVM Polynomial)", columns = ends_with("white.hpfvm.pol")
    )

  for (i in seq_len(nrow(df_equal_mvpa))) {
    tbl_mvpa <-
      tbl_mvpa |>
      tab_style(
        style = cell_fill(color = df_equal_mvpa$clr[i]),
        locations = cells_body(columns = ends_with(df_equal_mvpa$method[i]),
                               rows = df_equal_mvpa$rowid[i])
      )
  }

  tbl_mvpa <-
    tbl_mvpa |>
    cols_label_with(
      columns = !id,
      fn = ~sub(x = .x,
                pattern = "_.*",
                replacement = "")
    ) |>
    fmt_number(
      columns = !id,
      decimals = 1
    )

  ## steps ----
  df_step[, -1] <- trunc(df_step[, -1])
  df_test_step[, -1] <- trunc(df_test_step[, -1])
  df_equal_step <-
    (df_step[, -1] == df_test_step[, -1]) |>
    as.data.frame() |>
    mutate(id = df_step$id, .before = 1) |>
    pivot_longer(cols = !id,
                 names_to = "method",
                 values_to = "equal") |>
    mutate(
      rowid = rleid(id),
      yours = unlist(transpose(df_step[, -1])),
      waves = unlist(transpose(df_test_step[, -1])),
      diff = round(abs(yours - waves), digits = 0),
      clr = case_when(
        equal ~ "#D9F1D5",
        diff <= 1 ~ "#FAFA8E",
        .default = "#ea9999"
      ),
      equal = NULL,
      id = NULL
    )
  tbl_step <-
    left_join(
      df_step |>
        rename_with(
          .cols = !id,
          .fn = ~paste0("Yours_", .x)
        ),
      df_test_step |>
        rename_with(
          .cols = !id,
          .fn = ~paste0("WAVES_", .x)
        ),
      by = join_by(id)
    ) |>
    gt() |>
    tab_spanner(
      label = "Oak 1.0", columns = ends_with("oak.1.0")
    ) |>
    tab_spanner(
      label = "Oak Pre", columns = ends_with("oak.pre")
    ) |>
    tab_spanner(
      label = "SDT", columns = ends_with("sdt")
    ) |>
    tab_spanner(
      label = "Stepcount", columns = ends_with("stepcount")
    ) |>
    tab_spanner(
      label = "Verisense (Original)", columns = ends_with("verisense.original")
    ) |>
    tab_spanner(
      label = "Verisense (Revised)", columns = ends_with("verisense.revised")
    )

  for (i in seq_len(nrow(df_equal_step))) {
    tbl_step <-
      tbl_step |>
      tab_style(
        style = cell_fill(color = df_equal_step$clr[i]),
        locations = cells_body(columns = ends_with(df_equal_step$method[i]),
                               rows = df_equal_step$rowid[i])
      )
  }

  tbl_step <-
    tbl_step |>
    cols_label_with(
      columns = !id,
      fn = ~sub(x = .x,
                pattern = "_.*",
                replacement = "")
    ) |>
    fmt_number(
      columns = !id,
      decimals = 0
    )

  # agreement matrix ----
  ## sed ----
  df_agr <-
    df_pipe |>
    select(starts_with("intensity")) |>
    rename_with(.cols = everything(),
                .fn = ~sub(x = .x, pattern = "intensity_", replacement = "")) |>
    mutate(across(
      .cols = everything(),
      .fns =
        ~(factor(.x,
                levels = c("sedentary", "light", "mvpa", "sleep", "nonwear", "9999"),
                labels = c("sedentary", rep("not sedentary", times = 5))) |>
        as.numeric() - 1)
    ))
  df_agr <- df_agr[complete.cases(df_agr)]
  nrow_agr <- nrow(df_agr)
  vct_methods <-
    names(df_agr) |>
    sort()
  lst_combo <- combn(vct_methods, m = 2, simplify = FALSE)
  mtx_sed <- matrix(
    NA,
    nrow = length(vct_methods),
    ncol = length(vct_methods)
  )
  colnames(mtx_sed) <- vct_methods
  rownames(mtx_sed) <- vct_methods

  for (i in seq_along(lst_combo)) {
    le_x <- lst_combo[[i]][2]
    le_y <- lst_combo[[i]][1]
    mtx_sed[le_x, le_y] <-
      sum(df_agr[[le_x]] == df_agr[[le_y]], na.rm = TRUE) /
      nrow_agr
  }
  mtx_sed <- round(mtx_sed, digits = 2)
  diag(mtx_sed) <- 1

  ## mvpa ----
  df_agr <-
    df_pipe |>
    select(starts_with("intensity")) |>
    rename_with(.cols = everything(),
                .fn = ~sub(x = .x, pattern = "intensity_", replacement = "")) |>
    select(!starts_with("bakrania")) |>
    mutate(across(
      .cols = everything(),
      .fns =
        ~(factor(.x,
                 levels = c("sedentary", "light", "mvpa", "sleep", "nonwear", "9999"),
                 labels = c(rep("not mvpa", times = 2), "mvpa", rep("not mvpa", times = 3))) |>
            as.numeric() - 1)
    ))
  df_agr <- df_agr[complete.cases(df_agr)]
  nrow_agr <- nrow(df_agr)
  vct_methods <-
    names(df_agr) |>
    sort()
  lst_combo <- combn(vct_methods, m = 2, simplify = FALSE)
  mtx_mvpa <- matrix(
    NA,
    nrow = length(vct_methods),
    ncol = length(vct_methods)
  )
  colnames(mtx_mvpa) <- vct_methods
  rownames(mtx_mvpa) <- vct_methods

  for (i in seq_along(lst_combo)) {
    le_x <- lst_combo[[i]][2]
    le_y <- lst_combo[[i]][1]
    mtx_mvpa[le_x, le_y] <-
      sum(df_agr[[le_x]] == df_agr[[le_y]], na.rm = TRUE) /
      nrow_agr
  }
  mtx_mvpa <- round(mtx_mvpa, digits = 2)
  diag(mtx_mvpa) <- 1

  ## steps ----
  df_agr <-
    df_pipe |>
    select(starts_with("steps")) |>
    rename_with(.cols = everything(),
                .fn = ~sub(x = .x, pattern = "steps_", replacement = "")) |>
    as_tibble()
  df_agr <- df_agr[complete.cases(df_agr), ]
  df_agr <- df_agr[, sort(names(df_agr))]
  mtx_step <-
    cor(df_agr) |>
    round(digits = 2)
  vct_methods <- colnames(mtx_step)
  lst_combo <- combn(vct_methods, m = 2, simplify = FALSE)

  for (i in seq_along(lst_combo)) {
    le_x <- lst_combo[[i]][1]
    le_y <- lst_combo[[i]][2]
    mtx_step[le_x, le_y] <- NA
  }

  # make agr/cor table ----
  ## sed ----
  tbl_agr_sed <-
    make_table_agr.cor(
      mtx_yours = mtx_sed,
      mtx_waves = mtx_test_sed,
      df_agr_waves = df_test_agr_sed
    ) |>
    cols_label(
      "actinet" = "Actinet",
      "bakrania.enmo.average" = "Bakrania (ENMO Average)",
      "bakrania.enmo.simple" = "Bakrania (ENMO Simple)",
      "bakrania.mad.average" = "Bakrania (MAD Average)",
      "bakrania.mad.simple" = "Bakrania (MAD Simple)",
      "ellis" = "Ellis",
      "esliger" = "Esliger",
      "fraysee" = "Fraysee",
      "hildebrand" = "Hildrebrand",
      "mielke" = "Mielke",
      "montoye.dt" = "Montoye (DT)",
      "montoye.nn" = "Montoye (NN)",
      "montoye.rf" = "Montoye (RF)",
      "montoye.svm" = "Montoye (SVM)",
      "trost" = "Trost",
      "walmsley" = "Walmsley",
      "white.enmo.lin" = "White (ENMO Linear)",
      "white.enmo.pol" = "White (ENMO Polynomial)",
      "white.hpfvm.lin" = "White (HPFVM Linear)",
      "white.hpfvm.pol" = "White (HPFVM Polynomial)"
    )
  tbl_agr_sed$`_data`$method <- case_match(
    tbl_agr_sed$`_data`$method,
    "actinet" ~ "Actinet",
    "bakrania.enmo.average" ~ "Bakrania (ENMO Average)",
    "bakrania.enmo.simple" ~ "Bakrania (ENMO Simple)",
    "bakrania.mad.average" ~ "Bakrania (MAD Average)",
    "bakrania.mad.simple" ~ "Bakrania (MAD Simple)",
    "ellis" ~ "Ellis",
    "esliger" ~ "Esliger",
    "fraysee" ~ "Fraysee",
    "hildebrand" ~ "Hildrebrand",
    "mielke" ~ "Mielke",
    "montoye.dt" ~ "Montoye (DT)",
    "montoye.nn" ~ "Montoye (NN)",
    "montoye.rf" ~ "Montoye (RF)",
    "montoye.svm" ~ "Montoye (SVM)",
    "trost" ~ "Trost",
    "walmsley" ~ "Walmsley",
    "white.enmo.lin" ~ "White (ENMO Linear)",
    "white.enmo.pol" ~ "White (ENMO Polynomial)",
    "white.hpfvm.lin" ~ "White (HPFVM Linear)",
    "white.hpfvm.pol" ~ "White (HPFVM Polynomial)"
  )

  ## mvpa ----
  tbl_agr_mvpa <- make_table_agr.cor(
    mtx_yours = mtx_mvpa,
    mtx_waves = mtx_test_mvpa,
    df_agr_waves = df_test_agr_mvpa
  ) |>
    cols_label(
      "actinet" = "Actinet",
      "ellis" = "Ellis",
      "esliger" = "Esliger",
      "fraysee" = "Fraysee",
      "hildebrand" = "Hildrebrand",
      "mielke" = "Mielke",
      "montoye.dt" = "Montoye (DT)",
      "montoye.nn" = "Montoye (NN)",
      "montoye.rf" = "Montoye (RF)",
      "montoye.svm" = "Montoye (SVM)",
      "trost" = "Trost",
      "walmsley" = "Walmsley",
      "white.enmo.lin" = "White (ENMO Linear)",
      "white.enmo.pol" = "White (ENMO Polynomial)",
      "white.hpfvm.lin" = "White (HPFVM Linear)",
      "white.hpfvm.pol" = "White (HPFVM Polynomial)"
    )
  tbl_agr_mvpa$`_data`$method <- case_match(
    tbl_agr_mvpa$`_data`$method,
    "actinet" ~ "Actinet",
    "ellis" ~ "Ellis",
    "esliger" ~ "Esliger",
    "fraysee" ~ "Fraysee",
    "hildebrand" ~ "Hildrebrand",
    "mielke" ~ "Mielke",
    "montoye.dt" ~ "Montoye (DT)",
    "montoye.nn" ~ "Montoye (NN)",
    "montoye.rf" ~ "Montoye (RF)",
    "montoye.svm" ~ "Montoye (SVM)",
    "trost" ~ "Trost",
    "walmsley" ~ "Walmsley",
    "white.enmo.lin" ~ "White (ENMO Linear)",
    "white.enmo.pol" ~ "White (ENMO Polynomial)",
    "white.hpfvm.lin" ~ "White (HPFVM Linear)",
    "white.hpfvm.pol" ~ "White (HPFVM Polynomial)"
  )
  ## step ----
  tbl_agr_step <- make_table_agr.cor(
    mtx_yours = mtx_step,
    mtx_waves = mtx_test_step,
    df_agr_waves = df_test_agr_step
  ) |>
    cols_label(
      "oak.1.0" = "Oak 1.0",
      "oak.pre" = "Oak Pre",
      "sdt" = "SDT",
      "stepcount" = "Stepcount",
      "verisense.original" = "Verisense (Original)",
      "verisense.revised" = "Verisense (Revised)"
    )
  tbl_agr_step$`_data`$method <- case_match(
    tbl_agr_step$`_data`$method,
    "oak.1.0" ~ "Oak 1.0",
    "oak.pre" ~ "Oak Pre",
    "sdt" ~ "SDT",
    "stepcount" ~ "Stepcount",
    "verisense.original" ~ "Verisense (Original)",
    "verisense.revised" ~ "Verisense (Revised)"
  )

  # return ----
  return(list(
    total_sed = tbl_sed,
    total_mvpa = tbl_mvpa,
    total_step = tbl_step,
    agree_sed = tbl_agr_sed,
    agree_mvpa = tbl_agr_mvpa,
    cor_step = tbl_agr_step
  ))

}
summarize_metrics_main <- function(fpa_merged) {

  df_pipe <- read_parquet(fpa_merged)

  # total ----
  df_sed <-
    df_pipe |>
    select(id, starts_with("intensity")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "intensity_", replacement = "")) |>
    summarise(across(
      .cols = everything(),
      .fns = ~sum(.x == "sedentary", na.rm = TRUE) / 60
    ), .by = id)
  df_sed <- df_sed[
    , c(names(df_sed)[1], sort(names(df_sed[-1])))
  ]
  tbl_sed <-
    df_sed |>
    gt() |>
      cols_label(
        "actinet" = "Actinet",
        "bakrania.enmo.average" = "Bakrania (ENMO Average)",
        "bakrania.enmo.simple" = "Bakrania (ENMO Simple)",
        "bakrania.mad.average" = "Bakrania (MAD Average)",
        "bakrania.mad.simple" = "Bakrania (MAD Simple)",
        "ellis" = "Ellis",
        "esliger" = "Esliger",
        "fraysee" = "Fraysee",
        "hildebrand" = "Hildrebrand",
        "mielke" = "Mielke",
        "montoye.dt" = "Montoye (DT)",
        "montoye.nn" = "Montoye (NN)",
        "montoye.rf" = "Montoye (RF)",
        "montoye.svm" = "Montoye (SVM)",
        "trost" = "Trost",
        "walmsley" = "Walmsley",
        "white.enmo.lin" = "White (ENMO Linear)",
        "white.enmo.pol" = "White (ENMO Polynomial)",
        "white.hpfvm.lin" = "White (HPFVM Linear)",
        "white.hpfvm.pol" = "White (HPFVM Polynomial)"
      )
  df_mvpa <-
    df_pipe |>
    select(id, starts_with("intensity")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "intensity_", replacement = "")) |>
    select(id, !starts_with("bakrania")) |>
    summarise(across(
      .cols = everything(),
      .fns = ~sum(.x == "mvpa", na.rm = TRUE) / 60
    ), .by = id)
  df_mvpa <- df_mvpa[
    , c(names(df_mvpa)[1], sort(names(df_mvpa[-1])))
  ]
  tbl_mvpa <-
    df_mvpa |>
    gt() |>
    cols_label(
      "actinet" = "Actinet",
      "ellis" = "Ellis",
      "esliger" = "Esliger",
      "fraysee" = "Fraysee",
      "hildebrand" = "Hildrebrand",
      "mielke" = "Mielke",
      "montoye.dt" = "Montoye (DT)",
      "montoye.nn" = "Montoye (NN)",
      "montoye.rf" = "Montoye (RF)",
      "montoye.svm" = "Montoye (SVM)",
      "trost" = "Trost",
      "walmsley" = "Walmsley",
      "white.enmo.lin" = "White (ENMO Linear)",
      "white.enmo.pol" = "White (ENMO Polynomial)",
      "white.hpfvm.lin" = "White (HPFVM Linear)",
      "white.hpfvm.pol" = "White (HPFVM Polynomial)"
    )
  df_step <-
    df_pipe |>
    select(id, starts_with("steps")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "steps_", replacement = "")) |>
    summarise(across(
      .cols = everything(),
      .fns = ~sum(.x, na.rm = TRUE)
    ), .by = id)
  df_step <- df_step[
    , c(names(df_step)[1], sort(names(df_step[-1])))
  ]

   ## step ----
  tbl_step <-
    df_step |>
    gt() |>
    cols_label(
      "oak.1.0" = "Oak 1.0",
      "oak.pre" = "Oak Pre",
      "sdt" = "SDT",
      "stepcount" = "Stepcount",
      "verisense.original" = "Verisense (Original)",
      "verisense.revised" = "Verisense (Revised)"
    )

  # return ----
  return(list(
    total_sed = tbl_sed,
    total_mvpa = tbl_mvpa,
    total_step = tbl_step
  ))

}
