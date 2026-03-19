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
    if (!df_equal$method2[i] %in% names(tbl_agr[["_data"]])) {
      next()
    }
    tbl_agr <-
      tbl_agr |>
      tab_style(
        style = cell_fill(color = df_equal$clr[i]),
        locations = cells_body(columns = df_equal$method2[i],
                               rows = df_equal$rowid[i])
      )
  }

  for (ind in ind_near) {
    if (!df_equal$method[ind] %in% names(tbl_agr[["_data"]])) {
      next()
    }
    tbl_agr <-
      tbl_agr |>
      tab_style(
        style = cell_fill(color = df_equal$clr[ind]),
        locations = cells_body(columns = df_equal$method[ind],
                               rows = df_equal$colid[ind])
      )
  }

  for (ind in ind_unequal) {
    if (!df_equal$method[ind] %in% names(tbl_agr[["_data"]])) {
      next()
    }
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

  align_summary_tables <- function(df_yours, df_waves) {
    common_ids <- intersect(df_yours$id, df_waves$id)
    common_cols <- intersect(names(df_yours), names(df_waves))
    common_cols <- c("id", setdiff(common_cols, "id"))

    list(
      yours =
        df_yours |>
        filter(id %in% common_ids) |>
        select(all_of(common_cols)) |>
        arrange(id),
      waves =
        df_waves |>
        filter(id %in% common_ids) |>
        select(all_of(common_cols)) |>
        arrange(id)
    )
  }

  add_spanner_if_present <- function(tbl, label, suffix) {
    matching_cols <- names(tbl[["_data"]])[stringr::str_ends(names(tbl[["_data"]]), suffix)]

    if (length(matching_cols) == 0) {
      return(tbl)
    }

    tbl |>
      tab_spanner(
        label = label,
        columns = all_of(matching_cols)
      )
  }

  style_if_present <- function(tbl, column_suffix, row_index, color) {
    matching_cols <- names(tbl[["_data"]])[stringr::str_ends(names(tbl[["_data"]]), column_suffix)]

    if (length(matching_cols) == 0) {
      return(tbl)
    }

    tbl |>
      tab_style(
        style = cell_fill(color = color),
        locations = cells_body(columns = all_of(matching_cols),
                               rows = row_index)
      )
  }

  cols_label_if_present <- function(tbl, labels) {
    labels <- labels[names(labels) %in% names(tbl[["_data"]])]

    if (length(labels) == 0) {
      return(tbl)
    }

    do.call(
      gt::cols_label,
      c(list(data = tbl), as.list(labels))
    )
  }

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
  aligned_sed <- align_summary_tables(df_sed, df_test_sed)
  df_sed <- aligned_sed$yours
  df_test_sed <- aligned_sed$waves

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
      yours = unlist(as.list(df_sed[, -1]), use.names = FALSE),
      waves = unlist(as.list(df_test_sed[, -1]), use.names = FALSE),
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
    gt()

  tbl_sed <- tbl_sed |>
    add_spanner_if_present("Actinet", "actinet") |>
    add_spanner_if_present("Bakrania (ENMO Average)", "bakrania.enmo.average") |>
    add_spanner_if_present("Bakrania (ENMO Simple)", "bakrania.enmo.simple") |>
    add_spanner_if_present("Bakrania (MAD Average)", "bakrania.mad.average") |>
    add_spanner_if_present("Bakrania (MAD Simple)", "bakrania.mad.simple") |>
    add_spanner_if_present("Ellis", "ellis") |>
    add_spanner_if_present("Esliger", "esliger") |>
    add_spanner_if_present("Fraysee", "fraysee") |>
    add_spanner_if_present("Hildrebrand", "hildebrand") |>
    add_spanner_if_present("Mielke", "mielke") |>
    add_spanner_if_present("Montoye (DT)", "montoye.dt") |>
    add_spanner_if_present("Montoye (NN)", "montoye.nn") |>
    add_spanner_if_present("Montoye (RF)", "montoye.rf") |>
    add_spanner_if_present("Montoye (SVM)", "montoye.svm") |>
    add_spanner_if_present("Trost", "trost") |>
    add_spanner_if_present("Walmsley", "walmsley") |>
    add_spanner_if_present("White (ENMO Linear)", "white.enmo.lin") |>
    add_spanner_if_present("White (ENMO Polynomial)", "white.enmo.pol") |>
    add_spanner_if_present("White (HPFVM Linear)", "white.hpfvm.lin") |>
    add_spanner_if_present("White (HPFVM Polynomial)", "white.hpfvm.pol")

  for (i in seq_len(nrow(df_equal_sed))) {
    tbl_sed <-
      style_if_present(
        tbl = tbl_sed,
        column_suffix = df_equal_sed$method[i],
        row_index = df_equal_sed$rowid[i],
        color = df_equal_sed$clr[i]
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
  aligned_mvpa <- align_summary_tables(df_mvpa, df_test_mvpa)
  df_mvpa <- aligned_mvpa$yours
  df_test_mvpa <- aligned_mvpa$waves
  df_equal_mvpa <-
    (df_mvpa[, -1] == df_test_mvpa[, -1]) |>
    as.data.frame() |>
    mutate(id = df_mvpa$id, .before = 1) |>
    pivot_longer(cols = !id,
                 names_to = "method",
                 values_to = "equal") |>
    mutate(
      rowid = rleid(id),
      yours = unlist(as.list(df_mvpa[, -1]), use.names = FALSE),
      waves = unlist(as.list(df_test_mvpa[, -1]), use.names = FALSE),
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
    gt()

  tbl_mvpa <- tbl_mvpa |>
    add_spanner_if_present("Actinet", "actinet") |>
    add_spanner_if_present("Ellis", "ellis") |>
    add_spanner_if_present("Esliger", "esliger") |>
    add_spanner_if_present("Fraysee", "fraysee") |>
    add_spanner_if_present("Hildrebrand", "hildebrand") |>
    add_spanner_if_present("Mielke", "mielke") |>
    add_spanner_if_present("Montoye (DT)", "montoye.dt") |>
    add_spanner_if_present("Montoye (NN)", "montoye.nn") |>
    add_spanner_if_present("Montoye (RF)", "montoye.rf") |>
    add_spanner_if_present("Montoye (SVM)", "montoye.svm") |>
    add_spanner_if_present("Trost", "trost") |>
    add_spanner_if_present("Walmsley", "walmsley") |>
    add_spanner_if_present("White (ENMO Linear)", "white.enmo.lin") |>
    add_spanner_if_present("White (ENMO Polynomial)", "white.enmo.pol") |>
    add_spanner_if_present("White (HPFVM Linear)", "white.hpfvm.lin") |>
    add_spanner_if_present("White (HPFVM Polynomial)", "white.hpfvm.pol")

  for (i in seq_len(nrow(df_equal_mvpa))) {
    tbl_mvpa <-
      style_if_present(
        tbl = tbl_mvpa,
        column_suffix = df_equal_mvpa$method[i],
        row_index = df_equal_mvpa$rowid[i],
        color = df_equal_mvpa$clr[i]
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
  aligned_step <- align_summary_tables(df_step, df_test_step)
  df_step <- aligned_step$yours
  df_test_step <- aligned_step$waves
  df_equal_step <-
    (df_step[, -1] == df_test_step[, -1]) |>
    as.data.frame() |>
    mutate(id = df_step$id, .before = 1) |>
    pivot_longer(cols = !id,
                 names_to = "method",
                 values_to = "equal") |>
    mutate(
      rowid = rleid(id),
      yours = unlist(as.list(df_step[, -1]), use.names = FALSE),
      waves = unlist(as.list(df_test_step[, -1]), use.names = FALSE),
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
    gt()

  tbl_step <- tbl_step |>
    add_spanner_if_present("Oak 1.0", "oak.1.0") |>
    add_spanner_if_present("Oak Pre", "oak.pre") |>
    add_spanner_if_present("SDT", "sdt") |>
    add_spanner_if_present("Stepcount", "stepcount") |>
    add_spanner_if_present("Verisense (Original)", "verisense.original") |>
    add_spanner_if_present("Verisense (Revised)", "verisense.revised")

  for (i in seq_len(nrow(df_equal_step))) {
    tbl_step <-
      style_if_present(
        tbl = tbl_step,
        column_suffix = df_equal_step$method[i],
        row_index = df_equal_step$rowid[i],
        color = df_equal_step$clr[i]
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
  df_agr <- df_agr |> filter(if_all(everything(), ~ !is.na(.x)))
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
  df_agr <- df_agr |> filter(if_all(everything(), ~ !is.na(.x)))
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
  df_agr <- df_agr |> filter(if_all(everything(), ~ !is.na(.x)))
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
    )
  tbl_agr_sed <- cols_label_if_present(
    tbl_agr_sed,
    c(
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
  )
  tbl_agr_mvpa <- cols_label_if_present(
    tbl_agr_mvpa,
    c(
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
  )
  tbl_agr_step <- cols_label_if_present(
    tbl_agr_step,
    c(
      "oak.1.0" = "Oak 1.0",
      "oak.pre" = "Oak Pre",
      "sdt" = "SDT",
      "stepcount" = "Stepcount",
      "verisense.original" = "Verisense (Original)",
      "verisense.revised" = "Verisense (Revised)"
    )
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
