summary_config <- function(vct_raw,
                           vct_basic,
                           vct_cal,
                           lst_out.raw,
                           lst_out.cut,
                           vct_ox_step,
                           vct_ox_wlms,
                           vct_ox_acti) {

  lst_out.raw[sapply(lst_out.raw, is.null)] <- NULL
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
    GGIR = file %in% vct_nm_ggir,
    calibration = file_noext %in% vct_nm_cal,
    `raw methods` = file_noext %in% vct_nm_raw,
    `cutpoints` = file_noext %in% vct_nm_cut,
    stepcount = file_noext %in% vct_nm_stp,
    walmsley = file_noext %in% vct_nm_wlm,,
    actinet = file_noext %in% vct_nm_act,,
    file_noext = NULL
  )
}

metrics_config <- function(fpa_merged) {

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
  df_step <-
    df_pipe |>
    select(id, starts_with("steps")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "steps_", replacement = "")) |>
    summarise(across(
      .cols = everything(),
      .fns = ~sum(.x, na.rm = TRUE)
    ), .by = id)

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
                .fn = ~sub(x = .x, pattern = "steps_", replacement = ""))
  df_agr <- df_agr[complete.cases(df_agr)]
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

  # agreement table ----
  df_agr_sed <-
    mtx_sed |>
    as.data.frame() |>
    rownames_to_column(var = "method") |>
    pivot_longer(cols = !method,
                 names_to = "method2",
                 values_to = "config",
                 values_drop_na = TRUE) |>
    dplyr::filter(method != method2)
  df_agr_mvpa <-
    mtx_mvpa |>
    as.data.frame() |>
    rownames_to_column(var = "method") |>
    pivot_longer(cols = !method,
                 names_to = "method2",
                 values_to = "config",
                 values_drop_na = TRUE) |>
    dplyr::filter(method != method2)
  df_agr_step <-
    mtx_step |>
    as.data.frame() |>
    rownames_to_column(var = "method") |>
    pivot_longer(cols = !method,
                 names_to = "method2",
                 values_to = "config",
                 values_drop_na = TRUE) |>
    dplyr::filter(method != method2)

  # Compare total ----
  ## sed ----
  df_equal_sed <-
    (df_sed[, -1] == df_test_sed[, -1]) |>
    as.data.frame() |>
    mutate(id = df_sed$id, .before = 1) |>
    pivot_longer(cols = !id,
                 names_to = "method",
                 values_to = "equal") |>
    mutate(rowid = rleid(id),
           clr = ifelse(equal,
                        yes = "#D9F1D5",
                        no  = "#B31529"),
           id = NULL,
           equal = NULL)
  tbl_sed <- gt(df_sed)

  for (i in seq_len(nrow(df_equal_sed))) {
    tbl_sed <-
      tbl_sed |>
      tab_style(
        style = cell_fill(color = df_equal_sed$clr[i]),
        locations = cells_body(columns = df_equal_sed$method[i],
                               rows = df_equal_sed$rowid[i])
      )
  }

  ## mvpa ----
  df_equal_mvpa <-
    (df_mvpa[, -1] == df_test_mvpa[, -1]) |>
    as.data.frame() |>
    mutate(id = df_mvpa$id, .before = 1) |>
    pivot_longer(cols = !id,
                 names_to = "method",
                 values_to = "equal") |>
    mutate(rowid = rleid(id),
           clr = ifelse(equal,
                        yes = "#D9F1D5",
                        no  = "#B31529"),
           id = NULL,
           equal = NULL)
  tbl_mvpa <- gt(df_mvpa)

  for (i in seq_len(nrow(df_equal_mvpa))) {
    tbl_mvpa <-
      tbl_mvpa |>
      tab_style(
        style = cell_fill(color = df_equal_mvpa$clr[i]),
        locations = cells_body(columns = df_equal_mvpa$method[i],
                               rows = df_equal_mvpa$rowid[i])
      )
  }

  ## steps ----
  df_equal_step <-
    (df_step[, -1] == df_test_step[, -1]) |>
    as.data.frame() |>
    mutate(id = df_step$id, .before = 1) |>
    pivot_longer(cols = !id,
                 names_to = "method",
                 values_to = "equal") |>
    mutate(rowid = rleid(id),
           clr = ifelse(equal,
                        yes = "#D9F1D5",
                        no  = "#B31529"),
           id = NULL,
           equal = NULL)
  tbl_step <- gt(df_step)

  for (i in seq_len(nrow(df_equal_step))) {
    tbl_step <-
      tbl_step |>
      tab_style(
        style = cell_fill(color = df_equal_step$clr[i]),
        locations = cells_body(columns = df_equal_step$method[i],
                               rows = df_equal_step$rowid[i])
      )
  }

  # Compare agreement/correlation ----
  ## sed ----
  df_equal_sed <-
    left_join(
      df_agr_sed,
      df_test_agr_sed,
      by = join_by(method, method2)
    ) |>
    mutate(rowid = rleid(method) + 1,
           clr = ifelse(config == test,
                        yes = "#D9F1D5",
                        no  = "#B31529"),
           method = NULL)
  tbl_agr_sed <-
    pmax(mtx_sed, mtx_test_sed, na.rm = TRUE) |>
    as.data.frame() |>
    rownames_to_column(var = "method") |>
    gt()

  for (i in seq_len(nrow(df_equal_sed))) {
    tbl_agr_sed <-
      tbl_agr_sed |>
      tab_style(
        style = cell_fill(color = df_equal_sed$clr[i]),
        locations = cells_body(columns = df_equal_sed$method2[i],
                               rows = df_equal_sed$rowid[i])
      )
  }

  ## mvpa ----
  df_equal_mvpa <-
    left_join(
      df_agr_mvpa,
      df_test_agr_mvpa,
      by = join_by(method, method2)
    ) |>
    mutate(rowid = rleid(method) + 1,
           clr = ifelse(config == test,
                        yes = "#D9F1D5",
                        no  = "#B31529"),
           method = NULL)
  tbl_agr_mvpa <-
    pmax(mtx_mvpa, mtx_test_mvpa, na.rm = TRUE) |>
    as.data.frame() |>
    rownames_to_column(var = "method") |>
    gt()

  for (i in seq_len(nrow(df_equal_mvpa))) {
    tbl_agr_mvpa <-
      tbl_agr_mvpa |>
      tab_style(
        style = cell_fill(color = df_equal_mvpa$clr[i]),
        locations = cells_body(columns = df_equal_mvpa$method2[i],
                               rows = df_equal_mvpa$rowid[i])
      )
  }

  ## step ----
  df_equal_step <-
    left_join(
      df_agr_step,
      df_test_agr_step,
      by = join_by(method, method2)
    ) |>
    mutate(rowid = rleid(method) + 1,
           clr = ifelse(config == test,
                        yes = "#D9F1D5",
                        no  = "#B31529"),
           method = NULL)
  tbl_agr_step <-
    pmax(mtx_step, mtx_test_step, na.rm = TRUE) |>
    as.data.frame() |>
    rownames_to_column(var = "method") |>
    gt()

  for (i in seq_len(nrow(df_equal_step))) {
    tbl_agr_step <-
      tbl_agr_step |>
      tab_style(
        style = cell_fill(color = df_equal_step$clr[i]),
        locations = cells_body(columns = df_equal_step$method2[i],
                               rows = df_equal_step$rowid[i])
      )
  }

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
