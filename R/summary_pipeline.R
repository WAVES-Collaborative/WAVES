summarize_major_steps <- function(vct_raw,
                                  vct_basic,
                                  vct_nw.sleep,
                                  vct_cal,
                                  vct_out.raw,
                                  vct_out.oak.pre,
                                  vct_out.cut,
                                  vct_ox_step,
                                  vct_ox_wlms,
                                  vct_ox_acti) {

  df <- tibble(
    file       = basename(vct_raw),
    file_noext = strip_all_ext(file)
  )
  vct_nm_ggir <-
    basename(vct_basic) |>
    gsub(x = _,
         pattern = "meta_|\\.RData",
         replacement = "")
  vct_nm_nw.sleep <-
    basename(vct_nw.sleep) |>
    file_path_sans_ext()
  vct_nm_cal <-
    basename(vct_cal) |>
    file_path_sans_ext()
  vct_nm_raw <-
    basename(vct_out.raw) |>
    file_path_sans_ext()
  vct_nm_oak.pre <-
    basename(vct_out.oak.pre) |>
    file_path_sans_ext()
  vct_nm_cut <-
    basename(vct_out.cut) |>
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
      nw.sleep      = file_noext %in% vct_nm_nw.sleep,
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
summarize_major_main_steps <- function(lst_yaml,
                                       vct_out.ref,
                                       vct_raw,
                                       vct_basic,
                                       vct_nw.sleep,
                                       vct_cal,
                                       vct_out.raw,
                                       vct_out.oak.pre,
                                       vct_out.cut,
                                       vct_ox_step,
                                       vct_ox_wlms,
                                       vct_ox_acti) {

  df <- tibble(
    id         = stri_extract(
      basename(vct_raw),
      # TODO: When working with UWM data, have a id_pt
      # for raw data. What to do if lst_yaml$raw$id_pt
      # is not the same as lst_yaml$ref$pal$id_pt?
      regex = paste0(lst_yaml$ref$pal$id_pt, collapse = "|")
    ),
    file       = basename(vct_raw),
    file_noext = strip_all_ext(file)
  )

  if (!all(simplify_is_null(lst_yaml$ref$do))) {
    vct_nm_pal <-
      basename(vct_out.ref) |>
      file_path_sans_ext() |>
      grep(x = _,
           pattern = "do",
           value = TRUE) |>
      stri_replace(regex = "_do",
                   replacement = "")
  }
  if (!all(simplify_is_null(lst_yaml$ref$pal))) {
    vct_nm_pal <-
      basename(vct_out.ref) |>
      file_path_sans_ext() |>
      grep(x = _,
           pattern = "pal",
           value = TRUE) |>
      stri_replace(regex = "_pal",
                   replacement = "")
  }
  if (!all(simplify_is_null(lst_yaml$ref$pal))) {
    vct_nm_pal <-
      basename(vct_out.ref) |>
      file_path_sans_ext() |>
      grep(x = _,
           pattern = "pal",
           value = TRUE) |>
      stri_replace(regex = "_pal",
                   replacement = "")
  }
  vct_nm_ggir <-
    basename(vct_basic) |>
    gsub(x = _,
         pattern = "meta_|\\.RData",
         replacement = "")
  vct_nm_nw.sleep <-
    basename(vct_nw.sleep) |>
    file_path_sans_ext()
  vct_nm_cal <-
    basename(vct_cal) |>
    file_path_sans_ext()
  vct_nm_raw <-
    basename(vct_out.raw) |>
    file_path_sans_ext()
  vct_nm_oak.pre <-
    basename(vct_out.oak.pre) |>
    file_path_sans_ext()
  vct_nm_cut <-
    basename(vct_out.cut) |>
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
      DO            = if (exists("vct_nm_do")) {id %in% vct_nm_do} else NULL,
      activPAL      = if (exists("vct_nm_pal")) {id %in% vct_nm_pal} else NULL,
      ActiPass      = if (exists("vct_nm_pass")) {id %in% vct_nm_pass} else NULL,
      GGIR          = file %in% vct_nm_ggir,
      nw.sleep      = file_noext %in% vct_nm_nw.sleep,
      calibration   = file_noext %in% vct_nm_cal,
      `raw methods` = file_noext %in% vct_nm_raw,
      oak.pre       = file_noext %in% vct_nm_oak.pre,
      `cutpoints`   = file_noext %in% vct_nm_cut,
      stepcount     = file_noext %in% vct_nm_stp,
      walmsley      = file_noext %in% vct_nm_wlm,,
      actinet       = file_noext %in% vct_nm_act,,
      file          = NULL,
      file_noext    = NULL
    )
}
split_in_seq <- function(x, size) {
  split(
    x,
    ceiling(seq_along(x) / size)
  )
}
print_total_gt <- function(lst_summary,
                           lst_methods,
                           le_tbl,
                           size) {
  lst_split <- split_in_seq(lst_methods[[le_tbl]], size = size)
  for (i in seq_along(lst_split)) {
    vct_hide <-
      lst_methods[[le_tbl]][!lst_methods[[le_tbl]] %in% lst_split[[i]]]
    le_gt <-
      lst_summary[[le_tbl]] |>
      cols_hide(
        matches(paste0(vct_hide, collapse = "|"))
      ) |>
      cols_width(
        matches("id") ~ pct(20),
        everything()  ~ pct(8)
      )
    cat(
      knitr::knit_child(
        text = c(
          "```{r}",
          "#| echo: false",
          "le_gt",
          "```"
        ),
        quiet = TRUE
      ),
      sep = "\n"
    )
  }
  # lapply(
  #   split_in_seq(lst_methods[[le_tbl]], size = size),
  #   \(x) {
  #     vct_hide <-
  #       lst_methods[[le_tbl]][!lst_methods[[le_tbl]] %in% x]
  #     le_gt <-
  #       lst_summary[[le_tbl]] |>
  #       cols_hide(
  #         matches(paste0(vct_hide, collapse = "|"))
  #       ) |>
  #       cols_width(
  #         matches("id") ~ pct(20)
  #       )
  #     cat(
  #       knitr::knit_child(
  #         text = c(
  #           "```{r}",
  #           "#| echo: false",
  #           "le_gt",
  #           "```"
  #         ),
  #         quiet = TRUE
  #       ),
  #       sep = "\n"
  #     )
  #   }
  # ) |> print()
}
print_agree_gt <- function(lst_summary,
                           lst_methods,
                           le_tbl,
                           size) {
  le_tbl <- "agree_mvpa"
  size   <- 5
  lst_split <- split_in_seq(lst_methods[[le_tbl]], size = size)
  for (i in seq_along(lst_split)) {
    ind_methods <- which(lst_methods[[le_tbl]] %in% lst_split[[i]])
    le_df <-
      lst_summary[[le_tbl]][["_data"]] |>
      select(c(1, ind_methods + 1)) |>
      slice(ind_methods)
    le_styles <-
      lst_summary[[le_tbl]][["_styles"]] |>
      dplyr::filter(colname %in% names(le_df)[-1],
                    rownum %in% ind_methods) |>
      mutate(
        colname = recode_values(
          colname,
          from = names(le_df)[-1],
          to   = le_df$method
        ),
        rownum = rownum - (5 * (i - 1))
      )
    names(le_df) <- c(
      "Method",
      le_df$method
    )
    le_gt <-
      le_df |>
      gt() |>
      cols_width(
        matches("Method") ~ pct(20),
        everything()      ~ pct(16)
      )
    for (ii in seq_len(nrow(le_styles))) {
      le_gt <-
        le_gt |>
        tab_style(
          style = cell_fill(color = le_styles$styles[[ii]]$cell_fill$color),
          locations = cells_body(columns = le_styles$colname[ii],
                                 rows = le_styles$rownum[ii])
        )
    }
    cat(
      knitr::knit_child(
        text = c(
          "```{r}",
          "#| echo: false",
          "le_gt",
          "```"
        ),
        quiet = TRUE
      ),
      sep = "\n"
    )
  }
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
summarize_metrics_config <- function(df_pipe) {

  load("data/0_CONFIG/MERGED/WAVES_ALL_TEST.RData")

  # total ----
  # df_nw.sleep
  #   df_pipe |>
  #   select(id, date, invalid, sleep) |>
  #   summarise(
  #     `invalid (hours)` = sum(invalid, na.rm = TRUE) / 60 / 60,
  #     `sleep (hours)`   = sum(sleep, na.rm = TRUE) / 60 / 60,
  #     .by = c(id, date)
  #   ) |>
  #   gt(groupname_col = "id",
  #      rowname_col   = "date")
  df_sed <-
    df_pipe |>
    select(id, starts_with("intensity3")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "intensity3_", replacement = "")) |>
    summarise(across(
      .cols = everything(),
      .fns = ~sum(.x == "sedentary", na.rm = TRUE) / 60
    ), .by = id)
  df_sed <- df_sed[
    , c(names(df_sed)[1], sort(names(df_sed[-1])))
  ]
  df_mvpa <-
    df_pipe |>
    select(id, starts_with("intensity3")) |>
    rename_with(.cols = !id,
                .fn = ~sub(x = .x, pattern = "intensity3_", replacement = "")) |>
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
      label = "Bakrania (ENMO Avg)", columns = ends_with("bakrania.enmo.average")
    ) |>
    tab_spanner(
      label = "Bakrania (ENMO Simp)", columns = ends_with("bakrania.enmo.simple")
    ) |>
    tab_spanner(
      label = "Bakrania (MAD Avg)", columns = ends_with("bakrania.mad.average")
    ) |>
    tab_spanner(
      label = "Bakrania (MAD Simp)", columns = ends_with("bakrania.mad.simple")
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
      label = "Hildebrand", columns = ends_with("hildebrand")
    ) |>
    tab_spanner(
      label = "GGIR Default", columns = ends_with("ggir")
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
      label = "White (ENMO Poly)", columns = ends_with("white.enmo.pol")
    ) |>
    tab_spanner(
      label = "White (HPFVM Linear)", columns = ends_with("white.hpfvm.lin")
    ) |>
    tab_spanner(
      label = "White (HPFVM Poly)", columns = ends_with("white.hpfvm.pol")
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
      label = "Hildebrand", columns = ends_with("hildebrand")
    ) |>
    tab_spanner(
      label = "GGIR Default", columns = ends_with("ggir")
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
      label = "White (ENMO Poly)", columns = ends_with("white.enmo.pol")
    ) |>
    tab_spanner(
      label = "White (HPFVM Linear)", columns = ends_with("white.hpfvm.lin")
    ) |>
    tab_spanner(
      label = "White (HPFVM Poly)", columns = ends_with("white.hpfvm.pol")
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
      label = "Trost", columns = ends_with("trost")
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
    select(starts_with("intensity3")) |>
    rename_with(.cols = everything(),
                .fn = ~sub(x = .x, pattern = "intensity3_", replacement = "")) |>
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
    select(starts_with("intensity3")) |>
    rename_with(.cols = everything(),
                .fn = ~sub(x = .x, pattern = "intensity3_", replacement = "")) |>
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
      "bakrania.enmo.average" = "Bakrania (ENMO Avg)",
      "bakrania.enmo.simple" = "Bakrania (ENMO Simp)",
      "bakrania.mad.average" = "Bakrania (MAD Avg)",
      "bakrania.mad.simple" = "Bakrania (MAD Simp)",
      "ellis" = "Ellis",
      "esliger" = "Esliger",
      "fraysee" = "Fraysee",
      "hildebrand" = "Hildebrand",
      "ggir" = "GGIR Default",
      "mielke" = "Mielke",
      "montoye.dt" = "Montoye (DT)",
      "montoye.nn" = "Montoye (NN)",
      "montoye.rf" = "Montoye (RF)",
      "montoye.svm" = "Montoye (SVM)",
      "trost" = "Trost",
      "walmsley" = "Walmsley",
      "white.enmo.lin" = "White (ENMO Linear)",
      "white.enmo.pol" = "White (ENMO Poly)",
      "white.hpfvm.lin" = "White (HPFVM Linear)",
      "white.hpfvm.pol" = "White (HPFVM Poly)"
    )
  tbl_agr_sed$`_data`$method <- case_match(
    tbl_agr_sed$`_data`$method,
    "actinet" ~ "Actinet",
    "bakrania.enmo.average" ~ "Bakrania (ENMO Avg)",
    "bakrania.enmo.simple" ~ "Bakrania (ENMO Simp)",
    "bakrania.mad.average" ~ "Bakrania (MAD Avg)",
    "bakrania.mad.simple" ~ "Bakrania (MAD Simp)",
    "ellis" ~ "Ellis",
    "esliger" ~ "Esliger",
    "fraysee" ~ "Fraysee",
    "hildebrand" ~ "Hildebrand",
    "ggir" ~ "GGIR Default",
    "mielke" ~ "Mielke",
    "montoye.dt" ~ "Montoye (DT)",
    "montoye.nn" ~ "Montoye (NN)",
    "montoye.rf" ~ "Montoye (RF)",
    "montoye.svm" ~ "Montoye (SVM)",
    "trost" ~ "Trost",
    "walmsley" ~ "Walmsley",
    "white.enmo.lin" ~ "White (ENMO Linear)",
    "white.enmo.pol" ~ "White (ENMO Poly)",
    "white.hpfvm.lin" ~ "White (HPFVM Linear)",
    "white.hpfvm.pol" ~ "White (HPFVM Poly)"
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
      "hildebrand" = "Hildebrand",
      "ggir" = "GGIR Default",
      "mielke" = "Mielke",
      "montoye.dt" = "Montoye (DT)",
      "montoye.nn" = "Montoye (NN)",
      "montoye.rf" = "Montoye (RF)",
      "montoye.svm" = "Montoye (SVM)",
      "trost" = "Trost",
      "walmsley" = "Walmsley",
      "white.enmo.lin" = "White (ENMO Linear)",
      "white.enmo.pol" = "White (ENMO Poly)",
      "white.hpfvm.lin" = "White (HPFVM Linear)",
      "white.hpfvm.pol" = "White (HPFVM Poly)"
    )
  tbl_agr_mvpa$`_data`$method <- case_match(
    tbl_agr_mvpa$`_data`$method,
    "actinet" ~ "Actinet",
    "ellis" ~ "Ellis",
    "esliger" ~ "Esliger",
    "fraysee" ~ "Fraysee",
    "hildebrand" ~ "Hildebrand",
    "ggir" ~ "GGIR Default",
    "mielke" ~ "Mielke",
    "montoye.dt" ~ "Montoye (DT)",
    "montoye.nn" ~ "Montoye (NN)",
    "montoye.rf" ~ "Montoye (RF)",
    "montoye.svm" ~ "Montoye (SVM)",
    "trost" ~ "Trost",
    "walmsley" ~ "Walmsley",
    "white.enmo.lin" ~ "White (ENMO Linear)",
    "white.enmo.pol" ~ "White (ENMO Poly)",
    "white.hpfvm.lin" ~ "White (HPFVM Linear)",
    "white.hpfvm.pol" ~ "White (HPFVM Poly)"
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
      "trost" = "Trost",
      "verisense.original" = "Verisense (Original)",
      "verisense.revised" = "Verisense (Revised)"
    )
  tbl_agr_step$`_data`$method <- case_match(
    tbl_agr_step$`_data`$method,
    "oak.1.0" ~ "Oak 1.0",
    "oak.pre" ~ "Oak Pre",
    "sdt" ~ "SDT",
    "stepcount" ~ "Stepcount",
    "trost" ~ "Trost",
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
summarize_metrics_main <- function(lst_yaml,
                                   vct_ref,
                                   vct_merge) {

  lst_sed  <- vector(mode = "list", length(vct_merge))
  lst_mvpa <- vector(mode = "list", length(vct_merge))
  lst_step <- vector(mode = "list", length(vct_merge))

  for (i in seq_along(vct_merge)) {

    df_pipe <- read_parquet(vct_merge[i])

    df_sed <-
      df_pipe |>
      select(id, starts_with("intensity3")) |>
      rename_with(.cols = !id,
                  .fn = ~sub(x = .x, pattern = "intensity3_", replacement = "")) |>
      summarise(id = id[1], across(
        .cols = !id,
        .fns = ~sum(.x == "sedentary", na.rm = TRUE) / 60
      ))
    lst_sed[[i]] <- df_sed[
      , c(names(df_sed)[1], sort(names(df_sed[-1])))
    ]
    df_mvpa <-
      df_pipe |>
      select(id, starts_with("intensity3")) |>
      rename_with(.cols = !id,
                  .fn = ~sub(x = .x, pattern = "intensity3_", replacement = "")) |>
      select(id, !starts_with("bakrania")) |>
      summarise(id = id[1], across(
        .cols = !id,
        .fns = ~sum(.x == "mvpa", na.rm = TRUE) / 60
      ))
    lst_mvpa[[i]] <- df_mvpa[
      , c(names(df_mvpa)[1], sort(names(df_mvpa[-1])))
    ]
    df_step <-
      df_pipe |>
      select(id, starts_with("steps")) |>
      rename_with(.cols = !id,
                  .fn = ~sub(x = .x, pattern = "steps_", replacement = "")) |>
      summarise(id = id[1], across(
        .cols = !id,
        .fns = ~sum(.x, na.rm = TRUE)
      ))
    lst_step[[i]] <- df_step[
      , c(names(df_step)[1], sort(names(df_step[-1])))
    ]

  }

  vct_label_sed <- c(
    "do"   = if (vct_ref["do"]) "DO" else NULL,
    "palp" = if (vct_ref["pal"]) "activPAL Epoch" else NULL,
    "palv" = if (vct_ref["pal"]) "activPAL Event" else NULL,
    "pass" = if (vct_ref["pass"]) "ActiPass" else NULL,
    "actinet"               = "Actinet",
    "bakrania.enmo.average" = "Bakrania (ENMO Avg)",
    "bakrania.enmo.simple"  = "Bakrania (ENMO Simp)",
    "bakrania.mad.average"  = "Bakrania (MAD Avg)",
    "bakrania.mad.simple"   = "Bakrania (MAD Simp)",
    "ellis"                 = "Ellis",
    "esliger"               = "Esliger",
    "fraysee"               = "Fraysee",
    "hildebrand"            = "Hildebrand",
    "ggir"                  = "GGIR Default",
    "mielke"                = "Mielke",
    "montoye.dt"            = "Montoye (DT)",
    "montoye.nn"            = "Montoye (NN)",
    "montoye.rf"            = "Montoye (RF)",
    "montoye.svm"           = "Montoye (SVM)",
    "trost"                 = "Trost",
    "walmsley"              = "Walmsley",
    "white.enmo.lin"        = "White (ENMO Linear)",
    "white.enmo.pol"        = "White (ENMO Poly)",
    "white.hpfvm.lin"       = "White (HPFVM Linear)",
    "white.hpfvm.pol"       = "White (HPFVM Poly)"
  )
  tbl_sed <-
    rbindlist(lst_sed) |>
    select(
      id,
      starts_with("do"),
      starts_with("pal"),
      starts_with("pass"),
      everything()
    ) |>
    mutate(
      # TODO: change when id_pt is implemented in vct_raw/working with UWM data.
      id = stri_extract(
        id,
        regex = paste0(lst_yaml$ref$pal$id_pt, collapse = "|")
      ),
      across(
        .cols = !id,
        .fns = ~round(.x, digits = 1)
      )
    ) |>
    gt() |>
    cols_label(.list = vct_label_sed)
  vct_label_mvpa <- c(
    "do"   = if (vct_ref["do"]) "DO" else NULL,
    "palp" = if (vct_ref["pal"]) "activPAL Epoch" else NULL,
    "palv" = if (vct_ref["pal"]) "activPAL Event" else NULL,
    "pass" = if (vct_ref["pass"]) "ActiPass" else NULL,
    "actinet"         = "Actinet",
    "ellis"           = "Ellis",
    "esliger"         = "Esliger",
    "fraysee"         = "Fraysee",
    "hildebrand"      = "Hildebrand",
    "ggir"            = "GGIR Default",
    "mielke"          = "Mielke",
    "montoye.dt"      = "Montoye (DT)",
    "montoye.nn"      = "Montoye (NN)",
    "montoye.rf"      = "Montoye (RF)",
    "montoye.svm"     = "Montoye (SVM)",
    "trost"           = "Trost",
    "walmsley"        = "Walmsley",
    "white.enmo.lin"  = "White (ENMO Linear)",
    "white.enmo.pol"  = "White (ENMO Poly)",
    "white.hpfvm.lin" = "White (HPFVM Linear)",
    "white.hpfvm.pol" = "White (HPFVM Poly)"
  )
  tbl_mvpa <-
    rbindlist(lst_mvpa) |>
    select(
      id,
      starts_with("do"),
      starts_with("pal"),
      starts_with("pass"),
      everything()
    ) |>
    mutate(
      # TODO: change when id_pt is implemented in vct_raw/working with UWM data.
      id = stri_extract(
        id,
        regex = paste0(lst_yaml$ref$pal$id_pt, collapse = "|")
      ),
      across(
        .cols = !id,
        .fns = ~round(.x, digits = 1)
      )
    ) |>
    gt() |>
    cols_label(.list = vct_label_mvpa)
  vct_label_step <- c(
    "do"   = if (vct_ref["do"]) "DO" else NULL,
    "palp" = if (vct_ref["pal"]) "activPAL Epoch" else NULL,
    "palv" = if (vct_ref["pal"]) "activPAL Event" else NULL,
    "pass" = if (vct_ref["pass"]) "ActiPass" else NULL,
    "oak.1.0"            = "Oak 1.0",
    "oak.pre"            = "Oak Pre",
    "sdt"                = "SDT",
    "stepcount"          = "Stepcount",
    "trost"              = "Trost",
    "verisense.original" = "Verisense (Original)",
    "verisense.revised"  = "Verisense (Revised)"
  )
  tbl_step <-
    rbindlist(lst_step) |>
    select(
      id,
      starts_with("do"),
      starts_with("pal"),
      starts_with("pass"),
      everything()
    ) |>
    mutate(
      # TODO: change when id_pt is implemented in vct_raw/working with UWM data.
      id = stri_extract(
        id,
        regex = paste0(lst_yaml$ref$pal$id_pt, collapse = "|")
      ),
      across(
        .cols = !id,
        .fns = ~round(.x, digits = 1)
      )
    ) |>
    gt() |>
    cols_label(.list = vct_label_step)

  # return ----
  return(list(
    total_sed = tbl_sed,
    total_mvpa = tbl_mvpa,
    total_step = tbl_step
  ))

}
