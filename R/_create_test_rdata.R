library(targets)
library(tarchetypes)
library(crew)
library(autometric)
Sys.setenv(TAR_PROJECT = "config")
source("packages.R") |>
  suppressMessages() |>
  suppressWarnings()
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
# write_parquet(tar_read(df_pipe), "data/0_CONFIG/MERGED/WAVES_ALL_TEST.parquet")

df_test <- read_parquet("data/0_CONFIG/MERGED/WAVES_ALL_TEST.parquet")
# total -----
df_test_sed <-
  df_test |>
  select(id, starts_with("intensity")) |>
  rename_with(.cols = !id,
              .fn = ~sub(x = .x, pattern = "intensity_", replacement = "")) |>
  summarise(across(
    .cols = everything(),
    .fns = ~sum(.x == "sedentary", na.rm = TRUE) / 60
  ), .by = id)
df_test_sed <- df_test_sed[
  , c(names(df_test_sed)[1], sort(names(df_test_sed[-1])))
]
df_test_mvpa <-
  df_test |>
  select(id, starts_with("intensity")) |>
  rename_with(.cols = !id,
              .fn = ~sub(x = .x, pattern = "intensity_", replacement = "")) |>
  select(id, !starts_with("bakrania")) |>
  summarise(across(
    .cols = everything(),
    .fns = ~sum(.x == "mvpa", na.rm = TRUE) / 60
  ), .by = id)
df_test_mvpa <- df_test_mvpa[
  , c(names(df_test_mvpa)[1], sort(names(df_test_mvpa[-1])))
]
df_test_step <-
  df_test |>
  select(id, starts_with("steps")) |>
  rename_with(.cols = !id,
              .fn = ~sub(x = .x, pattern = "steps_", replacement = "")) |>
  summarise(across(
    .cols = everything(),
    .fns = ~sum(.x, na.rm = TRUE)
  ), .by = id)
df_test_step <- df_test_step[
  , c(names(df_test_step)[1], sort(names(df_test_step[-1])))
]

# agreement matrix ----
## sed ----
df_test_agr <-
  df_test |>
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
df_test_agr <- df_test_agr[complete.cases(df_test_agr)]
nrow_agr <- nrow(df_test_agr)
vct_methods <-
  names(df_test_agr) |>
  sort()
lst_combo <- combn(vct_methods, m = 2, simplify = FALSE)
mtx_test_sed <- matrix(
  NA,
  nrow = length(vct_methods),
  ncol = length(vct_methods)
)
colnames(mtx_test_sed) <- vct_methods
rownames(mtx_test_sed) <- vct_methods

for (i in seq_along(lst_combo)) {
  le_x <- lst_combo[[i]][1]
  le_y <- lst_combo[[i]][2]
  mtx_test_sed[le_x, le_y] <-
    sum(df_test_agr[[le_x]] == df_test_agr[[le_y]], na.rm = TRUE) /
    nrow_agr
}
mtx_test_sed <- round(mtx_test_sed, digits = 2)
diag(mtx_test_sed) <- 1

## mvpa ----
df_test_agr <-
  df_test |>
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
df_test_agr <- df_test_agr[complete.cases(df_test_agr)]
nrow_agr <- nrow(df_test_agr)
vct_methods <-
  names(df_test_agr) |>
  sort()
lst_combo <- combn(vct_methods, m = 2, simplify = FALSE)
mtx_test_mvpa <- matrix(
  NA,
  nrow = length(vct_methods),
  ncol = length(vct_methods)
)
colnames(mtx_test_mvpa) <- vct_methods
rownames(mtx_test_mvpa) <- vct_methods

for (i in seq_along(lst_combo)) {
  le_x <- lst_combo[[i]][1]
  le_y <- lst_combo[[i]][2]
  mtx_test_mvpa[le_x, le_y] <-
    sum(df_test_agr[[le_x]] == df_test_agr[[le_y]], na.rm = TRUE) /
    nrow_agr
}
mtx_test_mvpa <- round(mtx_test_mvpa, digits = 2)
diag(mtx_test_mvpa) <- 1

## steps ----
df_test_agr <-
  df_test |>
  select(starts_with("steps")) |>
  rename_with(.cols = everything(),
              .fn = ~sub(x = .x, pattern = "steps_", replacement = "")) |>
  as_tibble()
df_test_agr <- df_test_agr[complete.cases(df_test_agr), ]
df_test_agr <- df_test_agr[, sort(names(df_test_agr))]
mtx_test_step <-
  cor(df_test_agr) |>
  round(digits = 2)
vct_methods <- colnames(mtx_test_step)
lst_combo <- combn(vct_methods, m = 2, simplify = FALSE)

for (i in seq_along(lst_combo)) {
  le_x <- lst_combo[[i]][2]
  le_y <- lst_combo[[i]][1]
  mtx_test_step[le_x, le_y] <- NA
}

# agreement df ----
df_test_agr_sed <-
  mtx_test_sed |>
  as.data.frame() |>
  rownames_to_column(var = "method2") |>
  pivot_longer(cols = !method2,
               names_to = "method",
               values_to = "waves",
               values_drop_na = TRUE) |>
  dplyr::filter(method != method2)
df_test_agr_mvpa <-
  mtx_test_mvpa |>
  as.data.frame() |>
  rownames_to_column(var = "method2") |>
  pivot_longer(cols = !method2,
               names_to = "method",
               values_to = "waves",
               values_drop_na = TRUE) |>
  dplyr::filter(method != method2)
df_test_agr_step <-
  mtx_test_step |>
  as.data.frame() |>
  rownames_to_column(var = "method2") |>
  pivot_longer(cols = !method2,
               names_to = "method",
               values_to = "waves",
               values_drop_na = TRUE) |>
  dplyr::filter(method != method2)

# Write ----
save(
  # total
  df_test_sed,
  df_test_mvpa,
  df_test_step,
  # matrix
  mtx_test_sed,
  mtx_test_mvpa,
  mtx_test_step,
  # agreement
  df_test_agr_sed,
  df_test_agr_mvpa,
  df_test_agr_step,
  file = "data/0_CONFIG/MERGED/WAVES_ALL_TEST.RData"
)
