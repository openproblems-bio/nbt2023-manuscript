library(tidyverse)
library(funkyheatmap)

task_info_jsons <- list.files("website/results/", pattern = "task_info.json", recursive = TRUE, full.names = TRUE)

source("scripts/_helper_read_task_results.R")

data_lt <- read_task_results("website/results/cell_cell_communication_ligand_target/data")
data_st <- read_task_results("website/results/cell_cell_communication_source_target/data")

figure_prefix <- paste0("figures/figure2/fig2c")
if (!dir.exists(dirname(figure_prefix))) {
  dir.create(dirname(figure_prefix), recursive = TRUE, showWarnings = FALSE)
}

# load scores
data <- data_lt$method_info %>%
  filter(!is_baseline) %>%
  select(method_id, method_name) %>%
  left_join(data_lt$per_dataset %>% rename(lt_dataset_tnbc_data = dataset_tnbc_data), by = "method_id") %>%
  left_join(data_lt$per_metric %>% rename(lt_metric_auprc = metric_auprc, lt_metric_odds_ratio = metric_odds_ratio), by = "method_id") %>%
  left_join(data_st$per_dataset %>% rename(st_dataset_mouse_brain_atlas = dataset_mouse_brain_atlas), by = "method_id") %>%
  left_join(data_st$per_metric %>% rename(st_metric_auprc = metric_auprc, st_metric_odds_ratio = metric_odds_ratio), by = "method_id") %>%
  mutate(mean_score = (lt_dataset_tnbc_data + st_dataset_mouse_brain_atlas) / 2) %>%
  select(method_id, method_name, mean_score, everything()) %>%
  arrange(desc(mean_score))

# add ranks
for (cn in colnames(data)) {
  if (is.numeric(data[[cn]])) {
    data[[paste0(cn, "_rank")]] <- rank(data[[cn]], ties.method = "min")
  }
}

# determine column info
column_info <-
  bind_rows(
    tribble(
      ~id, ~id_color, ~name, ~group, ~geom, ~palette,
      "method_name", NA_character_, "Name", "method", "text", NA_character_,
      "mean_score", "mean_score_rank", "Mean score", "mean", "bar", "mean",
      "lt_dataset_tnbc_data", "lt_dataset_tnbc_data_rank", "TNBC atlas", "ltd", "funkyrect", "dataset",
      "lt_metric_auprc", "lt_metric_auprc_rank", "PR-AUC", "ltm", "funkyrect", "metric",
      "lt_metric_odds_ratio", "lt_metric_odds_ratio_rank", "Odds Ratio", "ltm", "funkyrect", "metric",
      "st_dataset_mouse_brain_atlas", "st_dataset_mouse_brain_atlas_rank", "Mouse brain atlas", "std", "funkyrect", "dataset",
      "st_metric_auprc", "st_metric_auprc_rank", "PR-AUC", "stm", "funkyrect", "metric",
      "st_metric_odds_ratio", "st_metric_odds_ratio_rank", "Odds Ratio", "stm", "funkyrect", "metric"
    )
  ) %>%
  mutate(
    options = map2(id, geom, function(id, geom) {
      if (id == "method_name") {
        list(width = 15, hjust = 0)
      } else if (id == "is_baseline") {
        list(width = 1)
      } else if (geom == "bar") {
        list(width = 4)
      } else {
        list()
      }
    }
  )
)

# determine column groups
column_groups <- tribble(
  ~Category, ~group, ~palette,
  "", "method", NA_character_,
  "Overall", "mean", "mean",
  "Ligand-Target", "ltd", "mean",
  "Ligand-Target", "ltm", "mean",
  "Source-Target", "std", "mean",
  "Source-Target", "stm", "mean"
)

# determine palettes
palettes <- list(
  mean = "Greys",
  dataset = "Blues",
  metric = "Reds"
)

# determine legends
legends <- list(
  list(
    title = "Rank",
    geom = "rect",
    palette = "mean",
    labels = c("N", "", "", "", "1"),
    size = 1
  ),
  list(
    title = "Score",
    geom = "funkyrect",
    color = "darkgray",
    labels = c("min", "", "", "", "", "", "", "", "", "", "max"),
    size = seq(0, 1, by = .1),
    label_hjust = c(0, rep(0.5, 9), 1)
  ),
  list(
    title = "Type",
    geom = "funkyrect",
    labels = c("Mean", "Dataset", "Metric"),
    color = c("#7a7a7a", "#1b6aaf", "#eb372a"),
    size = 1
  ),
  list(
    geom = "funkyrect",
    palette = "dataset",
    enabled = FALSE
  ),
  list(
    geom = "funkyrect",
    palette = "metric",
    enabled = FALSE
  )
)

# create figure
g_all <- funky_heatmap(
  data = data,
  column_info = column_info %>% filter(id %in% colnames(data)),
  column_groups = column_groups,
  palettes = palettes,
  legends = legends,
  add_abc = FALSE,
  scale_column = TRUE,
  position_args = position_arguments(
    col_annot_offset = 3.5,
    expand_xmax = max(str_length(tail(column_info$name, 4))) / 5
  )
)

g_all

ggsave(paste0(figure_prefix, ".pdf"), g_all, width = g_all$width, height = g_all$height)

library(openxlsx2)

wb <-
  wb_workbook(title = "Data for Figure 2c") |>
  wb_add_worksheet("Data") |>
  wb_add_data("Data", data, col_names = TRUE) |>
  wb_add_worksheet("Column info") |>
  wb_add_data("Column info", column_info, col_names = TRUE) |>
  wb_add_worksheet("Column groups") |>
  wb_add_data("Column groups", column_groups, col_names = TRUE) |>
  wb_add_worksheet("Palettes") |>
  wb_add_data("Palettes", enframe(unlist(palettes)), col_names = TRUE) |>
  wb_add_worksheet("Legends") |>
  wb_add_data("Legends", legends %>% map_dfr(~ as_tibble(map(., function(x) paste(x, collapse = ",")))), col_names = TRUE) |>
  wb_save(paste0(figure_prefix, "_data.xlsx"))
