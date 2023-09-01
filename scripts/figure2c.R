library(tidyverse)
library(funkyheatmap)

task_info_jsons <- list.files("website/results/", pattern = "task_info.json", recursive = TRUE, full.names = TRUE)

source("scripts/_helper_read_task_results.R")

data_lt <- read_task_results("website/results/cell_cell_communication_ligand_target/data")
data_st <- read_task_results("website/results/cell_cell_communication_source_target/data")

figure_path <- paste0("figures/figure2/fig2c.pdf")
if (!dir.exists(dirname(figure_path))) {
  dir.create(dirname(figure_path), recursive = TRUE, showWarnings = FALSE)
}

# load scores
data <-
  data_lt$method_info %>%
    filter(!is_baseline) %>%
    select(method_id, method_name) %>%
    left_join(data_lt$per_dataset %>% rename(lt_dataset_tnbc_data = dataset_tnbc_data), by = "method_id") %>%
    left_join(data_lt$per_metric %>% rename(lt_metric_auprc = metric_auprc, lt_metric_odds_ratio = metric_odds_ratio), by = "method_id") %>%
    left_join(data_st$per_dataset %>% rename(st_dataset_mouse_brain_atlas = dataset_mouse_brain_atlas), by = "method_id") %>%
    left_join(data_st$per_metric %>% rename(st_metric_auprc = metric_auprc, st_metric_odds_ratio = metric_odds_ratio), by = "method_id")

# add ranks
for (cn in colnames(data)) {
  if (is.numeric(data[[cn]])) {
    data[[paste0(cn, "_rank")]] <- rank(data[[cn]], ties.method = "min")
  }
}

# add mean score
data <- data %>%
    mutate(mean_score = (lt_dataset_tnbc_data + st_dataset_mouse_brain_atlas) / 2) %>%
    select(method_id, method_name, mean_score, everything()) %>%
    arrange(desc(mean_score))

# determine column info
column_info <-
  bind_rows(
    tribble(
      ~id, ~id_color, ~name, ~group, ~geom, ~palette,
      "method_name", NA_character_, "Name", "method", "text", NA_character_,
      "mean_score", NA_character_, "Score", "mean", "bar", "mean",
      "lt_dataset_tnbc_data", "lt_dataset_tnbc_data_rank", "TNBC atlas", "ltd", "funkyrect", "lt",
      "lt_metric_auprc", "lt_metric_auprc_rank", "PR-AUC", "ltm", "funkyrect", "st",
      "lt_metric_odds_ratio", "lt_metric_odds_ratio_rank", "Odds Ratio", "ltm", "funkyrect", "st",
      "st_dataset_mouse_brain_atlas", "st_dataset_mouse_brain_atlas_rank", "Mouse brain atlas", "std", "funkyrect", "lt",
      "st_metric_auprc", "st_metric_auprc_rank", "PR-AUC", "stm", "funkyrect", "st",
      "st_metric_odds_ratio", "st_metric_odds_ratio_rank", "Odds Ratio", "stm", "funkyrect", "st"
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
  lt = "Blues",
  st = "Reds"
)

# determine legends
legends <- list(
  list(
    title = "Ligand-Target rank",
    geom = "funkyrect",
    palette = "lt",
    labels = c("N", "", "", "", "1"),
    size = 1
  ),
  list(
    title = "Source-Target rank",
    geom = "funkyrect",
    palette = "st",
    labels = c("N", "", "", "", "1"),
    size = 1
  ),
  list(
    title = "Score",
    geom = "funkyrect",
    color = "darkgray",
    labels = c("0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1"),
    size = seq(0, 1, by = .1)
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

ggsave(figure_path, g_all, width = g_all$width, height = g_all$height)
