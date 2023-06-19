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

summary_all <-
  data_lt$method_info %>%
    filter(!is_baseline) %>%
    select(method_id, method_name) %>%
    left_join(data_lt$per_dataset %>% rename(lt_dataset_tnbc_data = dataset_tnbc_data), by = "method_id") %>%
    left_join(data_lt$per_metric %>% rename(lt_metric_auprc = metric_auprc, lt_metric_odds_ratio = metric_odds_ratio), by = "method_id") %>%
    left_join(data_st$per_dataset %>% rename(st_dataset_mouse_brain_atlas = dataset_mouse_brain_atlas), by = "method_id") %>%
    left_join(data_st$per_metric %>% rename(st_metric_auprc = metric_auprc, st_metric_odds_ratio = metric_odds_ratio), by = "method_id") %>%
    mutate(mean_score = (lt_dataset_tnbc_data + st_dataset_mouse_brain_atlas) / 2) %>%
    select(method_id, method_name, mean_score, everything()) %>%
    arrange(desc(mean_score))

column_info <-
  bind_rows(
    tribble(
      ~id, ~name, ~group, ~geom, ~palette,
      "method_name", "Name", "method", "text", NA_character_,
      "mean_score", "Score", "mean", "bar", "mean",
      "lt_dataset_tnbc_data", "TNBC atlas", "ltd", "funkyrect", "lt",
      "lt_metric_auprc", "PR-AUC", "ltm", "funkyrect", "st",
      "lt_metric_odds_ratio", "Odds Ratio", "ltm", "funkyrect", "st",
      "st_dataset_mouse_brain_atlas", "Mouse brain atlas", "std", "funkyrect", "lt",
      "st_metric_auprc", "PR-AUC", "stm", "funkyrect", "st",
      "st_metric_odds_ratio", "Odds Ratio", "stm", "funkyrect", "st"
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
column_groups <- tribble(
  ~Category, ~group, ~palette,
  "", "method", NA_character_,
  "Overall", "mean", "mean",
  "Ligand-Target", "ltd", "mean",
  "Ligand-Target", "ltm", "mean",
  "Source-Target", "std", "mean",
  "Source-Target", "stm", "mean"
)

palettes <- list(
  mean = "Greys",
  lt = "Blues",
  st = "Reds"
)

g_all <- funky_heatmap(
  data = summary_all,
  column_info = column_info %>% filter(id %in% colnames(summary_all)),
  column_groups = column_groups,
  palettes = palettes,
  # determine xmax expand heuristically
  expand = c(xmax = max(str_length(tail(column_info$name, 4))) / 5),
  col_annot_offset = 3.5,
  add_abc = FALSE,
  scale_column = TRUE
)

g_all

ggsave(figure_path, g_all, width = g_all$width, height = g_all$height)