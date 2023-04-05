library(tidyverse)
library(funkyheatmap)

task_info_jsons <- list.files("website/results/", pattern = "task_info.json", recursive = TRUE, full.names = TRUE)

source("scripts/_helper_read_task_results.R")

data_lt <- read_task_results("website/results/cell_cell_communication_ligand_target/data")
data_st <- read_task_results("website/results/cell_cell_communication_source_target/data")

figure_path <- paste0("figures/figure2c/plot.pdf")
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
      ~id, ~name, ~group, ~geom,
      "method_name", "Name", "method", "text",
      "mean_score", "Score", "mean", "bar",
      "lt_dataset_tnbc_data", "Dataset: TNBC atlas", "lt", "funkyrect",
      "lt_metric_auprc", "Metric: PR-AUC", "lt", "funkyrect",
      "lt_metric_odds_ratio", "Metric: Odds Ratio", "lt", "funkyrect",
      "st_dataset_mouse_brain_atlas", "Dataset: Mouse brain atlas", "st", "funkyrect",
      "st_metric_auprc", "Metric: PR-AUC", "st", "funkyrect",
      "st_metric_odds_ratio", "Metric: Odds Ratio", "st", "funkyrect",
    )
  ) %>%
  mutate(
    palette = ifelse(group %in% c("mean", "lt", "st"), group, NA_character_),
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
  "Ligand-Target", "lt", "lt",
  "Source-Target", "st", "st"
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