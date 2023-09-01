
library(tidyverse)
library(funkyheatmap)

task_info_jsons <- list.files("website/results", pattern = "task_info.json", recursive = TRUE, full.names = TRUE)

source("scripts/_helper_read_task_results.R")

walk(task_info_jsons, function(task_info_json) {
  cat("Processing ", basename(dirname(dirname(task_info_json))), "\n", sep = "")

  tryCatch({
    task_data <- read_task_results(dirname(task_info_json))

    figure_path <- paste0("figures/supnote2_figures/", task_info$task_id, "/plot.pdf")
    if (!dir.exists(dirname(figure_path))) {
      dir.create(dirname(figure_path), recursive = TRUE, showWarnings = FALSE)
    }

    summary_all <-
      task_data$method_info %>%
        filter(!is_baseline) %>%
        select(method_id, method_name) %>%
        left_join(task_data$overall, by = "method_id") %>%
        left_join(task_data$per_dataset, by = "method_id") %>%
        left_join(task_data$per_metric, by = "method_id") %>%
        arrange(desc(method_id))

    column_info <-
      bind_rows(
        tribble(
          ~id, ~name, ~group, ~geom,
          "method_name", "Name", "method", "text",
          "mean_score", "Score", "mean", "bar",
        ),
        task_data$dataset_info %>% transmute(
          id = paste0("dataset_", dataset_id),
          name = dataset_name,
          group = "dataset",
          geom = "funkyrect"
        ),
        task_data$metric_info %>% transmute(
          id = paste0("metric_", metric_id),
          name = metric_name,
          group = "metric",
          geom = "funkyrect"
        )
      ) %>%
      mutate(
        palette = ifelse(group %in% c("mean", "dataset", "metric"), group, NA_character_),
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
      "Dataset", "dataset", "dataset",
      "Metric", "metric", "metric"
    )

    palettes <- list(
      mean = "Greys",
      dataset = "Blues",
      metric = "Reds"
    )

    legends <- NULL # TODO

    g_all <- funky_heatmap(
      data = summary_all,
      column_info = column_info %>% filter(id %in% colnames(summary_all)),
      column_groups = column_groups,
      palettes = palettes,
      legends = legends,
      add_abc = FALSE,
      scale_column = TRUE,
      # determine position args heuristically
      position_args = position_arguments(
        col_annot_offset = max(str_length(column_info$name)) / 5,
        expand_xmax = max(str_length(tail(column_info$name, 4))) / 5
      )
    )

    ggsave(figure_path, g_all, width = g_all$width, height = g_all$height)
  }, error = function(e) {
    cat("Error: ", e$message, "\n", sep = "")
  })
})
