
library(tidyverse)
library(funkyheatmap)

task_info_jsons <- list.files("website/results/", pattern = "task_info.json", recursive = TRUE, full.names = TRUE)

walk(task_info_jsons, function(task_info_json) {
  data_dir <- dirname(task_info_json)

  tryCatch({
    # read task info
    task_info <- jsonlite::read_json(paste0(data_dir, "/task_info.json"))
    task_info$task_description <- task_info$task_description %||% NA_character_

    cat("Processing ", task_info$task_id, "\n", sep = "")

    figure_path <- paste0("figures/supnote2_figures/", task_info$task_id, "/plot.pdf")
    if (!dir.exists(dirname(figure_path))) {
      dir.create(dirname(figure_path), recursive = TRUE, showWarnings = FALSE)
    }

    `%|%` <- function(x, y) {
      ifelse(is.na(x), y, x)
    }

    method_info <- jsonlite::read_json(paste0(data_dir, "/method_info.json"), simplifyVector = TRUE)
    metric_info <- jsonlite::read_json(paste0(data_dir, "/metric_info.json"), simplifyVector = TRUE)
    dataset_info <- jsonlite::read_json(paste0(data_dir, "/dataset_info.json"), simplifyVector = TRUE)
    results <- jsonlite::read_json(paste0(data_dir, "/results.json"), simplifyVector = TRUE)
    qc <- jsonlite::read_json(paste0(data_dir, "/quality_control.json"), simplifyVector = TRUE)

    for (col in c("method_summary")) {
      method_info[[col]] <- method_info[[col]] %||% NA_character_
    }
    for (col in c("metric_summary")) {
      metric_info[[col]] <- metric_info[[col]] %||% NA_character_
    }
    for (col in c("dataset_summary")) {
      dataset_info[[col]] <- dataset_info[[col]] %||% NA_character_
    }

    num_methods <- sum(!method_info$is_baseline)
    num_baselines <- sum(method_info$is_baseline)
    num_datasets <- nrow(dataset_info)
    num_metrics <- nrow(metric_info)

    results_long <-
      inner_join(
        results %>%
          unnest(metric_values) %>%
          gather(metric_id, value, any_of(metric_info$metric_id)) %>%
          mutate(value = ifelse(is.na(value), NA_real_, value)) %>%
          select(method_id, dataset_id, metric_id, value),
        results %>%
          unnest(scaled_scores) %>%
          gather(metric_id, score, any_of(metric_info$metric_id)) %>%
          mutate(score = ifelse(is.na(score), NA_real_, score)) %>%
          select(method_id, dataset_id, metric_id, score),
        by = c("method_id", "dataset_id", "metric_id")
      ) %>%
      left_join(method_info %>% select(method_id, is_baseline), "method_id")

    overall_ranking <- results_long %>%
      group_by(method_id) %>%
      summarise(mean_score = mean(score %|% 0)) %>%
      arrange(desc(mean_score))

    # order by ranking
    results_long$method_id <- factor(results_long$method_id, levels = rev(overall_ranking$method_id))
    results$method_id <- factor(results$method_id, levels = rev(overall_ranking$method_id))
    method_info$method_id <- factor(method_info$method_id, levels = rev(overall_ranking$method_id))


    overall <- results_long %>%
      group_by(method_id) %>%
      summarise(mean_score = mean(score %|% 0), .groups = "drop") %>%
      mutate(mean_score = pmin(1, pmax(mean_score, 0))) %>%
      arrange(mean_score)
    per_dataset <- results_long %>%
      group_by(method_id, dataset_id) %>%
      summarise(score = mean(score %|% 0), .groups = "drop") %>%
      mutate(dataset_id = paste0("dataset_", dataset_id)) %>%
      mutate(score = pmin(1, pmax(score, 0))) %>%
      spread(dataset_id, score)
    per_metric <- results_long %>%
      group_by(method_id, metric_id) %>%
      summarise(score = mean(score %|% 0), .groups = "drop") %>%
      mutate(metric_id = paste0("metric_", metric_id)) %>%
      mutate(score = pmin(1, pmax(score, 0))) %>%
      spread(metric_id, score)

    summary_all <-
      method_info %>%
      filter(!is_baseline) %>%
      select(method_id, method_name) %>%
      left_join(overall, by = "method_id") %>%
      left_join(per_dataset, by = "method_id") %>%
      left_join(per_metric, by = "method_id") %>%
      arrange(desc(method_id))

    column_info <-
      bind_rows(
        tribble(
          ~id, ~name, ~group, ~geom,
          "method_name", "Name", "method", "text",
          "mean_score", "Score", "mean", "bar",
        ),
        dataset_info %>% transmute(
          id = paste0("dataset_", dataset_id),
          name = dataset_name,
          group = "dataset",
          geom = "funkyrect"
        ),
        metric_info %>% transmute(
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
      "Overall", "mean", "mean"
    )
    column_groups <- column_groups %>%
      add_row(Category = "Dataset", group = "dataset", palette = "dataset")

    column_groups <- column_groups %>%
      add_row(Category = "Metric", group = "metric", palette = "metric")

    palettes <- list(
      mean = "Greys",
      dataset = "Blues",
      metric = "Reds"
    )

    g_all <- funky_heatmap(
      data = summary_all,
      column_info = column_info %>% filter(id %in% colnames(summary_all)),
      column_groups = column_groups,
      palettes = palettes,
      # determine xmax expand heuristically
      expand = c(xmax = max(str_length(tail(column_info$name, 4))) / 5),
      # determine offset heuristically
      col_annot_offset = max(str_length(column_info$name)) / 5,
      add_abc = FALSE,
      scale_column = TRUE
    )

    ggsave(figure_path, g_all, width = g_all$width, height = g_all$height)
  }, error = function(e) {
    cat("Error: ", e$message, "\n", sep = "")
  })
})