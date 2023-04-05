`%|%` <- function(x, y) {
  ifelse(is.na(x), y, x)
}

read_task_results <- function(task_path) {
  task_info <- jsonlite::read_json(paste0(task_path, "/task_info.json"))
  task_info$task_description <- task_info$task_description %||% NA_character_

  method_info <- jsonlite::read_json(paste0(task_path, "/method_info.json"), simplifyVector = TRUE)
  metric_info <- jsonlite::read_json(paste0(task_path, "/metric_info.json"), simplifyVector = TRUE)
  dataset_info <- jsonlite::read_json(paste0(task_path, "/dataset_info.json"), simplifyVector = TRUE)
  results <- jsonlite::read_json(paste0(task_path, "/results.json"), simplifyVector = TRUE)
  qc <- jsonlite::read_json(paste0(task_path, "/quality_control.json"), simplifyVector = TRUE)

  for (col in c("method_summary")) {
    method_info[[col]] <- method_info[[col]] %||% NA_character_
  }
  for (col in c("metric_summary")) {
    metric_info[[col]] <- metric_info[[col]] %||% NA_character_
  }
  for (col in c("dataset_summary")) {
    dataset_info[[col]] <- dataset_info[[col]] %||% NA_character_
  }

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
    
  # create summary statistics
  overall <- results_long %>%
    group_by(method_id) %>%
    summarise(mean_score = mean(score %|% 0), .groups = "drop") %>%
    # mutate(mean_score = pmin(1, pmax(mean_score, 0))) %>%
    arrange(mean_score)
  per_dataset <- results_long %>%
    group_by(method_id, dataset_id) %>%
    summarise(score = mean(score %|% 0), .groups = "drop") %>%
    mutate(dataset_id = paste0("dataset_", dataset_id)) %>%
    # mutate(score = pmin(1, pmax(score, 0))) %>%
    spread(dataset_id, score)
  per_metric <- results_long %>%
    group_by(method_id, metric_id) %>%
    summarise(score = mean(score %|% 0), .groups = "drop") %>%
    mutate(metric_id = paste0("metric_", metric_id)) %>%
    # mutate(score = pmin(1, pmax(score, 0))) %>%
    spread(metric_id, score)

  list(
    task_info = task_info,
    dataset_info = dataset_info,
    method_info = method_info,
    metric_info = metric_info,
    qc = qc,
    results = results,
    results_long = results_long,
    overall = overall,
    per_dataset = per_dataset,
    per_metric = per_metric
  )
}