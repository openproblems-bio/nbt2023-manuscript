library(tidyverse)

# create benchmark data object
benchmarks <- list(
  list(
    name = "Tran et al. 2020",
    repository = "JinmiaoChenLab/Batch-effect-removal-benchmarking",
    methods = c("BBKNN", "ComBat", "fastMNN", "Harmony", "Liger", "Limma", "MDD-ResNet", "MNN", "Scanorama", "scGen", "scMerge", "Seurat 2", "Seurat 3", "ZINB-WaVE"),
    metrics = c("Batch ASW", "kBET", "ARI", "LISI combined"),
    dates = tribble(
      ~name, ~date,
      "Publication", "2020-01-16"
    )
  ),
  list(
    name = "Mereu et al.",
    repository = "ati-lz/HCA_Benchmarking",
    methods = c("Harmony", "scMerge", "Seurat 2"),
    metrics = c("Clustering Accuracy"),
    dates = tribble(
      ~name, ~date,
      "Preprint", "2019-05-13",
      "Publication", "2020-04-06"
    )
  ),
  list(
    name = "Chazarra-Gil et al.",
    repository = "cellgeni/batchbench",
    methods = c("BBKNN", "ComBat", "fastMNN", "Harmony", "MNN", "Scanorama", "Seurat 2"),
    metrics = c("Batch entropy", "Cell type entropy"),
    dates = tribble(
      ~name, ~date,
      "Preprint", "2019-05-13",
      "Publication", "2020-04-06"
    )
  ),
  list(
    name = "Luecken et al.",
    repository = c("theislab/scib", "theislab/scib-reproducibility"),
    methods = c("KKBNN", "ComBat", "DESC", "fastMNN", "Harmony", "Liger", "MNN", "Saucie", "Scanorama", "scanvi", "scGen", "scvi", "Seurat 2", "Seurat 3", "trVAE"),
    metrics = c("PCR batch", "Batch ASW", "Graph iLISI", "Graph connectivity", "kBET", "NMI", "ARI", "Cell type ASW", "Isolated label F1", "Isolated label silhouette", "Graph cLISI", "Cell cycle conservation", "HVG conservation", "Trajectory conservation"),
    dates = tribble(
      ~name, ~date,
      "Preprint", "2020-05-27",
      "Publication", "2021-12-23"
    )
  )
)

yaml::write_yaml(benchmarks, "data/benchmarks.yaml")

# fetch git commits
benchmark_repos <- map_df(benchmarks, function(benchmark) {
  tibble(
    repo = benchmark$repository,
    benchmark_name = benchmark$name
  )
})

benchmark_git_commits <- map_df(unique(unlist(map(benchmarks, "repository"))), function(repo) {
  page <- 1
  commit_info <- NULL
  while(TRUE) {
    # Build the URL for the GitHub API
    url <- paste0('https://api.github.com/repos/', repo, '/commits?page=', page)
    cat("Fetching repo ", repo, " page ", page, "\n", sep = "")

    # Request the data
    response <- httr::GET(url, httr::add_headers(Authorization = paste('token', Sys.getenv("GITHUB_PAT"))))
    
    # Check if the request was successful
    if (response$status_code != 200) {
      stop("Failed to fetch the data. Please make sure the repository and user names are correct.")
    }
    
    # Parse the JSON response
    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)

    # If data is empty, then we've retrieved all pages
    if(length(data) == 0) {
      break
    }
    
    # Extract commit messages and dates
    commit_info_page <- map_df(data, function(x) {
      tibble(
        repo = repo,
        message = x$commit$message,
        author = x$commit$author$name,
        date = lubridate::ymd_hms(x$commit$committer$date)
      )
    })

    # Append the commit info from this page to the list
    commit_info <- bind_rows(commit_info, commit_info_page)

    # Increment page number
    page <- page + 1
  }

  commit_info
})

benchmark_commits <- benchmark_git_commits %>%
  mutate(message = gsub("[\r\n\t ]+", " ", message)) %>%
  left_join(benchmark_repos, by = "repo")

readr::write_tsv(benchmark_commits, "data/benchmark_commits.tsv")
