library(tidyverse)

# create benchmark data object
benchmarks <- list(
  list(
    name = "Tran et al.",
    repository = "JinmiaoChenLab/Batch-effect-removal-benchmarking",
    methods = c("BBKNN", "ComBat", "fastMNN", "Harmony", "Liger", "Limma", "MDD-ResNet", "mnnCorrect", "Scanorama", "scGen", "scMerge", "Seurat v2", "Seurat v3", "ZINB-WaVE"),
    metrics = c("Batch ASW", "kBET", "ARI", "LISI combined"),
    dates = tribble(
      ~name, ~date,
      "Publication", "2020-01-16"
    )
  ),
  list(
    name = "Mereu et al.",
    repository = "ati-lz/HCA_Benchmarking",
    methods = c("Harmony", "scMerge", "Seurat v2"),
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
    methods = c("BBKNN", "ComBat", "fastMNN", "Harmony", "mnnCorrect", "Scanorama", "Seurat v2"),
    metrics = c("Batch entropy", "Cell type entropy"),
    dates = tribble(
      ~name, ~date,
      "Preprint", "2019-05-13",
      "Publication", "2020-04-06"
    )
  ),
  list(
    name = "Luecken et al.",
    repository = c("theislab/scib", "theislab/scib-reproducibility", "theislab/scib-pipeline"),
    methods = c("BBKNN", "ComBat", "DESC", "fastMNN", "Harmony", "Liger", "mnnCorrect", "Saucie", "Scanorama", "scanVI", "scGen", "scVI", "Seurat v2", "Seurat v3", "trVAE"),
    metrics = c("PCR batch", "Batch ASW", "Graph iLISI", "Graph connectivity", "kBET", "NMI", "ARI", "Cell type ASW", "Isolated label F1", "Isolated label silhouette", "Graph cLISI", "Cell cycle conservation", "HVG conservation", "Trajectory conservation"),
    dates = tribble(
      ~name, ~date,
      "Preprint", "2020-05-27",
      "Publication", "2021-12-23"
    )
  )
)

yaml::write_yaml(benchmarks, "data/fig1a_benchmarks.yaml")

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

readr::write_tsv(benchmark_commits, "data/fig1a_benchmark_commits.tsv")

method_info <- tribble(
  ~method, ~date,
  "BBKNN", "2018-08-22",
  "ComBat", "2006-04-21",
  "fastMNN", "2017-07-18",
  "Harmony", "2018-11-05",
  "Liger", "2022-04-08",
  "Limma", "2015-04-20",
  "MDD-ResNet", "2018-01-10",
  "mnnCorrect", "2017-07-18",
  "Scanorama", "2018-07-17",
  "scGen", "2018-12-14",
  "scMerge", "2018-09-12",
  "Seurat v2", "2018-11-02",
  "Seurat v3", "2017-07-18",
  "ZINB-WaVE", "2017-11-02",
  "DESC", "2019-01-25",
  "Saucie", "2019-01-03",
  "scanVI", "2019-01-29",
  "scVI", "2018-11-30",
  "trVAE", "2019-10-04"
)

yaml::write_yaml(method_info, "data/fig1a_method_info.yaml")
