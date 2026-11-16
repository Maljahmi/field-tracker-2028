# Attention feed: daily Wikipedia pageviews per candidate
source("R/helpers.R")

cand <- load_candidates()
out_path <- "data/pageviews.csv"

# first run backfills; later runs refresh the last days so gaps heal
days_back <- if (file.exists(out_path)) 10 else 120
start <- format(Sys.Date() - days_back, "%Y%m%d")
end <- format(Sys.Date() - 1, "%Y%m%d")

pageviews <- function(title) {
  article <- URLencode(str_replace_all(title, " ", "_"), reserved = TRUE)
  res <- get_json(
    paste0(
      "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
      "en.wikipedia/all-access/user/", article, "/daily/", start, "/", end
    ),
    rate = 5
  )
  tibble(
    date = as.Date(str_sub(map_chr(res$items, "timestamp"), 1, 8), format = "%Y%m%d"),
    views = map_dbl(res$items, "views")
  )
}

rows <- map2_dfr(cand$candidate_id, cand$wikipedia_title, \(id, title) {
  tryCatch(pageviews(title), error = \(e) tibble(date = as.Date(character()), views = numeric())) |>
    mutate(candidate_id = id)
}) |>
  select(date, candidate_id, views)

if (nrow(rows) == 0) stop("No pageviews returned")
upsert_csv(rows, out_path, c("date", "candidate_id"))

cat("Pageview rows written:", nrow(rows), "| candidates:", n_distinct(rows$candidate_id),
    "| latest day:", format(max(rows$date)), "\n")
cat("Candidates with no pageviews:", paste(setdiff(cand$candidate_id, rows$candidate_id), collapse = ", "), "\n")
