# Run every feed; a failed feed is recorded and the others still run
suppressPackageStartupMessages(library(tidyverse))

feeds <- c(
  markets = "R/03_feed_markets.R",
  pageviews = "R/04_feed_pageviews.R",
  polls = "R/05_feed_polls.R",
  fec = "R/06_feed_fec.R"
)

status_path <- "data/feed_status.csv"
old <- if (file.exists(status_path)) {
  read_csv(status_path, col_types = cols(.default = col_character()))
} else {
  tibble(feed = character(), last_success = character())
}

status <- imap_dfr(feeds, \(script, feed) {
  cat("\n==", feed, "==\n")
  message <- tryCatch(
    {
      source(script, local = new.env())
      "ok"
    },
    error = \(e) conditionMessage(e)
  )
  previous <- old$last_success[old$feed == feed]
  tibble(
    feed = feed,
    last_attempt = format(Sys.Date()),
    last_success = if (message == "ok") format(Sys.Date()) else c(previous, NA_character_)[1],
    message = str_sub(str_squish(message), 1, 200)
  )
})

write_csv(status, status_path)
cat("\n")
print(status)
