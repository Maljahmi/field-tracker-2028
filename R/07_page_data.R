# Page data: every number the page shows, one row per candidate
suppressPackageStartupMessages(library(tidyverse))

cand <- read_csv("data/candidates.csv", show_col_types = FALSE)
markets <- read_csv("data/markets.csv", show_col_types = FALSE)
pageviews <- read_csv("data/pageviews.csv", show_col_types = FALSE)
inclusion <- read_csv("data/poll_inclusion.csv", show_col_types = FALSE)
events <- read_csv("data/events.csv", col_types = cols(.default = col_character()))
feed_status <- read_csv("data/feed_status.csv", col_types = cols(.default = col_character()))
fec <- if (file.exists("data/fec.csv")) read_csv("data/fec.csv", show_col_types = FALSE)

as_of <- max(markets$date)

# nomination odds: average of the two markets each day
nominee <- markets |>
  filter(market == "nominee") |>
  summarise(price = mean(price), .by = c(candidate_id, date)) |>
  arrange(candidate_id, date)

# latest value on or before a date
value_at <- function(data, day) {
  data |>
    filter(date <= day) |>
    slice_max(date, n = 1, by = candidate_id) |>
    select(candidate_id, price)
}

price_now <- value_at(nominee, as_of) |> rename(price = price)
price_week <- value_at(nominee, as_of - 7) |> rename(price_week_ago = price)

# 30-day series for the sparkline
spark <- nominee |>
  filter(date > as_of - 30) |>
  summarise(spark_dates = list(date), spark_prices = list(price), .by = candidate_id)

# latest price per source and market
by_source <- markets |>
  slice_max(date, n = 1, by = c(candidate_id, source, market)) |>
  mutate(col = paste(source, market, sep = "_")) |>
  select(candidate_id, col, price) |>
  pivot_wider(names_from = col, values_from = price)

# attention: average daily views, last 7 days against the 7 before
views_end <- max(pageviews$date)
views <- pageviews |>
  summarise(
    views_7d = mean(views[date > views_end - 7]),
    views_prior_7d = mean(views[date > views_end - 14 & date <= views_end - 7]),
    .by = candidate_id
  ) |>
  mutate(views_change = views_7d / views_prior_7d - 1)

# polls: latest dated summary
polls <- inclusion |>
  slice_max(date, n = 1, by = candidate_id) |>
  select(candidate_id, n_polls_30d, avg_pct_30d)

page_rows <- cand |>
  left_join(price_now, by = "candidate_id") |>
  left_join(price_week, by = "candidate_id") |>
  left_join(spark, by = "candidate_id") |>
  left_join(by_source, by = "candidate_id") |>
  left_join(views, by = "candidate_id") |>
  left_join(polls, by = "candidate_id") |>
  mutate(change_7d = price - price_week_ago) |>
  arrange(desc(price), desc(n_polls_30d), name)

# header numbers
mover <- page_rows |>
  filter(!is.na(change_7d)) |>
  slice_max(abs(change_7d), n = 1, with_ties = FALSE)

feed_labels <- c(
  markets = "Market odds", pageviews = "Wikipedia views",
  polls = "Polls", fec = "FEC filings"
)
feeds <- feed_status |>
  mutate(
    label = feed_labels[feed],
    stale = is.na(last_success) | as.Date(last_success) < as.Date(last_attempt)
  )
