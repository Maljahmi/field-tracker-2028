# Candidate pool: everyone named in a 2028 primary poll or listed on a 2028 market
source("R/helpers.R")

# inclusion rule
min_price <- 0.01      # nominee price on either market
min_polls <- 3         # polls naming the person in the recent window
recent_days <- 365     # window for counting polls

# polls: one row per person per poll
polls_raw <- fetch_json(
  "https://api.votehub.com/polls?poll_type=presidential-primary",
  "votehub_primary_polls"
)

polls <- map_dfr(polls_raw, \(p) {
  tibble(
    poll_id = p$id,
    subject = p$subject,
    end_date = as.Date(p$end_date),
    name = map_chr(p$answers, "choice"),
    pct = map_dbl(p$answers, \(a) as.numeric(a$pct %||% NA))
  )
}) |>
  mutate(
    party = case_when(
      str_detect(subject, "Democratic") ~ "D",
      str_detect(subject, "Republican") ~ "R"
    ),
    key = name_key(name)
  ) |>
  filter(str_detect(subject, "2028"))

# drop non-person answers
not_people <- c(
  "undecided", "other", "someone else", "none", "none of the above",
  "not sure", "dont know", "refused", "would not vote", "unsure",
  "no opinion", "none of these", "another candidate", "other/undecided"
)
polls <- polls |> filter(!key %in% not_people)

last_poll_date <- max(polls$end_date)

poll_summary <- polls |>
  group_by(key, party) |>
  summarise(
    poll_name = first(name),
    n_polls_all = n_distinct(poll_id),
    n_polls_recent = n_distinct(poll_id[end_date > last_poll_date - recent_days]),
    avg_pct_recent = round(mean(pct[end_date > last_poll_date - recent_days], na.rm = TRUE), 1),
    last_polled = max(end_date),
    .groups = "drop"
  )

# Polymarket: active contracts, bid-ask midpoint
polymarket_event <- function(slug, name) {
  ev <- fetch_json(paste0("https://gamma-api.polymarket.com/events?slug=", slug), name)
  map_dfr(ev[[1]]$markets, \(m) {
    tibble(
      name = m$groupItemTitle %||% NA_character_,
      active = isTRUE(m$active) && !isTRUE(m$closed),
      bid = as.numeric(m$bestBid %||% NA),
      ask = as.numeric(m$bestAsk %||% NA),
      last = as.numeric(m$lastTradePrice %||% NA)
    )
  }) |>
    filter(active, !is.na(name)) |>
    mutate(
      price = if_else(!is.na(bid) & !is.na(ask), (bid + ask) / 2, last),
      key = name_key(name)
    ) |>
    select(key, name, price)
}

pm_dem <- polymarket_event("democratic-presidential-nominee-2028", "polymarket_nominee_dem") |>
  mutate(party = "D")
pm_rep <- polymarket_event("republican-presidential-nominee-2028", "polymarket_nominee_rep") |>
  mutate(party = "R")
pm_run_2027 <- polymarket_event(
  "who-will-announce-presidential-run-before-2027", "polymarket_announce_2027"
)
pm_run_2028 <- polymarket_event(
  "who-will-announce-presidential-run-before-2028-20260803142755224", "polymarket_announce_2028"
)

# Kalshi: active contracts, bid-ask midpoint
kalshi_event <- function(series, name) {
  ev <- fetch_json(
    paste0(
      "https://api.elections.kalshi.com/trade-api/v2/events?series_ticker=",
      series, "&with_nested_markets=true"
    ),
    name
  )
  map_dfr(ev$events[[1]]$markets, \(m) {
    tibble(
      name = m$yes_sub_title,
      status = m$status,
      bid = as.numeric(m$yes_bid_dollars),
      ask = as.numeric(m$yes_ask_dollars),
      last = as.numeric(m$last_price_dollars)
    )
  }) |>
    filter(status == "active") |>
    mutate(
      price = if_else(!is.na(bid) & !is.na(ask), (bid + ask) / 2, last),
      key = name_key(name)
    ) |>
    select(key, name, price)
}

kalshi_dem <- kalshi_event("KXPRESNOMD", "kalshi_nominee_dem") |> mutate(party = "D")
kalshi_rep <- kalshi_event("KXPRESNOMR", "kalshi_nominee_rep") |> mutate(party = "R")

# combine: one row per person per party
pm_nominee <- bind_rows(pm_dem, pm_rep) |>
  select(key, party, pm_name = name, polymarket_nominee = price)
kalshi_nominee <- bind_rows(kalshi_dem, kalshi_rep) |>
  select(key, party, kalshi_name = name, kalshi_nominee = price)

pool <- poll_summary |>
  full_join(pm_nominee, by = c("key", "party")) |>
  full_join(kalshi_nominee, by = c("key", "party")) |>
  left_join(select(pm_run_2027, key, announce_by_2027 = price), by = "key") |>
  left_join(select(pm_run_2028, key, announce_by_2028 = price), by = "key") |>
  mutate(
    name = coalesce(pm_name, kalshi_name, poll_name),
    n_polls_all = replace_na(n_polls_all, 0L),
    n_polls_recent = replace_na(n_polls_recent, 0L),
    best_price = pmax(polymarket_nominee, kalshi_nominee, na.rm = TRUE),
    passes_price = !is.na(best_price) & best_price >= min_price,
    passes_polls = n_polls_recent >= min_polls,
    passes_rule = passes_price | passes_polls
  ) |>
  arrange(party, desc(passes_rule), desc(best_price), desc(n_polls_recent)) |>
  select(
    key, name, party, passes_rule, passes_price, passes_polls,
    polymarket_nominee, kalshi_nominee, announce_by_2027, announce_by_2028,
    n_polls_recent, n_polls_all, avg_pct_recent, last_polled
  )

# people on the announce market who are in no poll and no nominee market
announce_only <- bind_rows(pm_run_2027, pm_run_2028) |>
  distinct(key, .keep_all = TRUE) |>
  anti_join(pool, by = "key")

write_csv(pool, "data/candidate_pool.csv")
write_csv(announce_only, "data/candidate_pool_announce_only.csv")

# summary for the checkpoint
cat("Snapshot:", snapshot, "\n")
cat("Polls read:", n_distinct(polls$poll_id), "| last poll ends", format(last_poll_date), "\n")
cat("People in pool:", nrow(pool), "| pass rule:", sum(pool$passes_rule), "\n")
print(count(pool, party, passes_rule))
cat("Announce-market names not in pool:", nrow(announce_only), "\n")
