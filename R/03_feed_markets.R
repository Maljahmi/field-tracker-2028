# Market feed: daily prices per candidate from Polymarket and Kalshi
source("R/helpers.R")

cand <- load_candidates()
out_path <- "data/markets.csv"

# first run backfills; later runs refresh the last days so gaps heal
days_back <- if (file.exists(out_path)) 10 else 120
start_ts <- as.integer(Sys.time()) - days_back * 86400
now_ts <- as.integer(Sys.time())

# midpoint when the spread is tight, otherwise the last trade
quote_price <- function(bid, ask, last) {
  tight <- !is.na(bid) & !is.na(ask) & (ask - bid) <= 0.1
  if_else(tight, (bid + ask) / 2, last)
}

# Polymarket: contracts in one event
polymarket_contracts <- function(slug, market) {
  ev <- get_json(paste0("https://gamma-api.polymarket.com/events?slug=", slug), rate = 5)
  map_dfr(ev[[1]]$markets, \(m) {
    tibble(
      key = name_key(m$groupItemTitle %||% NA_character_),
      active = isTRUE(m$active) && !isTRUE(m$closed),
      token = fromJSON(m$clobTokenIds %||% "[null]")[1],
      live = quote_price(
        as.numeric(m$bestBid %||% NA), as.numeric(m$bestAsk %||% NA),
        as.numeric(m$lastTradePrice %||% NA)
      )
    )
  }) |>
    filter(active, !is.na(key), !is.na(token)) |>
    mutate(market = market)
}

# Polymarket: daily history for one contract
polymarket_history <- function(token) {
  res <- get_json(
    "https://clob.polymarket.com/prices-history", rate = 5,
    market = token, startTs = start_ts, fidelity = 1440
  )
  tibble(
    ts = map_dbl(res$history, "t"),
    price = map_dbl(res$history, "p")
  ) |>
    # a point at midnight UTC closes the day before
    mutate(date = as.Date(as.POSIXct(ts - 43200, origin = "1970-01-01", tz = "UTC"))) |>
    summarise(price = last(price), .by = date)
}

pm_nominee <- bind_rows(
  polymarket_contracts("democratic-presidential-nominee-2028", "nominee") |> mutate(party = "D"),
  polymarket_contracts("republican-presidential-nominee-2028", "nominee") |> mutate(party = "R")
) |>
  inner_join(select(cand, candidate_id, key, party), by = c("key", "party"))

pm_announce <- polymarket_contracts(
  "who-will-announce-presidential-run-before-2028-20260803142755224", "announce_2028"
) |>
  inner_join(select(cand, candidate_id, key), by = "key")

pm_contracts <- bind_rows(pm_nominee, pm_announce)

pm_rows <- pmap_dfr(pm_contracts, \(candidate_id, token, market, live, ...) {
  hist <- tryCatch(polymarket_history(token), error = \(e) tibble(date = as.Date(character()), price = numeric()))
  today <- tibble(date = Sys.Date(), price = live)
  bind_rows(today, hist) |>
    filter(!is.na(price)) |>
    distinct(date, .keep_all = TRUE) |>
    mutate(candidate_id = candidate_id, source = "polymarket", market = market)
})

# Kalshi: contracts in one series
kalshi_contracts <- function(series, party) {
  ev <- get_json(
    "https://api.elections.kalshi.com/trade-api/v2/events", rate = 5,
    series_ticker = series, with_nested_markets = "true"
  )
  map_dfr(ev$events[[1]]$markets, \(m) {
    tibble(
      key = name_key(m$yes_sub_title),
      ticker = m$ticker,
      status = m$status,
      live = quote_price(
        as.numeric(m$yes_bid_dollars), as.numeric(m$yes_ask_dollars),
        as.numeric(m$last_price_dollars)
      )
    )
  }) |>
    filter(status == "active") |>
    mutate(series = series, party = party)
}

# Kalshi: daily candles for one contract
kalshi_history <- function(series, ticker) {
  res <- get_json(
    paste0(
      "https://api.elections.kalshi.com/trade-api/v2/series/", series,
      "/markets/", ticker, "/candlesticks"
    ),
    rate = 5, start_ts = start_ts, end_ts = now_ts, period_interval = 1440
  )
  num <- \(x) as.numeric(x %||% NA)
  map_dfr(res$candlesticks, \(k) {
    tibble(
      ts = k$end_period_ts,
      price = quote_price(
        num(k$yes_bid$close_dollars), num(k$yes_ask$close_dollars), num(k$price$close_dollars)
      )
    )
  }) |>
    # candles close at midnight Eastern
    mutate(date = as.Date(as.POSIXct(ts - 43200, origin = "1970-01-01", tz = "UTC"), tz = "America/New_York")) |>
    summarise(price = last(price), .by = date)
}

kalshi <- bind_rows(
  kalshi_contracts("KXPRESNOMD", "D"),
  kalshi_contracts("KXPRESNOMR", "R")
) |>
  inner_join(select(cand, candidate_id, key, party), by = c("key", "party"))

kalshi_rows <- pmap_dfr(kalshi, \(candidate_id, series, ticker, live, ...) {
  hist <- tryCatch(kalshi_history(series, ticker), error = \(e) tibble(date = as.Date(character()), price = numeric()))
  today <- tibble(date = Sys.Date(), price = live)
  bind_rows(today, hist) |>
    filter(!is.na(price)) |>
    distinct(date, .keep_all = TRUE) |>
    mutate(candidate_id = candidate_id, source = "kalshi", market = "nominee")
})

rows <- bind_rows(pm_rows, kalshi_rows) |>
  mutate(price = round(price, 4)) |>
  select(date, candidate_id, source, market, price)

if (nrow(rows) == 0) stop("No market prices returned")
upsert_csv(rows, out_path, c("date", "candidate_id", "source", "market"))

cat("Market rows written:", nrow(rows), "| candidates:", n_distinct(rows$candidate_id),
    "| days:", n_distinct(rows$date), "\n")
print(count(rows, source, market))
no_market <- setdiff(cand$candidate_id, rows$candidate_id[rows$market == "nominee"])
cat("Candidates with no nominee contract:", paste(no_market, collapse = ", "), "\n")
