# FEC feed: 2028 presidential filings matched to the candidate list
# Not yet run against the live API: it needs the FEC_API_KEY secret
source("R/helpers.R")

key <- trimws(Sys.getenv("FEC_API_KEY"))
if (key == "") stop("FEC_API_KEY is not set")

cand <- load_candidates()
base <- "https://api.open.fec.gov/v1"

# every presidential candidate record for the 2028 cycle
fec_page <- function(page) {
  get_json(
    paste0(base, "/candidates/"), rate = 1,
    api_key = key, office = "P", election_year = 2028,
    per_page = 100, page = page, sort = "name"
  )
}
first <- fec_page(1)
n_pages <- first$pagination$pages %||% 1
results <- c(first$results, unlist(map(seq_len(n_pages)[-1], \(p) fec_page(p)$results), recursive = FALSE))

# FEC names look like "LAST, FIRST MIDDLE"
filers <- map_dfr(results, \(r) {
  tibble(
    fec_candidate_id = r$candidate_id,
    fec_name = r$name,
    fec_party = r$party %||% NA_character_,
    first_file_date = r$first_file_date %||% NA_character_
  )
}) |>
  mutate(
    last = str_squish(str_extract(fec_name, "^[^,]+")),
    first = str_extract(str_squish(str_remove(fec_name, "^[^,]+,")), "^[^ ]+"),
    key = name_key(paste(first, last))
  )

# match on first and last name; ids already in candidates.csv win
matches <- cand |>
  mutate(match_key = name_key(paste(word(name, 1), word(name, -1)))) |>
  inner_join(filers, by = c("match_key" = "key"), relationship = "many-to-many") |>
  filter(is.na(fec_candidate_id.x) | fec_candidate_id.x == fec_candidate_id.y) |>
  transmute(candidate_id, fec_candidate_id = fec_candidate_id.y, fec_name, fec_party, first_file_date)

# receipts for each matched filing
receipts <- map_dbl(matches$fec_candidate_id, \(id) {
  res <- tryCatch(
    get_json(paste0(base, "/candidate/", id, "/totals/"), rate = 1, api_key = key, cycle = 2028),
    error = \(e) NULL
  )
  as.numeric(res$results[[1]]$receipts %||% NA)
})

rows <- matches |>
  mutate(receipts = receipts, date = Sys.Date()) |>
  select(date, candidate_id, fec_candidate_id, fec_name, fec_party, first_file_date, receipts)

upsert_csv(rows, "data/fec.csv", c("date", "candidate_id", "fec_candidate_id"))

cat("FEC 2028 presidential filers:", nrow(filers), "| matched to the list:", nrow(rows), "\n")
