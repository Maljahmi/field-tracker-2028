# Check data/candidates.csv against the pool, the coding rules and Wikipedia
source("R/helpers.R")

cand <- read_csv("data/candidates.csv", col_types = cols(.default = col_character()))
pool <- read_csv("data/candidate_pool.csv", show_col_types = FALSE)
problems <- character()
note <- function(...) problems <<- c(problems, paste0(...))

# columns
expected <- c(
  "candidate_id", "name", "party", "office", "home_state", "birth_year",
  "term_limited_2028", "faces_reelection_2028", "prior_run",
  "wikipedia_title", "fec_candidate_id", "declared_on"
)
if (!identical(names(cand), expected)) note("Columns do not match the spec")

# ids and allowed values
offices <- c(
  "President", "Vice president", "Governor", "Senator", "Representative", "Cabinet",
  "Former vice president", "Former governor", "Former senator",
  "Former representative", "Former cabinet", "None"
)
dup <- cand$candidate_id[duplicated(cand$candidate_id)]
if (length(dup) > 0) note("Duplicate ids: ", paste(dup, collapse = ", "))
bad <- function(col, ok) {
  x <- cand$candidate_id[!cand[[col]] %in% ok]
  if (length(x) > 0) note("Bad ", col, ": ", paste(x, collapse = ", "))
}
bad("party", c("D", "R"))
bad("office", offices)
bad("home_state", c(state.abb, "DC", NA))
bad("term_limited_2028", c("TRUE", "FALSE"))
bad("faces_reelection_2028", c("TRUE", "FALSE"))
bad("prior_run", c("TRUE", "FALSE"))
bad_date <- cand$candidate_id[!is.na(cand$declared_on) & is.na(as.Date(cand$declared_on, optional = TRUE))]
if (length(bad_date) > 0) note("Bad declared_on: ", paste(bad_date, collapse = ", "))

# people holding a seat cannot both be out of office and face reelection
clash <- cand |> filter(faces_reelection_2028 == "TRUE", !office %in% c("Governor", "Senator", "Representative"))
if (nrow(clash) > 0) note("Faces reelection without a seat: ", paste(clash$candidate_id, collapse = ", "))

# match to the pool
cand <- cand |> mutate(key = name_key(name))
passers <- pool |> filter(passes_rule)
not_in_pool <- anti_join(cand, pool, by = c("key", "party"))
fails_rule <- semi_join(cand, filter(pool, !passes_rule), by = c("key", "party"))
missing <- anti_join(passers, cand, by = c("key", "party"))
if (nrow(not_in_pool) > 0) note("Not in pool: ", paste(not_in_pool$name, collapse = ", "))

# Wikipedia: title exists, is not a redirect or disambiguation page
wiki <- map_dfr(split(cand$wikipedia_title, ceiling(seq_along(cand$wikipedia_title) / 40)), \(batch) {
  res <- polite_request("https://en.wikipedia.org/w/api.php") |>
    req_url_query(
      action = "query", prop = "pageprops", redirects = 1, format = "json",
      titles = paste(batch, collapse = "|")
    ) |>
    req_perform() |>
    resp_body_json()
  map_dfr(res$query$pages, \(p) {
    tibble(
      wikipedia_title = p$title,
      exists = is.null(p$missing),
      disambiguation = !is.null(p$pageprops$disambiguation),
      wikidata_id = p$pageprops$wikibase_item %||% NA_character_
    )
  })
})
unresolved <- setdiff(cand$wikipedia_title, wiki$wikipedia_title[wiki$exists & !wiki$disambiguation])
if (length(unresolved) > 0) note("Wikipedia title not exact: ", paste(unresolved, collapse = ", "))

# Wikidata: birth year
ids <- na.omit(wiki$wikidata_id)
births <- map_dfr(split(ids, ceiling(seq_along(ids) / 40)), \(batch) {
  res <- polite_request("https://www.wikidata.org/w/api.php") |>
    req_url_query(
      action = "wbgetentities", props = "claims", format = "json",
      ids = paste(batch, collapse = "|")
    ) |>
    req_perform() |>
    resp_body_json()
  map_dfr(res$entities, \(e) {
    tibble(
      wikidata_id = e$id,
      wikidata_birth_year = str_sub(e$claims$P569[[1]]$mainsnak$datavalue$value$time %||% NA, 2, 5)
    )
  })
})
birth_check <- cand |>
  left_join(wiki, by = "wikipedia_title") |>
  left_join(births, by = "wikidata_id") |>
  filter(is.na(wikidata_birth_year) | birth_year != wikidata_birth_year)
if (nrow(birth_check) > 0) note("Birth year differs from Wikidata: ", paste(birth_check$candidate_id, collapse = ", "))

# report
cat("Candidates:", nrow(cand), "\n")
print(count(cand, party))
print(count(cand, office, sort = TRUE), n = 20)
cat("Term-limited:", sum(cand$term_limited_2028 == "TRUE"),
    "| face reelection in 2028:", sum(cand$faces_reelection_2028 == "TRUE"),
    "| ran before:", sum(cand$prior_run == "TRUE"),
    "| declared:", sum(!is.na(cand$declared_on)), "\n")
cat("Blank home_state:", paste(cand$candidate_id[is.na(cand$home_state)], collapse = ", "), "\n")
cat("On the list but below the rule:", paste(fails_rule$name, collapse = ", "), "\n")
cat("Pass the rule but not on the list:", paste(missing$name, collapse = ", "), "\n")
cat("Wikipedia titles checked:", sum(wiki$exists), "| birth years matched:",
    nrow(cand) - nrow(birth_check), "\n")

if (length(problems) > 0) {
  cat("\nPROBLEMS\n", paste("-", problems, collapse = "\n"), "\n")
  quit(status = 1)
}
cat("\nAll checks passed\n")
