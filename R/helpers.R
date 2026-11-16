# Shared helpers for the feed scripts
library(tidyverse)
library(httr2)
library(jsonlite)
library(stringi)

snapshot <- format(Sys.Date(), "%Y-%m-%d")
# replace with the repository URL once it exists (Wikimedia asks for a contact)
user_agent <- "field-tracker-2028 (https://github.com/Maljahmi/field-tracker-2028)"

dir.create("data-raw", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# polite request with retries; rate is requests per second
polite_request <- function(url, rate = 0.5) {
  request(url) |>
    req_user_agent(user_agent) |>
    req_throttle(rate = rate) |>
    req_retry(max_tries = 6)
}

# live JSON, not saved
get_json <- function(url, rate = 0.5, ...) {
  polite_request(url, rate) |>
    req_url_query(...) |>
    req_perform() |>
    resp_body_json()
}

# download once per day, keep the raw file untouched
fetch_json <- function(url, name) {
  path <- file.path("data-raw", paste0(name, "_", snapshot, ".json"))
  if (!file.exists(path)) {
    polite_request(url) |> req_perform(path = path)
  }
  fromJSON(path, simplifyVector = FALSE)
}

# one spelling per person across sources
name_fixes <- c(
  "donald j trump" = "donald trump",
  "donald j trump jr" = "donald trump jr",
  "dwayne the rock johnson" = "dwayne johnson",
  "robert f kennedy jr" = "robert kennedy jr",
  "rfk jr" = "robert kennedy jr",
  "aoc" = "alexandria ocasio cortez",
  "stephen a smith" = "stephen smith",
  "philip murphy" = "phil murphy",
  "john n kennedy" = "john kennedy",
  "vivek ramswamy" = "vivek ramaswamy"
)

name_key <- function(x) {
  key <- x |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[.,'\"]", "") |>
    str_replace_all("-", " ") |>
    str_squish()
  fixed <- name_fixes[key]
  if_else(is.na(fixed), key, unname(fixed))
}

# approved candidate list with the matching key
load_candidates <- function() {
  read_csv("data/candidates.csv", show_col_types = FALSE) |>
    mutate(key = name_key(name))
}

# add new rows to a dated file; a rerun replaces rows with the same keys
upsert_csv <- function(new, path, keys) {
  as_text <- \(d) mutate(d, across(everything(), as.character))
  old <- if (file.exists(path)) {
    read_csv(path, col_types = cols(.default = col_character()))
  }
  bind_rows(as_text(new), old) |>
    distinct(across(all_of(keys)), .keep_all = TRUE) |>
    arrange(across(all_of(keys))) |>
    write_csv(path)
}
