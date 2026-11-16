# Poll feed: national primary polls from Wikipedia, and how often each candidate is named
source("R/helpers.R")
library(rvest)

cand <- load_candidates()
window_days <- 30

pages <- c(
  D = "Nationwide opinion polling for the 2028 Democratic Party presidential primaries",
  R = "Nationwide opinion polling for the 2028 Republican Party presidential primaries"
)

# end date of a range like "October 30 – November 5, 2026" or "January 1–7, 2026"
poll_end_date <- function(x) {
  x <- str_remove_all(x, "\\[[^\\]]*\\]")
  first_month <- str_extract(x, "[A-Z][a-z]+")
  last_part <- str_squish(str_extract(x, "[^–—-]+$"))
  last_part <- if_else(str_detect(last_part, "^[A-Z][a-z]+"), last_part, paste(first_month, last_part))
  suppressWarnings(mdy(last_part))
}

# number from a cell like "12%", "4.1%[e]" or "—"
cell_pct <- function(x) suppressWarnings(as.numeric(str_extract(x, "^[<>]?\\s*([0-9.]+)", group = 1)))

# a name in a footnote, not followed by "Jr." (so Donald Trump does not match his son)
name_pattern <- function(name) paste0(str_escape(name), "(?! ?Jr)")
in_text <- function(note, name) str_detect(note, regex(name_pattern(name), ignore_case = TRUE))

# percentage that follows a name in a footnote, if any
note_pct <- function(note, name) {
  after <- str_extract(note, regex(paste0(name_pattern(name), "(?: with)?[ :]*[<>]?([0-9.]+)"), ignore_case = TRUE), group = 1)
  suppressWarnings(as.numeric(after))
}

read_party <- function(party) {
  res <- get_json(
    "https://en.wikipedia.org/w/api.php", rate = 1 / 3,
    action = "parse", page = pages[[party]], prop = "text", format = "json", formatversion = 2
  )
  # keep the space where a header breaks over two lines
  doc <- read_html(str_replace_all(res$parse$text, "<br\\s*/?>", " "))
  people <- filter(cand, party == !!party)

  map_dfr(html_elements(doc, "table.wikitable"), \(tab) {
    head_cells <- html_elements(html_element(tab, "tr"), "th")
    heads <- str_squish(str_remove_all(html_text(head_cells), "\\[[^\\]]*\\]"))
    if (length(heads) < 5 || heads[1] != "Poll source") return(NULL)
    name_cols <- match(people$key, name_key(heads))
    other_col <- match("Other", heads)

    map_dfr(html_elements(tab, "tr")[-1], \(row) {
      cells <- html_elements(row, "td")
      if (length(cells) != length(heads)) return(NULL)
      text <- str_squish(html_text(cells))
      end_date <- poll_end_date(text[2])
      if (is.na(end_date) || !str_detect(text[3], "[0-9]")) return(NULL)

      # candidates with their own column
      listed <- tibble(
        candidate_id = people$candidate_id,
        pct = cell_pct(text[name_cols]),
        named = !is.na(name_cols) & !is.na(pct),
        listed_as = "column"
      )

      # candidates named only in the "Other" footnote
      note <- ""
      if (!is.na(other_col)) {
        refs <- html_attr(html_elements(cells[[other_col]], "sup a"), "href")
        refs <- str_remove(refs[!is.na(refs)], "^#")
        notes <- map_chr(refs, \(id) html_text(html_element(doc, paste0("[id='", id, "']"))) %||% "")
        note <- paste(notes[!is.na(notes)], collapse = " ")
      }
      in_note <- !listed$named & map_lgl(people$name, \(n) in_text(note, n))
      listed$pct[in_note] <- map_dbl(people$name[in_note], \(n) note_pct(note, n))
      listed$listed_as[in_note] <- "footnote"
      listed$named <- listed$named | in_note

      listed |>
        filter(named) |>
        mutate(
          pollster = str_squish(str_remove_all(text[1], "\\[[^\\]]*\\]")),
          end_date = end_date,
          sample = text[3],
          party = party
        )
    })
  })
}

polls <- bind_rows(read_party("D"), read_party("R")) |>
  select(end_date, pollster, sample, party, candidate_id, pct, listed_as) |>
  distinct(end_date, pollster, sample, party, candidate_id, .keep_all = TRUE) |>
  arrange(desc(end_date), pollster, candidate_id)

if (n_distinct(polls$end_date) < 20) stop("Poll tables did not parse as expected")

# the full poll table is rebuilt each run because Wikipedia holds the whole history
write_csv(polls, "data/polls.csv")

# dated summary: polls naming each candidate in the last 30 days
inclusion <- polls |>
  filter(end_date > Sys.Date() - window_days) |>
  summarise(
    n_polls_30d = n_distinct(paste(pollster, end_date, sample)),
    avg_pct_30d = round(mean(pct, na.rm = TRUE), 1),
    .by = candidate_id
  ) |>
  right_join(select(cand, candidate_id), by = "candidate_id") |>
  mutate(
    n_polls_30d = replace_na(n_polls_30d, 0L),
    avg_pct_30d = if_else(is.nan(avg_pct_30d), NA_real_, avg_pct_30d),
    date = Sys.Date()
  ) |>
  select(date, candidate_id, n_polls_30d, avg_pct_30d)

upsert_csv(inclusion, "data/poll_inclusion.csv", c("date", "candidate_id"))

n_polls <- polls |> distinct(pollster, end_date, sample, party) |> count(party)
cat("Polls parsed:", paste(n_polls$party, n_polls$n, collapse = ", "),
    "| latest ends", format(max(polls$end_date)), "\n")
cat("Candidates named in the last", window_days, "days:", sum(inclusion$n_polls_30d > 0), "\n")
