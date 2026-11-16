# 2028 field tracker

A public page with one row per potential 2028 presidential candidate, showing market odds and public signals, refreshed once a day. Market prices are not predictions made by this project.

Status on November 12, 2026: the candidate list is approved (61 people), and the feeds and page run locally. The daily GitHub job and the FEC feed have not run yet; both wait on the repository and the API key.

## Layout

```text
data-raw/      downloads kept exactly as received (candidate pool snapshot)
data/          tables written by the scripts, plus two hand-kept files
R/             numbered scripts
docs/          the rendered page, served by GitHub Pages
index.qmd      the page source (Quarto)
styles.css     page styles
.github/workflows/update.yml   the daily job
```

## Feeds

Each feed adds dated rows to its own file, so history is kept. A rerun replaces rows for the same day and refreshes the last ten days, so a missed day fills in on the next run.

| Feed | Script | Source | Output | Status |
| --- | --- | --- | --- | --- |
| Market odds | `R/03_feed_markets.R` | Polymarket and Kalshi public market data | `data/markets.csv` | Working, backfilled 120 days |
| Attention | `R/04_feed_pageviews.R` | Wikimedia pageviews API | `data/pageviews.csv` | Working, backfilled 120 days |
| Polls | `R/05_feed_polls.R` | Wikipedia's lists of national 2028 primary polls | `data/polls.csv`, `data/poll_inclusion.csv` | Working |
| Filings and money | `R/06_feed_fec.R` | FEC API | `data/fec.csv` | Written, not yet run: needs `FEC_API_KEY` |
| Events | by hand | News coverage | `data/events.csv` | Empty; add one dated row per event with a source link |

`R/run_feeds.R` runs every feed and writes `data/feed_status.csv`. If a feed fails, the others still run and the page marks the failed feed as stale.

## How to reproduce

In RStudio, open the project folder and run, in order:

```r
source("R/01_candidate_pool.R")    # polls and markets behind the candidate list
source("R/02_check_candidates.R")  # checks data/candidates.csv
source("R/run_feeds.R")            # all daily feeds
```

Then open `index.qmd` and click Render. The page is written to `docs/index.html`.

## One-time setup on GitHub

1. On github.com, click **New repository**, name it, choose **Public**, leave every box unchecked, and click **Create repository**.
2. In GitHub Desktop, choose **File > Clone repository**, pick the new repository, and click **Clone**.
3. Copy this project's files into the cloned folder.
4. In GitHub Desktop, type a summary such as "Add tracker", click **Commit to main**, then **Push origin**.
5. On github.com, open the repository's **Settings > Secrets and variables > Actions**, click **New repository secret**, name it `FEC_API_KEY`, paste the key from api.data.gov, and click **Add secret**.
6. Open **Settings > Pages**, set **Source** to "Deploy from a branch", choose branch **main** and folder **/docs**, and click **Save**.
7. Open the **Actions** tab, choose **Daily update**, and click **Run workflow** to test the job once.
8. In `R/helpers.R`, replace the `user_agent` text with the repository's web address. Wikimedia asks scripts to identify themselves.

After that, the job runs every morning, commits new data and the rebuilt page, and GitHub Pages serves it.

## Candidate list rules

`data/candidates.csv` is maintained by hand. Adding or removing a person is a one-row edit, followed by rerunning the check script.

**Who is on the list.** A person is listed if either is true on the snapshot date:

- their nominee contract is priced at 1% or more on Polymarket or Kalshi (midpoint of bid and ask), or
- they were named in three or more primary polls in the 365 days before the most recent poll.

The thresholds are set at the top of `R/01_candidate_pool.R`. The list was approved on November 12, 2026.

**Columns.**

| Column | Rule |
| --- | --- |
| `candidate_id` | First and last name, lowercase, joined by underscores |
| `party` | The party whose nomination market or primary polls list the person, even if the person is an independent |
| `office` | The office the person will hold on January 20, 2027, given the November 2026 results. For people out of office, the highest office they have held, in this order: president, vice president, governor or senator, cabinet, representative. `None` if they have held none of these. Cabinet means a Senate-confirmed cabinet or cabinet-level post. |
| `home_state` | State of residence or political base. Blank if not established. |
| `birth_year` | From Wikidata |
| `term_limited_2028` | TRUE if a term limit ends the person's time in their most recent elected office between January 2025 and January 2029 |
| `faces_reelection_2028` | TRUE if the person holds a governor, Senate or House seat whose next regular election is in November 2028 and may legally run for it again |
| `prior_run` | TRUE if the person was a declared candidate for a major-party presidential nomination in an earlier cycle |
| `wikipedia_title` | Exact title of the person's English Wikipedia article |
| `fec_candidate_id` | Blank until the FEC feed has run |
| `declared_on` | Date of the formal 2028 announcement or FEC statement of candidacy. Blank if none. |

## How the page numbers are built

- **Nomination odds:** the average of the Polymarket and Kalshi prices each day. A price is the midpoint of the best bid and ask when they are within 10 points of each other, otherwise the last trade.
- **7-day change:** today's nomination odds minus the value seven days earlier, in percentage points.
- **Announces by end of 2027:** the Polymarket contract that resolves Yes if the person officially announces a 2028 run by December 31, 2027.
- **Polls:** a candidate counts as named in a poll if they have their own column in Wikipedia's table or appear in the poll's "Other" footnote.
- **Wikipedia views:** average daily views by people, not bots, over the last seven days.

## Known limits

- November 2026 results are unofficial until states certify, so `office` should be rechecked after January 20, 2027.
- Only national polls are counted; state primary polls are not.
- The poll feed depends on the layout of Wikipedia's tables. If the layout changes, the feed stops and the page marks polls as stale.
- Spencer Cox has no nominee contract on either market, so he has no odds.
- The page has no dark mode.
- The Duke navy in the palette is darker than chart guidelines prefer, so party is always shown with a D or R label as well as color.
