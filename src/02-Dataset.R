# ---------------------------------------------------------------------------
# Load all tables from jolpica.db into data frames in the global environment,
# and build a table inventory (name / columns / rows) for documentation.
# ---------------------------------------------------------------------------

con <- NULL

tryCatch({
  
  # Connect to the raw Jolpica snapshot
  con <- dbConnect(SQLite(), dbname = "data/jolpica.db")
  
  # List all tables, skipping SQLite internal tables (e.g. sqlite_sequence)
  tables <- dbListTables(con)
  tables <- tables[!startsWith(tables, "_")]
  
  # Read each table into a data frame named after the table itself,
  # e.g. table "driver" -> data frame `driver`
  # (downstream chunks rely on this naming, incl. `inventory`)
  dfs <- lapply(setNames(tables, tables), function(tbl) dbReadTable(con, tbl))
  
  # Load all data frames into the global environment
  list2env(dfs, envir = .GlobalEnv)
  
  # --- Build the table inventory -------------------------------------------
  if (length(dfs) > 0) {
    
    # Inventory: one row per table with its dimensions
    inventory <- tibble(
      table_name = names(dfs),
      n_cols     = map_int(dfs, ncol),
      n_rows     = map_int(dfs, nrow)
    )
    
    # Sort alphabetically by table name
    inventory <- inventory[order(inventory$table_name), ]
    
    message("Table inventory (columns / rows):")
    print(inventory)
  }
  # --------------------------------------------------------------------------
  
  # Inform user how many tables were loaded
  message(sprintf("Successfully loaded %d tables", length(tables)))
  
}, error = function(e) {
  
  # Report any failure (bad path, corrupt database, missing package, ...)
  message(sprintf("Error during database import: %s", conditionMessage(e)))
  
}, finally = {
  
  # Always close the connection if it exists
  if (!is.null(con)) {
    tryCatch(
      dbDisconnect(con),
      warning = function(w) message("Warning while closing connection: ", conditionMessage(w))
    )
  }
  con <- NULL
  
})

# ---------------------------------------------------------------------------
# Report helpers for Section 2.2: a rounded total row count used in prose,
# and a LaTeX table listing the raw Jolpica export files in a 3-column grid.
# ---------------------------------------------------------------------------

# Total size of the dataset across all tables, rounded to the nearest 10,000
# (e.g. 123,456 rows -> 120) and expressed in thousands. Used for inline
# prose like "~r total_rows_k K rows covering every F1 race since 1950".
total_rows_k <- round(sum(inventory$n_rows) / 1000, -1)

# --- Lay out the export files in a 3-column grid ---------------------------
# Number of grid rows needed so that every table gets a cell
# (ceiling accommodates a partial last row when the table count
# is not divisible by 3).
n_col <- 3
n_row <- ceiling(nrow(inventory) / n_col)

# Icon shown next to each file name in the rendered table.
# here::here() resolves the project-root-relative path; the gsub() call
# normalises separators to forward slashes, which LaTeX requires
# (backslashes on Windows paths would otherwise be parsed as commands).
icon_path <- here::here("images", "report", "file-csv.png")
icon_latex <- paste0(
  "\\includegraphics[width=0.5cm]{",
  gsub("\\\\", "/", icon_path),
  "}"
)

# Build the file names ("table_name.csv"), escaping underscores as "\_"
# so LaTeX renders them literally instead of treating them as subscripts,
# then sort alphabetically for a stable, predictable listing.
files <- sort(paste0(gsub("_", "\\\\_", inventory$table_name), ".csv"))

# Pad the list with empty strings so it exactly fills the n_row x n_col grid
# (avoids an NA-filled ragged last row in the matrix below).
files <- c(files, rep("", n_row * n_col - length(files)))

# Reshape into the grid. NOTE: byrow = FALSE fills column by column,
# so files run DOWN each column, not across each row — i.e. alphabetical
# order reads top-to-bottom, left-to-right in the final table.
grid <- matrix(files, nrow = n_row, ncol = n_col, byrow = FALSE)

# Interleave the icon column with each file column: cells that hold a file
# name get the icon's LaTeX snippet, padding cells ("") get an empty string
# so no stray icon appears in blank cells. stringsAsFactors = FALSE keeps
# the columns as plain characters (essential for escape = FALSE in kbl()).
dataset_files <- data.frame(
  icon1 = ifelse(grid[, 1] == "", "", icon_latex),
  file1 = grid[, 1],
  icon2 = ifelse(grid[, 2] == "", "", icon_latex),
  file2 = grid[, 2],
  icon3 = ifelse(grid[, 3] == "", "", icon_latex),
  file3 = grid[, 3],
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Helpers for rendering the §2.3 schema tables (Table | Rows | Purpose).
# Each caller supplies a two-column tibble (~Table, ~Purpose); this trio
# attaches live row counts from the inventory, LaTeX-escapes the table
# names, and typesets everything as a booktabs table.
# ---------------------------------------------------------------------------

# Return the comma-formatted row count for one or more table names.
# `match()` maps each name to its row in the inventory; any name not found
# (typo, renamed table, stale inventory) aborts with an explicit error
# listing the offending names — silently printing NA would corrupt the
# report's counts unnoticed.
rows_of <- function(table_name) {
  idx <- match(table_name, inventory$table_name)
  if (any(is.na(idx))) {
    stop("Unknown table(s) in inventory: ",
         paste(table_name[is.na(idx)], collapse = ", "))
  }
  format(inventory$n_rows[idx], big.mark = ",", scientific = FALSE)
}

# Wrap a string in LaTeX \texttt{} and escape underscores as "\_",
# so names like "session_entry" render literally (unescaped underscores
# would be parsed as subscript commands by LaTeX).
to_tex <- function(x) paste0("\\texttt{", gsub("_", "\\\\_", x), "}")

# Typeset a schema description (columns: Table, Purpose) as the standard
# three-column table used throughout §2.3:
#   1. look up the formatted row count for each table,
#   2. order the columns as Table | Rows | Purpose,
#   3. monospace + escape the table names,
#   4. render via kbl with escape = FALSE so the pre-built \texttt{}
#      markup and fixed column widths pass through to LaTeX untouched.
fmt_schema_table <- function(df) {
  df <- df %>%
    mutate(Rows = rows_of(Table)) %>%
    relocate(Table, Rows, Purpose) %>%      # put Rows back where it belongs
    mutate(Table = to_tex(Table))
  kbl(df, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      col.names = c("Table", "Rows", "Purpose")) %>%
    kable_styling(font_size = 9, latex_options = "hold_position") %>%
    column_spec(1, width = "4.5cm") %>%
    column_spec(2, width = "1.5cm") %>%
    column_spec(3, width = "9cm")
}

# ---------------------------------------------------------------------------
# results: flattened per-driver race results (cf. report §2.4)
# ---------------------------------------------------------------------------

# The Jolpica export models modern F1 with fine-grained tables
# (session -> session_entry, round -> roundentry -> team_driver), so a
# driver's race result is spread across five hops. This chain flattens it
# into one analysis-friendly data frame:
#
#   session_entry          one row per driver per session (the anchor)
#   + session              add session type, keep race sessions only ("R")
#   + roundentry           link session_entry -> round entry (team_driver_id)
#   + team_driver          link round entry -> team_id + driver_id
#   + round                add season_id, circuit_id, circuit name
#   + team                 add constructor name
#   + driver               add forename/surname
#   + season               add championship year
#
# `filter(type == "R")` restricts to Grands Prix, excluding qualifying,
# practice and sprint sessions — the basis for every result-level table in
# Chapter 2 (winner uniqueness, fractional points, flags integrity).
# MUST be defined before any block that consumes `results`.
results <- session_entry %>%
  left_join(
    session %>% select(id, type),
    by = c("session_id" = "id")
  ) %>%
  left_join(
    roundentry %>% select(id, round_id, team_driver_id),
    by = c("round_entry_id" = "id")
  ) %>%
  left_join(
    team_driver %>% select(id, team_id, driver_id),
    by = c("team_driver_id" = "id")
  ) %>%
  left_join(
    round %>% select(id, season_id, circuit_id, circuit = name),
    by = c("round_id" = "id")
  ) %>%
  left_join(
    team %>% select(id, team = name),
    by = c("team_id" = "id")
  ) %>%
  left_join(
    driver %>% select(id, forename, surname),
    by = c("driver_id" = "id")
  ) %>%
  left_join(
    season %>% select(id, year),
    by = c("season_id" = "id")
  ) %>%
  filter(type == "R") %>%
  select(
    id, year, circuit, forename, surname, team,
    position, points, detail, laps_completed, fastest_lap_rank,
    is_classified, is_eligible_for_points
  )

# ---------------------------------------------------------------------------
# Races with more than one recorded winner (shared drives, cf. report §2.4.3)
# ---------------------------------------------------------------------------

# Count how many drivers are recorded in position 1 for each race session;
# keep only sessions where more than one driver shares the win.
multi_winner_races <- session %>%
  filter(type == "R") %>%                                        # race sessions only
  left_join(session_entry, by = c("id" = "session_id")) %>%
  filter(position == 1) %>%                                      # winners only
  count(id, name = "n_winners") %>%                              # drivers ranked 1st per session
  filter(n_winners > 1)                                          # shared wins

# ---------------------------------------------------------------------------
# Races with more than one recorded winner — human-readable context
# (year + circuit per affected race, cf. report §2.4.3)
# ---------------------------------------------------------------------------

# Group the results by season and Grand Prix, keep the winners, and retain
# only races where more than one driver is recorded in position 1.
# Complements multi_winner_races (session-level view) with readable labels.
multi_winner_details <- results %>%
  group_by(year, circuit) %>%
  filter(position == 1) %>%
  summarise(n_winners = n()) %>%
  filter(n_winners > 1)

# ---------------------------------------------------------------------------
# Survey of fractional point values (cf. report §2.4.5)
# ---------------------------------------------------------------------------

# All non-integer point values in the export, grouped by their fractional
# part. Rounding to 3 decimals before counting is essential: the 1/7-point
# shares of the 1954 British GP are stored as binary floats whose low-order
# digits differ row to row, so without round() identical fractions would
# split into several near-identical groups (e.g. four 0.14 buckets instead
# of one).
fractional_points_survey <- session_entry %>%
  filter(!is.na(points), points %% 1 != 0) %>%              # fractional values only
  mutate(fractional_part = round(points %% 1, 3)) %>%       # normalise float noise
  count(fractional_part, name = "n_rows") %>%
  arrange(desc(n_rows))                                     # most common first

# ---------------------------------------------------------------------------
# Fine fractions: values that are neither integers nor exact halves
# (cf. report §2.4.6) — the 1/7 shares of the 1954 British GP and the
# 4/3 shares of the 1955 Argentine GP
# ---------------------------------------------------------------------------

# Step 1 — identify the "contaminated" point values: distinct values whose
# fractional part is neither 0 nor 0.5. Drawn from session_entry (the raw
# source of truth for points) so the definition is independent of the
# results-table view.
fine_fraction_values <- session_entry %>%
  distinct(points) %>%
  filter(points %% 1 != 0, points %% 1 != 0.5) %>%   # exclude integers and halves
  pull(points)

# Step 2 — fetch the result rows carrying those values, with readable
# context (year, circuit, driver, team, position). Sorted chronologically
# for the report.
fine_fraction_rows <- results %>%
  filter(points %in% fine_fraction_values) %>%
  transmute(year, circuit, driver = paste(forename, surname),
            team, position, points) %>%
  arrange(year, circuit)

# ---------------------------------------------------------------------------
# Shared fastest-lap award at the 1954 British GP (cf. report §2.4.7)
# ---------------------------------------------------------------------------

# Isolate the rows whose fractional part sits within ±0.01 of 1/7 (~0.143):
# seven drivers set an identical fastest lap and shared its single bonus
# point seven ways. The tolerance comparison is robust to the float noise
# documented in fractional_points_survey (a plain equality test would be
# fragile); it cannot accidentally admit other fractions, as the nearest
# rivals (1/6 ~ 0.167, 1/8 = 0.125) lie far outside the band.
# fastest_lap_rank is included to demonstrate in the report that the column
# is NA for these rows — the coverage boundary §2.4.7 discusses.
fastest_lap_shared_rows <- results %>%
  filter(abs(points %% 1 - 1/7) < 1e-2) %>%
  transmute(year, circuit, driver = paste(forename, surname),
            team, position, points, fastest_lap_rank) %>%
  arrange(circuit, position)

# ---------------------------------------------------------------------------
# Fastest-lap coverage boundary (cf. report §2.4.7)
# ---------------------------------------------------------------------------

# Earliest season for which the export records any fastest_lap_rank.
# Jolpica only tracks fastest laps from 1958 onward; earlier seasons have
# no officially tracked fastest lap — which is why the seven drivers who
# jointly set the fastest lap in 1954 (fastest_lap_shared_rows) show NA
# in that column.
first_fastest_lap_season <- results %>%
  filter(!is.na(fastest_lap_rank)) %>%
  summarise(min(year)) %>%
  pull()                                            # unwrap to a plain scalar

# ---------------------------------------------------------------------------
# Ineligible drivers who nevertheless scored points (cf. report §2.4.8)
# ---------------------------------------------------------------------------

# Violation of the eligibility/points correspondence: rows flagged as
# ineligible (is_eligible_for_points == "f") yet carrying positive points.
# Expected to return exactly ONE row — Fangio's uncollected 1.5 share at
# the 1956 Monaco GP, where the points column preserves the notional share
# while the flag records that it went unawarded. If this ever returns more
# rows (or none), Jolpica or the dbt layer has changed and §2.4.8's prose
# ("violates this expectation exactly once") must be revisited.
ineligible_scorers <- results %>%
  filter(!is.na(points),
         is_eligible_for_points == "f",
         points > 0) %>%
  transmute(year, circuit, driver = paste(forename, surname),
            team, position, points, detail, laps_completed) %>%
  arrange(year, circuit)

# ---------------------------------------------------------------------------
# Context for the eligibility violation: Ferrari entries at the 1956
# Monaco GP (cf. report §2.4.8)
# ---------------------------------------------------------------------------

# All Ferrari entries in the race where Fangio's ineligible 1.5 share
# occurred. The extract shows, in one view:
#   - the shared second place (Fangio + Collins, car #26), where Fangio's
#     extra point vs Collins is the era's fastest-lap bonus;
#   - the shared fourth place (Castellotti + Fangio, car #20, "+1 Lap"),
#     where Fangio's 1.5 is flagged eligible = "f" — the single violating
#     row singled out by ineligible_scorers above;
#   - Castellotti's and Musso's retirements, still classified — the
#     classified-but-retired convention §2.4.9 relies on.
# Constant columns (year, circuit, team) are dropped to save table width.
ferrari_monaco_1956 <- results %>%
  filter(year == 1956,
         team == "Ferrari",
         circuit == "Monaco Grand Prix") %>%
  transmute(driver = paste(forename, surname), position, points,
            eligible = is_eligible_for_points,   # rename for a natural header
            detail, laps_completed) %>%
  arrange(position)

# ---------------------------------------------------------------------------
# Unclassified drivers who nevertheless scored points (cf. report §2.4.9)
# ---------------------------------------------------------------------------

# Mirror image of ineligible_scorers: rows flagged as unclassified
# (is_classified == "f") yet carrying positive points. This is NOT a single
# anomaly but a group with one coherent explanation — the fastest-lap bonus
# point: a driver who sets the fastest lap and then retires (mechanical
# failure, accident, or disqualification) cannot be classified, yet the
# bonus point earned remains attached to his name.
#
# Corroboration structure (used in the prose):
#   - rows from 1958 onward carry fastest_lap_rank == 1 directly
#     (coverage boundary documented in first_fastest_lap_season);
#   - earlier rows show the same one-point signature but the rank column
#     cannot confirm them.
#
# Historical curiosity embedded here: Stirling Moss at the 1959 French GP —
# disqualified after setting the fastest lap, the only disqualified driver
# of the period to score a championship point.
unclassified_scorers <- results %>%
  filter(!is.na(points),
         is_classified == "f",
         points > 0) %>%
  transmute(year, circuit, driver = paste(forename, surname),
            team, position, points, detail, fastest_lap_rank) %>%
  arrange(year, circuit)
