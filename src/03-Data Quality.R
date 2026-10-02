# =============================================================================
# FORMULA 1 DATA PROJECT — SECTION 3: DATA QUALITY ANALYSIS
# =============================================================================
# Purpose: Identify inconsistencies, anomalies, and historical edge cases in F1 data
# Output: Clean enriched dataset ready for transformation (Section 4)
# Next: Table flattening/normalisation → Exploratory Data Analysis (EDA)

# -----------------------------------------------------------------------------
# PRE-ANALYSIS: TABLE INSPECTION
# -----------------------------------------------------------------------------
# Quick sanity checks on base table structure and sizes

head(roundentry)
head(session)
head(team_driver)
head(driver)
head(team)
head(season)
head(round)
head(circuit)
head(driver_championship)

summary(session_entry)
head(session_entry)

# -----------------------------------------------------------------------------
# STEP 1: BUILD ENRICHED RESULTS TABLE (DONE ONCE)
# -----------------------------------------------------------------------------
# All downstream QA checks share these joins, so we build them once here.
# Includes session-level flags (is_classified, is_eligible_for_points) for filters.
# This enriched dataset becomes the foundation for Section 4 (Transformation).

results <- session_entry %>%
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
  select(
    id, year, circuit, forename, surname, team,
    points, detail, laps_completed, fastest_lap_rank,
    is_classified, is_eligible_for_points
  )

# -----------------------------------------------------------------------------
# QA CHECK 1: UNCLASSIFIED DRIVERS WHO SCORED POINTS
# -----------------------------------------------------------------------------
# Anomaly: drivers marked unclassified (is_classified == "f") still received points.
#
# Expected resolution for Section 4 (Transformation):
#   - Flag these records for manual review
#   - Decide whether to preserve or adjust point allocations
#   - Document historical rules that justified these cases

unclassified_with_points <- results %>%
  filter(!is.na(points), is_classified == "f", points > 0) %>%
  select(-is_classified, -is_eligible_for_points)
unclassified_with_points

# Known anomalies identified in initial inspection:
# • Drivers scoring 1 point often had fastest lap, yet many have NA in fastest_lap_rank
# • Stirling Moss (1959 French GP): scored 1 point despite disqualification
# • 3 drivers scored 0.14 pts in 1954 British GP (shared fastest-lap point)

# -----------------------------------------------------------------------------
# QA CHECK 2: DECIMAL POINT ALLOCATIONS
# -----------------------------------------------------------------------------
# Early F1 seasons used fractional points due to:
#   - Shared cars (multiple drivers credited with same finish)
#   - Split fastest-lap bonuses (multiple drivers tied)
# These are legitimate historical cases, not necessarily data errors.
#
# Expected resolution for Section 4 (Transformation):
#   - Preserve fractional points for historical races (pre-~1960)
#   - Document rules per era in a separate lookup table

unique_points <- sort(unique(session_entry$points))

# Identify non-half-integer decimals (exclude 0.5 increments)
decimal_points <- unique_points[unique_points %% 1 != 0 & unique_points %% 1 != 0.5]

decimals_explained <- results %>%
  filter(points %in% decimal_points) %>%
  select(-is_classified, -is_eligible_for_points)
decimals_explained

# Known fractional cases from preliminary scan:
# • 1954 British GP: 7 drivers shared fastest lap → 1/7 = 0.14 pts each (the fastest_lap_rank should be 1)
# • 1955 Argentine GP: 3 drivers shared a 3rd-place car → 4/3 = 1.33 pts each
# Shared driving was common in early F1 eras

# -----------------------------------------------------------------------------
# QA CHECK 3: INELIGIBLE DRIVERS WHO SCORED POINTS
# -----------------------------------------------------------------------------
# Anomaly: entries marked ineligible (is_eligible_for_points == "f") received points.
#
# Expected resolution for Section 4 (Transformation):
#   - Cross-reference with championship standings to confirm actual awarding
#   - Decide whether to zero out or retain points based on official records

ineligible_with_points <- results %>%
  filter(!is.na(points), is_eligible_for_points == "f", points > 0) %>%
  select(-is_classified, -is_eligible_for_points)
ineligible_with_points

monaco_1956 <- results %>%
  filter(year == 1956, circuit == "Monaco Grand Prix") %>%
  select(-is_classified, -is_eligible_for_points)
monaco_1956

# Notable case: Fangio (1954)
#   - Drove 2 shared cars in the same race
#   - Earned 1.5 pts in one car that didn't count toward his total
#     (already had 3 pts from the other car)
#   - Plus fastest lap point

















# -----------------------------------------------------------------------------
# SUMMARY OF DATA QUALITY ISSUES IDENTIFIED
# -----------------------------------------------------------------------------
# | Issue                          | Severity | Action in Section 4          |
# |--------------------------------|----------|------------------------------|
# | NA in fastest_lap_rank         | Medium   | Impute or flag as unknown    |
# | Unclassified with points       | Medium   | Validate per race; document  |
# | Fractional points              | Low      | Preserve; add metadata       |
# | Ineligible with points         | High     | Cross-check with standings   |
# | Multi-car same driver/race     | Medium   | Normalize to car-level view  |

# -----------------------------------------------------------------------------
# PREPARATION FOR SECTION 4 (TRANSFORMATION)
# -----------------------------------------------------------------------------
# Define standard output schema to ensure consistency across transformations

standard_cols <- c(
  "id", "year", "circuit", "forename", "surname", "team",
  "points", "detail", "laps_completed", "fastest_lap_rank"
)

quality_issues_summary <- list(
  unclassified_count = nrow(unclassified_with_points),
  decimal_races = nrow(decimals_explained),
  ineligible_count = nrow(ineligible_with_points),
  case_studies = c(fangio_monaco_1956$year, fangio_monaco_1956$circuit)
)