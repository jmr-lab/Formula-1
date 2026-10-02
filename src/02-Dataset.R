# Connect to the SQLite database
con <- dbConnect(
  SQLite(),
  dbname = "data/formula1.db"
)

all_tables <- dbListTables(con)
include_tables <- c(
  "session_entry",
  "roundentry",
  "session",
  "team_driver",
  "driver",
  "team",
  "season",
  "round",
  "circuit",
  "driver_championship"
)

# Find all remaining tables
other_tables <- setdiff(all_tables, include_tables)

# Exclude f1summary and _countries_lookup
other_tables <- other_tables[
  !other_tables %in% c("f1summary", "_countries_lookup")
]

# Create the two groups
table_summary <- rbind(
  data.frame(
    group = rep("Tables to include", length(include_tables)),
    table = include_tables,
    stringsAsFactors = FALSE
  ),
  data.frame(
    group = rep("Other tables", length(other_tables)),
    table = other_tables,
    stringsAsFactors = FALSE
  )
)

# Count rows
table_summary$rows <- sapply(
  table_summary$table,
  function(table_name) {
    quoted_table <- dbQuoteIdentifier(con, table_name)
    
    dbGetQuery(
      con,
      paste0("SELECT COUNT(*) AS n FROM ", quoted_table)
    )$n
  }
)

table_summary

table_summary[
  table_summary$group == "Tables to include",
  c("table", "rows")
]

table_summary[
  table_summary$group == "Other tables",
  c("table", "rows")
]

# Create a dm object containing only the selected tables
dm_f <- dm_from_con(
  con,
  table_names = include_tables,
  learn_keys = TRUE
)

dm_f

# Add primary keys to the data model
# dm_f <- dm_add_pk(dm_f, session_entry, id)
# dm_f <- dm_add_pk(dm_f, roundentry, id)
# dm_f <- dm_add_pk(dm_f, session, id)
# dm_f <- dm_add_pk(dm_f, team_driver, id)
# dm_f <- dm_add_pk(dm_f, driver, id)
# dm_f <- dm_add_pk(dm_f, team, id)
# dm_f <- dm_add_pk(dm_f, season, id)
# dm_f <- dm_add_pk(dm_f, round, id)
# dm_f <- dm_add_pk(dm_f, circuit, id)
# dm_f <- dm_add_pk(dm_f, driver_championship, id)

# Add foreign key references to the data model
# dm_f <- dm_add_fk(dm_f, session_entry, round_entry_id, roundentry, id)
# dm_f <- dm_add_fk(dm_f, session_entry, session_id, session, id)
# dm_f <- dm_add_fk(dm_f, roundentry, round_id, round, id)
# dm_f <- dm_add_fk(dm_f, roundentry, team_driver_id, team_driver, id)
# dm_f <- dm_add_fk(dm_f, session, round_id, round, id)
# dm_f <- dm_add_fk(dm_f, team_driver, team_id, team, id)
# dm_f <- dm_add_fk(dm_f, team_driver, driver_id, driver, id)
# dm_f <- dm_add_fk(dm_f, team_driver, season_id, season, id)
# dm_f <- dm_add_fk(dm_f, round, season_id, season, id)
# dm_f <- dm_add_fk(dm_f, round, circuit_id, circuit, id)
# dm_f <- dm_add_fk(dm_f, driver_championship, session_id, session, id)
# dm_f <- dm_add_fk(dm_f, driver_championship, driver_id, driver, id)
# dm_f <- dm_add_fk(dm_f, driver_championship, season_id, season, id)
# dm_f <- dm_add_fk(dm_f, driver_championship, round_id, round, id)

# Create the graph
graph <- dm_f %>%
  dm_set_colors(
    darkblue = starts_with("team"),
    darkgreen = starts_with("driver")
  ) %>%
  dm_draw(rankdir = "TB")

# Display the graph
graph

# Load the selected tables into data frames
table_data <- lapply(
  include_tables,
  function(table_name) {
    dbReadTable(con, table_name)
  }
)

# Give each data frame the same name as its database table
names(table_data) <- include_tables

# Create objects in the current environment:
# session_entry, roundentry, session, team_driver, etc.
list2env(table_data, envir = .GlobalEnv)
