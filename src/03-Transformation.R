# ---------------------------------------------------------------------------
# Load the analytics schema (formula1.db): the dbt-built f1summary view plus
# the season-scoped nationality correction tables promised in report §2.4.10.
# ---------------------------------------------------------------------------

con <- NULL

tryCatch({
  
  # Connect to the dbt-transformed analytics database
  con <- dbConnect(SQLite(), dbname = "data/formula1.db")
  
  # List all tables and views — informational only (does not distinguish
  # tables from views; use sqlite_master via dbGetQuery for that)
  tables <- dbListTables(con)
  message(sprintf("Database contains %d tables/views:", length(tables)))
  message(paste(tables, collapse = "\n"))
  
  # Load the nationality correction tables. These repair the "single
  # nationality retrojected onto all history" defect documented in §2.4.10
  # (e.g. Alfa Romeo recorded as Swiss even for its Italian-era entries,
  # Bertrand Gachot's Belgian/French dual nationality).
  driver_nationality_corrections <- dbReadTable(con, "driver_nationality_corrections")
  team_nationality_corrections <- dbReadTable(con, "team_nationality_corrections")
  
  # Attach to the global environment so report chunks can access them
  # regardless of where this script is sourced from
  assign("driver_nationality_corrections", driver_nationality_corrections, envir = .GlobalEnv)
  assign("team_nationality_corrections", team_nationality_corrections, envir = .GlobalEnv)
  
  # Load the f1summary view: the main per-driver/per-constructor aggregate
  # used throughout Chapter 4 (EDA)
  formula1 <- dbReadTable(con, "f1summary")
  
  # Attach to the global environment (same rationale as above)
  assign("formula1", formula1, envir = .GlobalEnv)
  
  # Report what was loaded
  message(sprintf("Successfully loaded 'formula1' view with %d rows and %d columns",
                  nrow(formula1), ncol(formula1)))
  
}, error = function(e) {
  
  # Report any failure (missing dbt output, stale database, ...)
  message(sprintf("Error during database import: %s", conditionMessage(e)))
  
}, finally = {
  
  # Always close the connection if it was opened
  if (!is.null(con)) {
    tryCatch(
      dbDisconnect(con),
      warning = function(w) message("Warning while closing connection: ", conditionMessage(w))
    )
  }
  con <- NULL
  
})