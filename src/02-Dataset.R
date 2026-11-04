# Get tables from jolpica.db and load them into data frames
con <- NULL

tryCatch({
  
  # Connect to the SQLite database
  con <- dbConnect(SQLite(), dbname = "data/jolpica.db")
  
  # List all tables in the database
  tables <- dbListTables(con)
  tables <- tables[!startsWith(tables, "_")]
  
  # Read each table into a dataframe (named after the table)
  dfs <- lapply(setNames(tables, tables), function(tbl) dbReadTable(con, tbl))
  
  # Load all dataframes into the global environment
  list2env(dfs, envir = .GlobalEnv)
  
  # --- Table inventory from loaded data frames ------------------------------
  if (length(dfs) > 0) {
    
    # Build inventory directly from the loaded data frames
    inventory <- tibble(
      table_name = names(dfs),
      n_cols = map_int(dfs, ncol),
      n_rows = map_int(dfs, nrow)
    )
    
    # Sort alphabetically by table name
    inventory <- inventory[order(inventory$table_name), ]
    rownames(inventory) <- NULL
    
    message("Table inventory (columns / rows):")
    print(inventory)
  }
  # -------------------------------------------------------------------------
  
  # Inform user how many tables were loaded
  message(sprintf("Successfully loaded %d tables", length(tables)))
  
}, error = function(e) {
  
  # Handle any error that occurred
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

# championship_adjustment
championship_adjustment
driver %>% filter(id == 703)
season %>% filter(id == 48)

exclude_names <- c("base_team", "penalty", "pit_stop", "lap")

print(inventory)
head(season)
head(session)
head(session_entry)
head(round)
head(roundentry)
