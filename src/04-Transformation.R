# Load the formula1 view into a data frame
con <- NULL

tryCatch({
  
  # Connect to the SQLite database
  con <- dbConnect(SQLite(), dbname = "data/formula1.db")

  # List all tables and views in the database
  tables <- dbListTables(con)
  message(sprintf("Database contains %d tables/views:", length(tables)))
  message(paste(tables, collapse = "\n"))

  # Load the driver_nationality_corrections and team_nationality_corrections tables
  driver_nationality_corrections <- dbReadTable(con, "driver_nationality_corrections")
  team_nationality_corrections <- dbReadTable(con, "team_nationality_corrections")
  
  # Optionally attach it to the global environment
  assign("driver_nationality_corrections", driver_nationality_corrections, envir = .GlobalEnv)
  assign("team_nationality_corrections", team_nationality_corrections, envir = .GlobalEnv)
  
  # Load only the formula1 view
  formula1 <- dbReadTable(con, "f1summary")
  
  # Optionally attach it to the global environment
  assign("formula1", formula1, envir = .GlobalEnv)

  # Report what was loaded
  message(sprintf("Successfully loaded 'formula1' view with %d rows and %d columns",
                  nrow(formula1), ncol(formula1)))
  
}, error = function(e) {
  
  # Handle any error that occurred
  message(sprintf("Error during database import: %s", conditionMessage(e)))
  
}, finally = {
  
  # Always close the connection if it exists
  if (!is.null(con)) {
    tryCatch(
      dbDisconnect(con),
      warning = function(w) message("Warning while closed connection: ", conditionMessage(w))
    )
  }
  con <- NULL
  
})

#formula1 %>%
#  filter(constructorName == "Alfa Romeo") %>%
#  select(constructorName, constructorCountry, constructorImage)

# Only keep data for the previous years :
current_year <- as.numeric(format(Sys.Date(), "%Y"))
head(driver_nationality_corrections)
head(team_nationality_corrections)
