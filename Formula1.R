#############################################################
# 2. Dataset                                                #
#############################################################

#library(rsvg)
#library(DiagrammeR)
library(dplyr)
library(dm)

# Read various data CSV files into data frames
circuits <- as_tibble(read.csv("data/circuits.csv"))
constructor_results <- as_tibble(read.csv("data/constructor_results.csv"))
constructor_standings <- as_tibble(read.csv("data/constructor_standings.csv"))
constructors <- as_tibble(read.csv("data/constructors.csv"))
driver_standings <- as_tibble(read.csv("data/driver_standings.csv"))
drivers <- as_tibble(read.csv("data/drivers.csv"))
races <- as_tibble(read.csv("data/races.csv"))
results <- as_tibble(read.csv("data/results.csv"))
status <- as_tibble(read.csv("data/status.csv"))

# Create a data model from the data frames
dm_f <- dm(
  circuits,
  constructor_results, 
  constructor_standings, 
  constructors, 
  driver_standings, 
  drivers, 
  races, 
  results,
  status
)

# Add primary keys to the data model
dm_f <- dm_add_pk(dm_f, circuits, circuitId)
dm_f <- dm_add_pk(dm_f, constructor_results, constructorResultsId)
dm_f <- dm_add_pk(dm_f, constructor_standings, constructorStandingsId)
dm_f <- dm_add_pk(dm_f, constructors, constructorId)
dm_f <- dm_add_pk(dm_f, driver_standings, driverStandingsId)
dm_f <- dm_add_pk(dm_f, drivers, driverId)
dm_f <- dm_add_pk(dm_f, races, raceId)
dm_f <- dm_add_pk(dm_f, results, resultId)
dm_f <- dm_add_pk(dm_f, status, statusId)

# Add foreign key references to the data model
dm_f <- dm_add_fk(dm_f, constructor_results, constructorId, constructors, constructorId)
dm_f <- dm_add_fk(dm_f, constructor_results, raceId, races, raceId)
dm_f <- dm_add_fk(dm_f, constructor_standings, constructorId, constructors, constructorId)
dm_f <- dm_add_fk(dm_f, constructor_standings, raceId, races, raceId)
dm_f <- dm_add_fk(dm_f, driver_standings, raceId, races, raceId)
dm_f <- dm_add_fk(dm_f, driver_standings, driverId, drivers, driverId)
dm_f <- dm_add_fk(dm_f, races, circuitId, circuits, circuitId)
dm_f <- dm_add_fk(dm_f, results, constructorId, constructors, constructorId)
dm_f <- dm_add_fk(dm_f, results, raceId, races, raceId)
dm_f <- dm_add_fk(dm_f, results, driverId, drivers, driverId)
dm_f <- dm_add_fk(dm_f, results, statusId, status, statusId)

# Create the graph
graph <- dm_f %>%
  dm_set_colors(
  darkblue = starts_with("constructor"),
  darkgreen = starts_with("driver")
  ) %>%
  dm_draw(rankdir = "TB")

# We can use the export_graph function, but the text inside the boxes will overflow,
# it may be better to manually save the graph (graph object) :
#graph %>%
#  export_svg %>% charToRaw %>% rsvg_png("Entity Relationship Diagram Model.png")

# Display the rendered graph
graph

#############################################################
# 2.5 Data Issues                                           #
#############################################################

# Filter the driver_standings dataset to keep the ones for which position != positionText :
driver_standings %>% filter(position != positionText)

# The first item (positionText == "D" or Disqualified).
# Michael Schumacher was indeed disqualified from the European GP in 1997 :
bind_cols(
  races %>% filter(raceId == "223") %>% select(raceId, year, name),
  drivers %>% filter(driverId == "30") %>% select(driverId, forename, surname))

# All results with Disqualified status :
results %>%
  filter(statusId == 2) %>%
  select(-number, -time, -milliseconds, -fastestLap, -rank, -fastestLapTime, -fastestLapSpeed)

# Result for race 223 and driver 30.
# The statusId is 4 which indicates a collision :
status %>%
  filter(statusId == results %>% filter(raceId == 223 & driverId == 30) %>% pull(statusId))

# Fatal Accident (statusId = 104). 3 results only :
# Riccardo Paletti, Helmuth Koinigg and Jochen Rindt :
results %>%
  filter(statusId == 104) %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  left_join(status, by = "statusId") %>%
  select(resultId, grid, positionOrder, laps, statusId, points, year, name, forename, surname, status)

# But Ayrton Senna's status in the 1994 San Marin GP was marked as accident (statusId = 3)
results %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  left_join(status, by = "statusId") %>%
  filter(forename == "Ayrton" & surname == "Senna" & year == 1994 & name == "San Marino Grand Prix") %>%
  select(resultId, grid, positionOrder, laps, statusId, points, year, name, forename, surname, status)

# Filter the results to keep drivers disqualified who won a point (shouldn't occur!?)
# Stirling Moss in the 1959 French GP scored one point (best lap)
# but was disqualified and lost race points :
results %>%
  filter(points > 0) %>%
  filter(statusId == 2) %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  left_join(status, by = "statusId") %>%
  select(resultId, grid, positionOrder, laps, statusId, points, year, name, forename, surname, status)

#############################################################
# 3. Data Transformation                                    #
#############################################################

head(circuits)
head(constructor_results)
head(constructor_standings)
head(constructors)
head(driver_standings)
head(drivers)
head(races)
head(results)
head(status)

# We remove unnecessary columns
circuits_df <- circuits %>% select(-lat, -lng, -url)

# Note sure if we need to keep the constructors tables
constructor_results_df <- constructor_results %>% select(-status)
constructor_standings_df <- constructor_standings %>% select(-positionText)
constructors_df <- constructors %>% select(-url)

# Driver's columns
driver_standings_df <- driver_standings %>% select(-positionText)
drivers_df <- drivers %>% select(-number, -url)

# Not sure if the races table is really needed
races_df <- races %>% select(raceId, year, round, circuitId, name, date)

# Results' columns
results_df <- results %>% select(-number, -time, -milliseconds, -fastestLap, -fastestLapTime, -fastestLapSpeed,
                              -position, -positionText)

# Modifying the status values
status_df <- status %>%
  mutate(
    status = case_when(
      status %in% c("Finished", "Disqualified", "Not classified") ~ status,
      grepl("^\\+\\d+ Laps?$", status) ~ "Lapsed",
      status %in% c("107% Rule", "Did not qualify", "Did not prequalify") ~ "Not Qualified",
      TRUE ~ "Abandoned"
    )
  )
#head(status_df, 20)

# Formula 1 Dataset : from results table, we add race, driver and contructor names
# Join the results and races data frames
formula1 <- results_df %>%
  left_join(races_df, by = "raceId") %>%
  left_join(drivers_df, by = "driverId") %>%
  left_join(constructors_df, by = "constructorId") %>%
  left_join(status_df, by = "statusId")

# Remove unneeded columns
formula1 <- formula1 %>% select(-raceId, -driverId, -constructorId, -circuitId, -statusId, -driverRef, -code, -constructorRef)

# Rename columns
formula1 <- formula1 %>%
  rename(
    circuit = name.x,
    constructorName = name.y,
    driverNationality = nationality.x,
    constructorNationality = nationality.y
  )

# Add the driver age
formula1 <- formula1 %>%
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),  # Convert date to Date type
    dob = as.Date(dob, format = "%Y-%m-%d"),    # Convert dob to Date type
    driverAge = floor(as.numeric(difftime(date, dob, units = "weeks")) / 52.25)  # Calculate age in years
  )

# Remove date columns
formula1 <- formula1 %>% select(-date, -dob)

# Merge name columns
formula1 <- formula1 %>%
  mutate(driverName = paste(forename, surname)) %>%
  select(-forename, -surname)

# Print the resulting formula1 table
print(formula1)
formula1 %>%
  select(resultId, grid, positionOrder, points, year, round, circuit, driverName, driverAge, constructorName) %>%
  head()

#############################################################
# 4. Exploratory Data Analysis                              #
#############################################################

formula1 %>% select(resultId, positionOrder, points, year, driverName, driverNationality, constructorName, constructorNationality)

formula1 %>% select(points, positionOrder, year, constructorName, constructorNationality, driverName, driverNationality) %>%
  group_by(year, driverName, driverNationality, constructorName, constructorNationality) %>%
  summarize(total_points = sum(points, na.rm = TRUE), wins = sum(positionOrder == 1), .groups = 'drop')

# Calculate total points for each driver in winning years
world_champions <- formula1 %>%
  select(points, positionOrder, year, constructorName, driverName, driverNationality) %>%
  group_by(year, driverName, driverNationality) %>%
  summarize(total_points = sum(points, na.rm = TRUE), .groups = 'drop') %>%
  group_by(year) %>%
  filter(total_points == max(total_points)) %>%
  ungroup() %>%
  select(-total_points)

# Counting titles and total career points per driver
titles_count <- world_champions %>%
  group_by(driverName, driverNationality) %>%
  summarize(titles = n(), .groups = 'drop') %>%
  arrange(desc(titles))

# Calculating total career points regardless of titles
career <- formula1 %>%
  group_by(driverName) %>%
  summarize(wins = sum(positionOrder == 1, na.rm = TRUE),
            races = n(),
            seasons = n_distinct(year),
            points = sum(points, na.rm = TRUE),
            .groups = 'drop')

# Joining titles and career points data frames
f1_summary <- titles_count %>%
  left_join(career, by = "driverName")

# Display the final result with titles and total career points
print(f1_summary)

# Stop execution
stop("Exiting the script")

rmarkdown::render("Formula1.Rmd")

stop("Exiting the script")



















formula1 %>% filter(statusId == 62) %>% select(resultId, grid, positionOrder, points, laps, rank, year, circuit, driverName)

formula1 %>% filter(statusId == 62 & rank != "\\N") %>% select(resultId, grid, positionOrder, points, laps, rank, year, circuit, driverName)

formula1 %>% filter(statusId == 77) %>% select(resultId, grid, positionOrder, points, laps, rank, year, circuit, driverName)
formula1 %>% filter(statusId == 81) %>% select(resultId, grid, positionOrder, points, laps, rank, year, circuit, driverName)

formula1 %>% filter(statusId == 97) %>% select(resultId, grid, positionOrder, points, laps, rank, year, circuit, driverName)

formula1 %>% filter(statusId == 100) %>% select(resultId, grid, positionOrder, points, laps, rank, year, circuit, driverName)

formula1
head(status, 12)
# Extracting column names from the dataset formula1
column_names <- names(formula1)

# Creating an empty descriptions vector
descriptions <- rep("", length(column_names))

# Constructing the new table with empty descriptions
empty_column_table <- data.frame(
  Column = column_names,
  Description = descriptions,
  stringsAsFactors = FALSE
)



rmarkdown::render("Formula1.Rmd")

formula1 %>% filter(points == 0.5)

formula1 %>% filter(circuit == "German Grand Prix" & year == 1994)


short <- formula1 %>% filter(laps < 20 & statusId == 1)

status
formula1
races %>% filter(raceId == 1145)


head(results)
str(results)
summary(results)

max(status$statusId)

unique_position <- unique(results$positionOrder)
print(unique_position)

unique_position <- unique(results$grid)
print(unique_position)

df2 <- results %>% filter(grid == "\\N")
df2

unique_position <- unique(df2$position)
print(unique_position)

unique_positionText <- unique(df2$positionText)
print(unique_positionText)

nrow(results)
nrow(df2)

df3 <- df2 %>% filter(position == "\\N")
df3
unique_positionText <- unique(df3$positionText)
print(unique_positionText)


races %>% filter(raceId == "1167")
drivers %>% filter(driverId == "830")


summary(constructor_results)
df <- constructor_results %>% filter(status != "\\N")
head(df)

unique_statuses <- unique(constructor_results$status)
print(unique_statuses)

# Count the occurrences of each unique value in the 'status' column
status_counts <- table(constructor_results$status)

# Print the counts for each status
print(status_counts)




drivers <- as_tibble(read.csv("data/drivers.csv"))
races <- as_tibble(read.csv("data/races.csv"))
results <- as_tibble(read.csv("data/results.csv"))
status <- as_tibble(read.csv("data/status.csv"))

results <- results %>% select(resultId,
                              raceId,
                              driverId,
                              constructorId,
                              number,
                              grid,
                              position,
                              positionText,
                              positionOrder,
                              points,
                              laps,
                              statusId)

head(results)

#df <- results %>% filter(position != positionText) %>% distinct(statusId) %>% arrange(statusId)
df <- results %>% filter(position != positionText) %>% arrange(statusId)
df %>% filter(points > 0)


driver_standings <- as_tibble(read.csv("data/driver_standings.csv"))
driver_standings %>% filter(position != positionText)

races %>% filter(raceId == "223")
drivers %>% filter(driverId == "30")

driver_standings %>% filter(positionText == "D")
driver_standings %>% filter(position == "\\N")
driver_standings %>% filter(positionText == "-")

library(lubridate)
races %>%
  select(year, date) %>%
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),
    date_year = year(date)
  ) %>%
  filter(year != date_year)

nrow(status)
head(status, 12)


results %>% filter(statusId == 2)
results %>% filter(statusId == 11)

library(stringr)
status %>%
  filter(
    status == "Finished" |
      str_detect(status, "\\+1 Lap") |
      str_detect(status, "\\+\\d+ Laps")
  )

# Extract statusIds to exclude
exclude_status_ids <- status %>%
  filter(
    status == "Finished" |
      str_detect(status, "\\+1 Lap") |
      str_detect(status, "\\+\\d+ Laps")
  ) %>%
  pull(statusId)

# Filter the results to exclude those statusIds
results %>%
  filter(!statusId %in% exclude_status_ids) %>%
  filter(points > 0) %>%
  filter(statusId == 2)

races %>% filter(raceId == "759")
drivers %>% filter(driverId == "475")

results %>%
  filter(!statusId %in% exclude_status_ids) %>%
  filter(statusId == 2)

formula1


results %>%
  filter(statusId == 2) %>%
  filter(position != "\\N")

results %>%
  filter(statusId == 2) %>%
  filter(positionOrder < 7)

results %>%
  filter(positionText == "D") %>%
  filter(statusId != 2)













# Filter drivers who scored at least one point but with status different from 1 (Finished) :
formula1 %>%
  filter(points > 0 & statusId != 1 & (statusId < 11 | statusId > 19) & positionOrder > 2)


results %>%
  filter(points > 0) %>%
  filter(statusId == 2) %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  left_join(status, by = "statusId") %>%
  select(resultId, positionText, positionOrder, points, year, name, forename, surname, status)


