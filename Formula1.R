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
circuits <- circuits %>% select(-lat, -lng, -url)

# Note sure if we need to keep the constructors tables
constructor_results <- constructor_results %>% select(-status)
constructor_standings <- constructor_standings %>% select(-positionText)
constructors <- constructors %>% select(-url)

# Driver's columns
driver_standings <- driver_standings %>% select(-positionText)
drivers <- drivers %>% select(-number, -url)

# Not sure of the races table is really needed
races <- races %>% select(raceId, year, round, circuitId, name, date)

# Results' columns
results <- results %>% select(-number, -time, -milliseconds, -fastestLap, -fastestLapTime, -fastestLapSpeed,
                              -position, -positionText)

# Formula 1 Dataset : from results table, we add race, driver and contructor names
# Join the results and races data frames
formula1 <- results %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  left_join(constructors, by = "constructorId")

# Remove unneeded columns
formula1 <- formula1 %>% select(-raceId, -driverId, -constructorId, -statusId, -circuitId)

# Rename columns
formula1 <- formula1 %>%
  rename(
    circuit = name.x,
    constructorName = name.y,
    driverNationality = nationality.x,
    constructorNationality = nationality.y
  )

# Print the resulting formula1 table
print(formula1)

# Stop execution
stop("Exiting the script")





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