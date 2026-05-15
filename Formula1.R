#############################################################
# 2. Dataset                                                #
#############################################################

#library(rsvg)
#library(DiagrammeR)
library(dplyr)
library(tidyr)
library(dm)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(kableExtra)

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

# Read Countries CSV file
countries <- as_tibble(read.csv("data/Countries.csv"))

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

# DriverStanding's columns
driver_standings_df <- driver_standings %>% select(driverStandingsId, raceId, driverId, cumulPoints = points)

# Driver's columns
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
  left_join(status_df, by = "statusId") %>%
  left_join(driver_standings_df, by = c("raceId", "driverId")) %>%
  mutate(cumulPoints = replace_na(cumulPoints, 0))

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

# Replace demonyms or country adjectives by country names :
formula1 <- formula1 %>%
  mutate(driverNationality = trimws(driverNationality), 
         constructorNationality = trimws(constructorNationality)) %>%
  left_join(countries %>% rename(driverCountry = Country), by = c("driverNationality" = "Adjective")) %>%
  select(-driverNationality) %>%
  left_join(countries %>% rename(constructorCountry = Country), by = c("constructorNationality" = "Adjective")) %>%
  select(-constructorNationality)

# Only keep data for the previous years :
current_year <- as.numeric(format(Sys.Date(), "%Y"))
formula1 <- formula1 %>%
  filter(year < current_year)

# Print the resulting formula1 table
print(formula1)
formula1 %>%
  select(resultId, grid, positionOrder, cumulPoints, points, year, round, circuit, driverName, driverAge, constructorName) %>%
  head()

#############################################################
# 4. Exploratory Data Analysis                              #
#############################################################

formula1 %>% select(resultId, positionOrder, cumulPoints, points, year, driverName, driverCountry, constructorName, constructorCountry)

formula1 %>% select(cumulPoints, points, positionOrder, year, constructorName, constructorCountry, driverName, driverCountry) %>%
  group_by(year, driverName, driverCountry, constructorName, constructorCountry) %>%
  summarize(total_cumul_points = max(cumulPoints, na.rm = TRUE),
            total_points = sum(points, na.rm = TRUE),
            wins = sum(positionOrder == 1),
            .groups = 'drop')

# Calculate total points for each driver in winning years
world_champions <- formula1 %>%
  select(cumulPoints, points, positionOrder, year, constructorName, driverName, driverCountry) %>%
  group_by(year, driverName, driverCountry) %>%
  summarize(total_points = sum(points, na.rm = TRUE),
            total_cumul_points = max(cumulPoints, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(year) %>%
  filter(total_cumul_points == max(total_cumul_points) & year < 2026) %>%
  ungroup()

# Get the first and last year a driver won a title
#first_year <- world_champions %>%
#  group_by(driverName) %>%
#  summarize(first_year = min(year), .groups = 'drop')
#first_year
titles_period <- world_champions %>%
  group_by(driverName) %>%
  summarize(title_first = min(year), title_last = max(year), .groups = 'drop')
titles_period

# Counting titles and total career points per driver
#titles_count <- world_champions %>%
#  group_by(driverCountry, driverName) %>%
#  summarize(titles = n(), .groups = 'drop') %>%
#  arrange(desc(titles))
titles_count <- world_champions %>%
  group_by(driverName) %>%
  summarize(titles = n(), .groups = 'drop') %>%
  arrange(desc(titles)) %>%
  left_join(titles_period, by = "driverName")

# Calculating total career points regardless of titles
career <- formula1 %>%
  group_by(driverCountry, driverName) %>%
  summarize(wins = sum(positionOrder == 1, na.rm = TRUE),
            races = n(),
            seasons = n_distinct(year),
            points = sum(points, na.rm = TRUE),
            career_start = min(year),
            career_end = max(year),
            career_length = n_distinct(year),
            .groups = 'drop')

# Replace country name by link to image
career <- career %>%
  mutate(countryImage = paste0("images/icons8-", gsub(" ", "-", tolower(driverCountry)), "-50.png"))

# Joining titles and career points data frames
f1_summary <- titles_count %>%
  right_join(career, by = "driverName") %>%
  arrange(desc(titles), desc(wins)) %>%
  mutate(titles = replace_na(titles, 0))

# Add win and title ratios
f1_summary <- f1_summary %>%
  mutate(ratio_titles = round(titles / seasons, 3),
         ratio_wins = round(wins / races, 3))

# Rearrange columns: move driverCountry to the first position and countryImage to the last
f1_summary <- f1_summary %>%
  select(driverCountry, everything(), countryImage)

# Round the ratio_wins column to 3 decimal places
#f1_summary <- f1_summary %>%
#  mutate(ratio_titles = round(ratio_titles, 3),
#         ratio_wins = round(ratio_wins, 3))

# Categorize into decades
#f1_summary <- f1_summary %>%
#  mutate(period_start = paste0(floor(title_first / 10) * 10, "s"))
f1_summary <- f1_summary %>%
  mutate(period_start = case_when(
    title_first >= 1950 & title_first <= 1968 ~ "1950-1968",
    title_first >= 1969 & title_first <= 1993 ~ "1969-1993",
    title_first >= 1994 & title_first <= 2025 ~ "1994-2025",
    TRUE ~ NA_character_
  ))

# Display the final result with titles and total career points
print(f1_summary)

# Filter f1_summary to keep drivers with 2 or more titles
# and convert to factor :
filtered_drivers <- f1_summary %>% 
  filter(titles > 1) %>% 
  arrange(career_start) %>%  # Order by first_year
  mutate(driverName = factor(driverName, levels = driverName))

# Careers
career_segments <- ggplot(filtered_drivers, aes(y = driverName)) +
  geom_segment(aes(x = career_start, xend = career_end, yend = driverName), 
               size = 2, color = "royalblue") +
  scale_x_continuous(breaks = seq(1950, 2025, by = 5), name = "Years") +
  labs(y = "Driver Name") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Display the plot
print(career_segments)

# Titles
title_segments <- ggplot(filtered_drivers, aes(y = driverName)) +
  geom_segment(aes(x = title_first, xend = title_last, yend = driverName), 
               size = 2, color = "seagreen") +
  geom_vline(xintercept = c(1968, 1993), linetype = "dashed", color = "darkgrey") +
  scale_x_continuous(breaks = seq(1950, 2025, by = 5), name = "Years") +
  labs(y = "Driver Name") +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Display the plot
print(title_segments)

# Career length
career_data <- f1_summary %>%
  filter(titles > 1) %>%
  arrange(title_first) %>%  # Order by first_year
  mutate(driverName = factor(driverName, levels = driverName)) %>%
  select(driverName, titles, title_first, career_length)

# Get the blues palette with 7 colours
blues_palette <- brewer.pal(7, "Blues")

# Define custom colours based on blues_palette
custom_colours <- c(
  "2" = blues_palette[3],
  "3" = blues_palette[4],
  "4" = blues_palette[5],
  "5" = blues_palette[6],
  "7" = blues_palette[7]
)

career_lengths <- ggplot(career_data, aes(x = title_first, y = career_length, color = as.factor(titles))) +
  geom_point(size = 2) +
  geom_vline(xintercept = c(1968, 1993), linetype = "dashed", color = "darkgrey") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", color = "darkgrey") +
  scale_color_manual(values = custom_colours, name = "Titles") +
  scale_x_continuous(breaks = seq(1950, 2025, by = 5), name = "First Title") +
  scale_y_continuous(breaks = seq(0, max(career_data$career_length, na.rm = TRUE), by = 5), 
                     limits = c(0, NA), 
                     name = "Career Length") +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "left")

# Display the plot
print(career_lengths)

# Calculate the baseline ratios wins / races and titles / seasons
# I will use Ayrton Senna data as a baseline :
ratio_wins_baseline <- f1_summary %>% filter(driverName == "Ayrton Senna") %>% pull(ratio_wins)
ratio_titles_baseline <- f1_summary %>% filter(driverName == "Ayrton Senna") %>% pull(ratio_titles)

# Set seed for reproducibility
set.seed(123)

# Define custom colours
custom_colours <- c(
  "1950-1968" = "tomato",
  "1969-1993" = "wheat3",
  "1994-2025" = "steelblue"
)

# Wins vs Races plot
wins_vs_races <- f1_summary %>%
  filter(titles > 1) %>%
  ggplot(aes(x = races, y = wins, label = driverName)) +
  geom_point(aes(color = period_start), size = 2) +
  scale_color_manual(values = custom_colours, name = "Titles") +
  geom_text_repel(size = 2) +
  labs(x = "Races", y = "Wins") +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  xlim(0, max(f1_summary$races, na.rm = TRUE)) +
  ylim(0, max(f1_summary$wins, na.rm = TRUE)) +
  geom_abline(slope = ratio_wins_baseline, intercept = 0, linetype = "dashed", color = "darkgrey")
wins_vs_races

# Titles vs Seasons plot
titles_vs_seasons <- f1_summary %>%
  filter(titles > 1) %>%
  ggplot(aes(x = seasons, y = titles, label = driverName)) +
  geom_point(aes(color = period_start), size = 2) +
  scale_color_manual(values = custom_colours, name = "Titles") +
  geom_text_repel(size = 2) +
  labs(x = "Seasons", y = "Titles") +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  xlim(0, max(f1_summary$seasons, na.rm = TRUE)) +
  ylim(0, max(f1_summary$titles, na.rm = TRUE)) +
  geom_abline(slope = ratio_titles_baseline, intercept = 0, linetype = "dashed", color = "darkgrey")
titles_vs_seasons

# Number of points compared to number of total points (per driver)

# Number of points per race given to the winner, second, third
# Total number of points per race
plot_points_per_race <- formula1 %>%
  group_by(round, year) %>%
  summarize(total_points = sum(points, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = interaction(round, year), y = total_points)) +
  geom_bar(stat = "identity") +
  labs(x = "Round and Year",
       y = "Total Points") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme_minimal()
plot_points_per_race

# Points per year
points_per_year <- formula1 %>%
  group_by(year, positionOrder) %>%
  summarize(points = as.numeric(names(which.max(table(points)))), .groups = 'drop') %>%
  mutate(points = case_when(
    year <= 1959 & positionOrder == 1 ~ 8,
    year <= 1959 & positionOrder == 2 ~ 6,
    year <= 1959 & points == 1 ~ 0,
    TRUE ~ points
  )) %>%
  filter(points > 0)
plot_points_per_year <- points_per_year %>%
  ggplot(aes(x = year, y = points, color = as.factor(positionOrder), group = positionOrder)) +
  geom_line() +
  geom_point(size = 1) +
  labs(x = "Year", y = "Points", color = "Position Order") +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")
plot_points_per_year

# Ratio winner / second, second / third overtime
percentage_diff <- points_per_year %>%
  group_by(year) %>%
  summarise(
    `1-2` = (points[positionOrder == 1] - points[positionOrder == 2]) / points[positionOrder == 2] * 100,
    `2-3` = (points[positionOrder == 2] - points[positionOrder == 3]) / points[positionOrder == 3] * 100,
    `3-4` = (points[positionOrder == 3] - points[positionOrder == 4]) / points[positionOrder == 4] * 100,
    `4-5` = (points[positionOrder == 4] - points[positionOrder == 5]) / points[positionOrder == 5] * 100
  ) %>%
  pivot_longer(
    cols = `1-2`:`4-5`,  # Specify the columns to pivot
    names_to = "Position_Difference", 
    values_to = "Percentage_Difference"
  )

# Display the result
plot_percentage_diff <- percentage_diff %>%
  filter(Position_Difference%in%c("1-2", "2-3", "3-4")) %>%
  ggplot(aes(x = year, y = Percentage_Difference, color = as.factor(Position_Difference), group = Position_Difference)) +
  geom_line() +
  geom_point(size = 1) +
  labs(x = "Year", y = "Percentage Difference", color = "Position Difference") +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")
plot_percentage_diff

# Total number of points per year
plot_total_points_per_year <- formula1 %>%
  group_by(year) %>%
  summarize(total_points = sum(points, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = year, y = total_points)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Total Points") +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")
plot_total_points_per_year

# Difference between world champion
# and driver who scored the max points :
#head(world_champions)
wc_revised <- formula1 %>%
  select(cumulPoints, points, positionOrder, year, constructorName, driverName, driverCountry) %>%
  group_by(year, driverName, driverCountry) %>%
  summarize(total_points = sum(points, na.rm = TRUE),
            total_cumul_points = max(cumulPoints, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(year) %>%
  filter(total_points == max(total_points) & year < 2026) %>%
  ungroup()

# Merge dfA and dfB based on year
merged_data <- world_champions %>%
  inner_join(wc_revised, by = "year", suffix = c("_Champion", "_Best"))

# Filter for rows where driverName or driverCountry differ
differences <- merged_data %>%
  filter(driverName_Champion != driverName_Best | driverCountry_Champion != driverCountry_Best) %>%
  mutate(points_Champion = paste(total_points_Champion, "(", total_cumul_points_Champion, ")", sep = ""),
         points_Best = paste(total_points_Best, "(", total_cumul_points_Best, ")", sep = ""),
         countryImage_Champion = paste0("images/icons8-", gsub(" ", "-", tolower(driverCountry_Champion)), "-50.png"),
         countryImage_Best = paste0("images/icons8-", gsub(" ", "-", tolower(driverCountry_Best)), "-50.png")) %>%
  select(year,
         countryImage_Champion,
         driverName_Champion,
         points_Champion,
         countryImage_Best,
         driverName_Best,
         points_Best)

# Display the resulting table
differences

# Calculate points, cumulPoints, and additional performance metrics 
performance_stats <- formula1 %>%
  #  filter(year < 1991) %>%
  group_by(year, driverName, driverCountry) %>%
  summarise(
    totalPoints = sum(points, na.rm = TRUE),
    cumulPoints = max(cumulPoints, na.rm = TRUE),
    total_races = n(),  # Total races the driver participated in
    wins = sum(positionOrder == 1),  # Wins
    podiums = sum(positionOrder <= 3),  # Podiums
    points_count = sum(points > 0)  # Points earned
  ) %>%
  # Retain only the world champion for each year
  group_by(year) %>%
  filter(cumulPoints == max(cumulPoints)) %>%
  ungroup() %>%
  mutate(
    percentage_wins = (wins / total_races) * 100,
    percentage_podiums = (podiums / total_races) * 100,
    percentage_points = (points_count / total_races) * 100
  )

# View the result
print(performance_stats)

# Variation of percentage win over the years
plot_percentage_wins <- ggplot(performance_stats, aes(x = year, y = percentage_wins)) +
  geom_line(color = "grey", size = 0.7) +
  geom_point(color = "darkgrey", size = 2) +
  geom_smooth(method = "loess", span = 0.4, formula = y ~ x, se = TRUE, linetype = "dashed", color = "royalblue", fill = "lightgrey") +
  geom_vline(xintercept = c(1961, 1991, 2003, 2010), linetype = "dashed", color = "black") +
  labs(x = "Year", y = "Percentage Wins") +
  scale_x_continuous(breaks = seq(min(performance_stats$year), 
                                  max(performance_stats$year), 
                                  by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(text = element_text(size = 9))
plot_percentage_wins

# Variation of percentage podium over the years
plot_percentage_podiums <- ggplot(performance_stats, aes(x = year, y = percentage_podiums)) +
  geom_line(color = "grey", size = 0.7) +
  geom_point(color = "darkgrey", size = 2) +
  geom_smooth(method = "loess", span = 0.4, formula = y ~ x, se = TRUE, linetype = "dashed", color = "seagreen", fill = "lightgrey") +
  geom_vline(xintercept = c(1961, 1991, 2003, 2010), linetype = "dashed", color = "black") +
  labs(x = "Year", y = "Percentage Podiums") +
  scale_x_continuous(breaks = seq(min(performance_stats$year), 
                                  max(performance_stats$year), 
                                  by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(text = element_text(size = 9))
plot_percentage_podiums

# Variation of percentage points over the years
plot_percentage_points <- ggplot(performance_stats, aes(x = year, y = percentage_points)) +
  geom_line(color = "grey", size = 0.7) +
  geom_point(color = "darkgrey", size = 2) +
  geom_smooth(method = "loess", span = 0.4, formula = y ~ x, se = TRUE, linetype = "dashed", color = "brown", fill = "lightgrey") +
  geom_vline(xintercept = c(1961, 1991, 2003, 2010), linetype = "dashed", color = "black") +
  labs(x = "Year", y = "Percentage Points") +
  scale_x_continuous(breaks = seq(min(performance_stats$year), 
                                  max(performance_stats$year), 
                                  by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(text = element_text(size = 9))
plot_percentage_points

# Stop execution
stop("Exiting the script")

rmarkdown::render("Formula1.Rmd")

stop("Exiting the script")



formula1 %>%
#  filter(year == 1964) %>%
  filter(year == 1954) %>%
  group_by(driverName, driverCountry) %>%
  summarise(points = sum(points),
            cumulPoints = max(cumulPoints)) %>%
  arrange(-points)

df <- formula1 %>%
  filter(year < 1991) %>%
  group_by(year, driverName, driverCountry) %>%
  summarise(points = sum(points),
            cumulPoints = max(cumulPoints)) %>%
  filter(points != cumulPoints) %>%
  arrange(-year)

formula1 %>%
  filter(year < 1991) %>%
  group_by(year, driverName, driverCountry) %>%
  summarise(points = sum(points),
            cumulPoints = max(cumulPoints))


#

performance_stats %>%
  select(year, percentage_wins, percentage_podiums) %>%
  pivot_longer(cols = c(percentage_wins, percentage_podiums), 
               names_to = "Metric", 
               values_to = "Percentage") %>%
  ggplot(aes(x = year, y = Percentage, color = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Driver Performance Metrics Over the Years",
       x = "Year",
       y = "Percentage",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "right")
#












































#

df <- formula1 %>% filter(driverName == "Graham Hill") %>% pull(year)
max(df)
#


#


# Categorize into decades
f1_summary <- f1_summary %>%
  mutate(period_start = case_when(
    first_year >= 1950 & first_year < 1975 ~ "1950-1975",
    first_year >= 1975 & first_year < 1994 ~ "1975-1994",
    first_year >= 1994 & first_year < 2008 ~ "1994-2008",
    first_year >= 2008 & first_year <= 2025 ~ "2008-2025",
    TRUE ~ NA_character_
  ))



world_champions
top_names <- head(f1_summary, 20) %>% select(driverName)

filtered_champions <- world_champions %>% semi_join(top_names, by = "driverName")
#


f1_summary %>%
  filter(titles > 1) %>%
  mutate(driverName = paste0("\\includegraphics[width=0.25cm]{", countryImage, "}", " ", driverName) ) %>%
  select(-driverCountry, -countryImage,
         -title_first, -title_last,
         -career_start, -career_end,
         winspc = ratio_wins, titlespc = ratio_titles)
#



























f1_summary$driverCountry

df <-formula1 %>% filter(is.na(formula1$driverCountry))
df$driverCountry
df$driverName

drivers %>% filter(forename == "Charles" & surname == "Leclerc") %>% pull(nationality)
drivers %>% filter(forename == "Liam" & surname == "Lawson") %>% pull(nationality)
drivers %>% filter(forename == "Franco" & surname == "Colapinto") %>% pull(nationality)
drivers %>% filter(forename == "Pastor" & surname == "Maldonado") %>% pull(nationality)
drivers %>% filter(forename == "Juan" & surname == "Fangio") %>% pull(nationality)

drivers %>% filter(forename == "Eliseo" & surname == "Salazar") %>% pull(nationality)
drivers %>% filter(forename == "Rikky" & surname == "von Opel") %>% pull(nationality)
drivers %>% filter(forename == "John" & surname == "Love") %>% pull(nationality)

drivers %>% filter(forename == "Alfonso" & surname == "Thiele") %>% pull(nationality)
drivers %>% filter(forename == "Alessandro" & surname == "de Tomaso") %>% pull(nationality)
drivers %>% filter(forename == "Rudolf" & surname == "Krause") %>% pull(nationality)
drivers %>% filter(forename == "Ernst" & surname == "Klodwig") %>% pull(nationality)
drivers %>% filter(forename == "Theo" & surname == "Fitzau") %>% pull(nationality)

drivers$nationality
# Get distinct nationalities and sort them alphabetically
distinct_nationalities <- sort(unique(drivers$nationality))

# Display the result
print(distinct_nationalities)

drivers %>% filter(nationality == "American-Italian")
drivers %>% filter(nationality == "Argentine-Italian")
#

countries <- as_tibble(read.csv("data/Countries.csv"))
countries
f1_summary <- f1_summary %>%
  left_join(countries, by = "driverName")

formula1$driverCountry
sort(unique(formula1$driverCountry))
#






























library(countrycode)
# Vector of nationalities
nationalities <- c("German", "American", "Canadian", "French")

# Convert to country names
countries <- countrycode(nationalities, "country.name", "country.name", custom_match = TRUE)

# Display the results
print(countries)


demonym <- countryname("United States", destination = "demonym")
print(demonym)  # Output: "American"



countries <- c("United States", "Canada", "France")
demonyms <- countryname(countries, destination = "demonym")
print(demonyms)



cldr_examples
driver_standings

drivers %>% filter(forename == "Alain" & surname == "Prost")
drivers %>% filter(forename == "Ayrton" & surname == "Senna")

driver_standings %>% filter(driverId == 117)

raceListIds <- races %>% filter(year == 1989) %>% pull(raceId)
driver_standings %>% filter(driverId == 117 & raceId%in%raceListIds)

raceListIds <- races %>% filter(year == 1988) %>% pull(raceId)
driver_standings %>% filter(driverId == 117 & raceId%in%raceListIds)
driver_standings %>% filter(driverId == 102 & raceId%in%raceListIds)

results %>% filter(driverId == 117 & raceId%in%raceListIds)

results
driver_standings
races
# Get the latest record per year and driverId
latest_standing <- driver_standings %>%
  group_by(year, driverId) %>%
  slice_max(driverStandingsId, with_ties = FALSE) %>%  # Use driverStandingsId or raceId here
  ungroup()

# Display the result
print(latest_standing)
#










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


