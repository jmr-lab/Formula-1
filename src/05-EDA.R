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
  ungroup() %>%
  group_by(driverName) %>%
  mutate(previous_titles = row_number() - 1) %>%
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

# Summary table
summary_table <- tibble(
  # Number of seasons
  Number_of_Seasons = formula1 %>% select(year) %>% distinct() %>% nrow(),
  # Number of races
  Number_of_Races = formula1 %>% select(year, round) %>% distinct() %>% nrow(),
  # Number of drivers
  Number_of_Drivers = formula1 %>% select(driverName) %>% distinct() %>% nrow(),
  # Number of winners
  Number_of_Winners = formula1 %>% filter(positionOrder == 1) %>% select(driverName) %>% distinct() %>% nrow(),
  # Number of world champions
  Number_of_Champions = formula1 %>%
    group_by(year, driverName) %>%
    summarize(points = max(cumulPoints, na.rm = TRUE), .groups = 'drop') %>%
    group_by(year) %>%
    filter(points == max(points)) %>%
    ungroup() %>%
    select(driverName) %>% distinct() %>% nrow()
)

# Modify column names to have spaces instead of underscores
colnames(summary_table) <- gsub("_", " ", colnames(summary_table))

# View the summary table
summary_table

# Create a complete list of drivers
all_drivers <- formula1 %>%
  distinct(driverName)

# Create the list of drivers and their title counts
driver_titles <- formula1 %>%
  group_by(year) %>%
  slice(which.max(cumulPoints)) %>%
  ungroup() %>%
  count(driverName) %>%
  rename(titles = n)

# Perform a left join to include all drivers
driver_titles <- all_drivers %>%
  left_join(driver_titles, by = "driverName") %>%
  replace_na(list(titles = 0))
driver_titles

# Probability of winning another title
summary_titles <- driver_titles %>%
  group_by(titles) %>%
  summarize(number_of_drivers = n(), .groups = 'drop') %>%
  complete(titles = 1:max(titles), fill = list(number_of_drivers = 0)) %>%
  arrange(desc(titles)) %>%
  mutate(cumul_number_of_drivers = cumsum(number_of_drivers),
         prob_winning = lag(cumul_number_of_drivers) / cumul_number_of_drivers)

plot_titles <- ggplot(summary_titles, aes(x = titles, y = number_of_drivers)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Titles", y = "Drivers") +
  scale_x_continuous(breaks = summary_titles$titles) +
  scale_y_continuous(limits = c(0, 850), breaks = seq(0, 900, by = 100)) +
  theme_minimal() +
  theme(text = element_text(size = 9))

plot_titles_oneplus <- summary_titles %>%
  filter(titles > 0) %>%
  ggplot(aes(x = titles, y = number_of_drivers)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Titles", y = "Drivers") +
  scale_x_continuous(breaks = summary_titles$titles) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Create the list of drivers and their win counts
driver_wins <- formula1 %>%
  filter(positionOrder == 1) %>%
  count(driverName) %>%
  rename(wins = n)

# Perform a left join to include all drivers
driver_wins <- all_drivers %>%
  left_join(driver_wins, by = "driverName") %>%
  replace_na(list(wins = 0))
driver_wins

# Probability of winning another race
summary_wins <- driver_wins %>%
  group_by(wins) %>%
  summarize(number_of_drivers = n(), .groups = 'drop') %>%
  complete(wins = 1:max(wins), fill = list(number_of_drivers = 0)) %>%
  arrange(desc(wins)) %>%
  mutate(cumul_number_of_drivers = cumsum(number_of_drivers),
         prob_winning = lag(cumul_number_of_drivers) / cumul_number_of_drivers)
summary_wins

# Create dataframes for title & win categories
title_categories_oneplus <- paste0(seq(1, 7, by = 1), "+")
title_categories <- c("0+", title_categories_oneplus)
win_categories_oneplus <- paste0(seq(1, 101, by = 10), "+")
win_categories <- c("0+", win_categories_oneplus)

# Group by win_category and count the number of drivers in each category
title_counts <- driver_titles %>%
  rowwise() %>%
  mutate(title_category = list(title_categories[titles >= as.numeric(sub("\\+", "", title_categories))])) %>%
  unnest(title_category) %>%
  group_by(driverName, titles, title_category) %>%
  summarise(driver_count = n(), .groups = 'drop') %>%
  group_by(title_category) %>%
  summarise(driver_count = n()) %>%
  arrange(factor(title_category, 
                 levels = title_categories))

# Display the result
print(title_counts)

# Group by win_category and count the number of drivers in each category
win_counts <- driver_wins %>%
  rowwise() %>%
  mutate(win_category = list(win_categories[wins >= as.numeric(sub("\\+", "", win_categories))])) %>%
  unnest(win_category) %>%
  group_by(driverName, wins, win_category) %>%
  summarise(driver_count = n(), .groups = 'drop') %>%
  group_by(win_category) %>%
  summarise(driver_count = n()) %>%
  arrange(factor(win_category, 
                 levels = c("0+", "1+", "11+", "21+", "31+", "41+", 
                            "51+", "61+", "71+", "81+", "91+", "101+")))

# Display the result
print(win_counts)

# Create the bar plot using ggplot2
plot_titles <- title_counts %>%
  ggplot(aes(x = title_category, y = driver_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Titles", y = "Drivers") +
  scale_x_discrete(limits = title_categories) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 100)) +
  theme_minimal() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Same histogram without the drivers who didn't win a race
plot_titles_oneplus <- title_counts %>%
  filter(title_category != "0+") %>%
  ggplot(aes(x = title_category, y = driver_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Titles", y = "Drivers") +
  scale_x_discrete(limits = title_categories_oneplus) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 10)) +
  theme_minimal() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Create the bar plot using ggplot2
plot_wins <- win_counts %>%
  ggplot(aes(x = win_category, y = driver_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Wins", y = "Drivers") +
  scale_x_discrete(limits = win_categories) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 100)) +
  theme_minimal() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Same histogram without the drivers who didn't win a race
plot_wins_oneplus <- win_counts %>%
  filter(win_category != "0+") %>%
  ggplot(aes(x = win_category, y = driver_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Wins", y = "Drivers") +
  scale_x_discrete(limits = win_categories_oneplus) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 10)) +
  theme_minimal() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Plotting probability of winning a new title
plot_prob_titles <- summary_titles %>%
  filter(!is.na(prob_winning) & titles < 6) %>%
  ggplot(aes(x = titles, y = prob_winning)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(x = "Titles", y = "Probability of Winning") +
  scale_x_continuous(breaks = summary_titles$titles) +
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 1, by = 0.1), 
                     limits = c(0, 1)) +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Plotting probability of winning a new race
plot_prob_wins <- summary_wins %>%
  filter(!is.na(prob_winning) & wins < 15) %>%
  ggplot(aes(x = wins, y = prob_winning)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(x = "Wins", y = "Probability of Winning") +
  scale_x_continuous(breaks = summary_wins$wins) +
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 1, by = 0.1), 
                     limits = c(0, 1)) +
  theme_minimal() +
  theme(text = element_text(size = 9))

# Plot the 2 graphs :
plot_grid(
  plot_titles,
  plot_wins,
  ncol = 2, align = 'hv'
)

# Plot the 2 graphs :
plot_grid(
  plot_titles_oneplus,
  plot_wins_oneplus,
  ncol = 2, align = 'hv'
)

# Plot the 2 graphs (probs) :
plot_grid(
  plot_prob_titles,
  plot_prob_wins,
  ncol = 2, align = 'hv'
)

# Drivers who were able to become world champion at least once and won less than 11 GP :
wc_low_wins <- driver_titles %>%
  filter(titles > 0) %>%
  inner_join(driver_wins %>% filter(wins < 11), by = "driverName") %>%
  left_join(world_champions, by = "driverName") %>%
  mutate(countryImage = paste0("images/icons8-", gsub(" ", "-", tolower(driverCountry)), "-50.png")) %>%
  select(countryImage, driverName, titles, wins, year) %>%
  arrange(year)
wc_low_wins

ggplot(wc_low_wins, aes(x = year, y = wins, group = driverName)) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Year", y = "Number of Wins") +
  scale_x_continuous(breaks = seq(1950, 2025, by = 10), limits = c(1950, 2025)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5), limits = c(0, 100)) +
  theme_minimal()

# Drivers who were never able to become world champion but won at least 11 GP :
wc_high_wins <- driver_titles %>%
  filter(titles == 0) %>%
  inner_join(driver_wins %>% filter(wins > 10), by = "driverName") %>%
  left_join(f1_summary, by = "driverName") %>%
  select(countryImage, driverName, wins = wins.x, career_start, career_end, career_length)

# Remove underscores from the column names
wc_high_wins <- wc_high_wins %>%
  rename_with(.fn = ~ gsub("_", "", .), .cols = everything())
wc_high_wins

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
  scale_color_manual(values = custom_colours, name = "Era") +
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
  geom_text_repel(size = 2, max.overlaps = Inf) +
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

# Races per year
races_per_year <- formula1 %>%
  group_by(year) %>%
  summarize(nbraces = max(round), .groups = 'drop')
plot_races_per_year <- races_per_year %>%
  ggplot(aes(x = year, y = nbraces)) +
  geom_line(color = "gray30") +
  geom_point(color = "gray50", size = 1) +
  geom_smooth(method = "loess", span = 0.3, formula = y ~ x, se = TRUE, color = "royalblue") +
  labs(x = "Year", y = "Races") +
  scale_x_continuous(breaks = seq(min(races_per_year$year), max(races_per_year$year), by = 10)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top")
plot_races_per_year

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
performance_stats %>% filter(year == 2025)
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

performance_stats %>%
  arrange(-percentage_wins) %>%
  mutate(countryImage = paste0("images/icons8-", gsub(" ", "-", tolower(driverCountry)), "-50.png"),
         driverName = paste0("\\includegraphics[width=0.25cm]{", countryImage, "}", " ", driverName),
         percentage_wins = round(percentage_wins, 1)) %>%
  select(year,
         driverName,
         nbraces = total_races,
         wins,
         winspc = percentage_wins) %>%
  head(20)

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

# Constructors
constructorResults <- formula1 %>%
  group_by(year, constructorCountry, constructorName) %>%
  summarise(wins = sum(positionOrder == 1, na.rm = TRUE),
            points = sum(points),
            .groups = 'drop') %>%
  group_by(year) %>%
  arrange(desc(points)) %>%
  mutate(positionOrder = row_number(),
         countryImage = paste0("images/icons8-", gsub(" ", "-", tolower(constructorCountry)), "-50.png")) %>%
  ungroup() %>%
  arrange(year, -points)
constructorResults

constructorSummary <- constructorResults %>%
  group_by(constructorCountry, countryImage, constructorName) %>%
  summarise(titles = sum(positionOrder == 1, na.rm = TRUE),
            wins = sum(wins),
            points = sum(points),
            start = min(year),
            end = max(year),
            seasons = n_distinct(year),
            .groups = 'drop') %>%
  arrange(-titles, -wins, -points, -seasons)
constructorSummary

constructorResults %>% filter(constructorName == "Mercedes")

# Constructor Summary table
constructor_summary_table <- tibble(
  # Number of constructors
  Number_of_Constructors = nrow(constructorSummary),
  # Number of constructors who won points (at least one)
  Points = nrow(constructorSummary %>% filter(points > 0)),
  # Number of constructors who won races (at least one)
  Wins = nrow(constructorSummary %>% filter(wins > 0)),
  # Number of world champions
  Titles = nrow(constructorSummary %>% filter(titles > 0)),
)

# Modify column names to have spaces instead of underscores
colnames(constructor_summary_table) <- gsub("_", " ", colnames(constructor_summary_table))

# View the constructor summary table
constructor_summary_table

# Plot the constructorResults :
plotConstructorResults <- constructorResults %>%
  filter(positionOrder <= 2) %>%
  ggplot(aes(x = year, y = wins, color = factor(positionOrder))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 5)) +
  labs(x = "Year", y = "Wins") +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  scale_color_discrete(name = "Position")
plotConstructorResults

# Drivers
driverResults <- formula1 %>%
  group_by(year, driverCountry, driverName) %>%
  summarise(wins = sum(positionOrder == 1, na.rm = TRUE),
            points = sum(points),
            .groups = 'drop') %>%
  group_by(year) %>%
  arrange(desc(points)) %>%
  mutate(positionOrder = row_number(),
         countryImage = paste0("images/icons8-", gsub(" ", "-", tolower(driverCountry)), "-50.png")) %>%
  ungroup() %>%
  arrange(year, -points)
driverResults

# Plot the driverResults :
plotDriverResults <- driverResults %>%
  filter(positionOrder <= 2) %>%
  ggplot(aes(x = year, y = wins, color = factor(positionOrder))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 5)) +
  labs(x = "Year", y = "Wins") +
  theme_minimal() +
  theme(text = element_text(size = 9), legend.position = "top") +
  scale_color_discrete(name = "Position")
plotDriverResults

plot_grid(
  plotConstructorResults,
  plotDriverResults,
  nrow = 2, align = 'hv'
)

# Probabilities of winning a race or title :
world_champions

# Consecutive streaks (drivers):
consecutive_streaks_drivers <- world_champions %>%
  select(driverName, driverCountry, year) %>%
  arrange(driverName, year) %>%
  group_by(driverName, driverCountry) %>%
  mutate(
    new_block = is.na(lag(year)) | (year != lag(year) + 1),
    block_id = cumsum(new_block)
  ) %>%
  group_by(driverName, driverCountry, block_id) %>%
  summarise(
    nb_consecutive_titles = n(),
    years = paste(year, collapse = ","),
    first_year = min(year),
    .groups = "drop"
  ) %>%
  select(-block_id) %>%
  filter(nb_consecutive_titles >= 2) %>%
  arrange(desc(nb_consecutive_titles), driverName)

ggplot(consecutive_streaks_drivers, aes(x = first_year, y = nb_consecutive_titles, color = driverName)) +
  geom_point(show.legend = FALSE, size = 3) +
  geom_text_repel(aes(label = driverName), size = 3, show.legend = FALSE) +
  scale_x_continuous(
    limits = c(1950, 2025),      # Start at 1950, extend to 2025
    breaks = seq(1950, 2025, by = 10)  # Every 10 years: 1950, 1960, 1970, ...
  ) +
  expand_limits(y = 0) +
  guides(color = "none") +
  labs(
    title = "Consecutive World Championship Streaks (Drivers)",
    x = "First year of streak",
    y = "Number of consecutive titles"
  ) +
  theme_minimal()

# Consecutive streaks (constructors):
consecutive_streaks_constructors <- constructorResults %>%
  filter(positionOrder == 1) %>%
  select(constructorName, constructorCountry, year) %>%
  arrange(constructorName, year) %>%
  group_by(constructorName, constructorCountry) %>%
  mutate(
    new_block = is.na(lag(year)) | (year != lag(year) + 1),
    block_id = cumsum(new_block)
  ) %>%
  group_by(constructorName, constructorCountry, block_id) %>%
  summarise(
    nb_consecutive_titles = n(),
    years = paste(year, collapse = ","),
    first_year = min(year),
    .groups = "drop"
  ) %>%
  select(-block_id) %>%
  filter(nb_consecutive_titles >= 2) %>%
  arrange(desc(nb_consecutive_titles), constructorName)

ggplot(consecutive_streaks_constructors, aes(x = first_year, y = nb_consecutive_titles, color = constructorName)) +
  geom_point(show.legend = FALSE, size = 3) +
  geom_text_repel(aes(label = constructorName), size = 3, show.legend = FALSE) +
  scale_x_continuous(
    limits = c(1950, 2025),      # Start at 1950, extend to 2025
    breaks = seq(1950, 2025, by = 10)  # Every 10 years: 1950, 1960, 1970, ...
  ) +
  expand_limits(y = 0) +
  guides(color = "none") +
  labs(
    title = "Consecutive World Championship Streaks (Constructors)",
    x = "First year of streak",
    y = "Number of consecutive titles"
  ) +
  theme_minimal()