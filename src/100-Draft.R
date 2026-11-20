#############################################################
# 3. END Exploratory Data Analysis                          #
#############################################################

stop("Stopping here")

formula1 %>% select(resultId, positionOrder, cumulPoints, points, year, driverName, driverCountry, constructorName, constructorCountry)

formula1 %>% select(cumulPoints, points, positionOrder, year, constructorName, constructorCountry, driverName, driverCountry) %>%
  group_by(year, driverName, driverCountry, constructorName, constructorCountry) %>%
  summarize(total_cumul_points = max(cumulPoints, na.rm = TRUE),
            total_points = sum(points, na.rm = TRUE),
            wins = sum(positionOrder == 1),
            .groups = 'drop')

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

performance_stats %>%
  arrange(-percentage_wins) %>%
  mutate(countryImage = paste0("images/flags/icons8-", gsub(" ", "-", tolower(driverCountry)), "-50.png"),
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

constructorResults %>% filter(constructorName == "Mercedes")

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