library(dplyr)
library(ggplot2)

# Data ====
babynames <- readRDS("Data/babynames.rds")
glimpse(babynames)

# Manipulation ====
babynames %>%
  # Filter for the year 1990
  filter(year == 1990) %>%
  # Sort the number column in descending order 
  arrange(desc(number))
# The most common name for babies in the US were Michael 
# christopher and Jessica.

# Find the most common name in each year ====
# Using top_n and group_by()
babynames %>%
  group_by(year) %>%
  top_n(1, number)

# Using Ggplot2 for visualizing names ====
# Filter for the names Steven, Thomas, and Matthew 
selected_names <- babynames %>%
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Plot the names using a different color for each name
ggplot(selected_names, 
       aes(x = year, 
           y = number, 
           color = name)) +
  geom_line()

# Groped mutates ====
# Calculate the fraction of people born each year with the same name
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>%
  # Find the year each name is most common
  group_by(name) %>%
  top_n(1, fraction)

# Adding the total and maximum for each name ====
# Add columns name_total and name_max for each name
babynames %>%
  group_by(name) %>%
  mutate(name_total = sum(number),
         name_max = max(number)) %>%
  
  # Ungroup the table 
  ungroup() %>%
  # Add the fraction_max column containing the number by the name maximum 
  mutate(fraction_max = number / name_max)

# Visualizing the normalized change in popularity ====
names_normalized <- babynames %>%
  group_by(name) %>%
  mutate(name_total = sum(number),
         name_max = max(number)) %>%
  ungroup() %>%
  mutate(fraction_max = number / name_max)

# Filter for the names Steven, Thomas, and Matthew
names_filtered <- names_normalized %>%
  filter(name %in% c("Steven", 
                     "Thomas", 
                     "Matthew"))

# Visualize these names over time
ggplot(names_filtered, aes(x = year, y = fraction_max, color = name)) +
  geom_line()

# Windows Functions ====