library(tidyverse)
library(ggrepel)
library(scales)

covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

covid_data_tbl_processed <- covid_data_tbl %>% 
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
  )) %>%
  distinct()

# Filter data for specified countries
countries <- c("Germany", "UK", "France", "Spain", "USA")
filtered_data <- covid_data_tbl_processed %>%
  filter(location %in% countries) %>%
  filter(total_cases > 0) # Filter out rows with zero total cases

# Custom color palette
custom_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")

# Plot the time course of cumulative cases
ggplot(filtered_data, aes(x = as.Date(date), y = total_cases/1e6, color = location)) +
  geom_line(size = 0.75) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 125, by = 25), limits = c(0, 125), expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
  scale_color_manual(values = custom_colors) +
  labs(x = "Date", y = "Cumulative Cases (Millions)", color = "Country",
       title = "COVID-19 confirmed cases worldwide",
       subtitle = "As of 11/05/2024") +
  expand_limits(y = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

p <- ggplot(filtered_data, aes(x = as.Date(date), y = total_cases/1e6, color = location)) +
  geom_line(size = 0.75) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 125, by = 25), limits = c(0, 125), expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
  scale_color_manual(values = custom_colors) +
  labs(x = "Date", y = "Cumulative Cases (Millions)", color = "Country",
       title = "COVID-19 confirmed cases worldwide",
       subtitle = "As of 11/05/2024") +
  expand_limits(y = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

# Add labels at the last data point
p + geom_label_repel(data = subset(filtered_data, date == max(date)), 
                     aes(label = total_cases/1e6), 
                     size = 3,
                     box.padding = 0.5,
                     point.padding = 0.1)


world <- map_data("world")

covid_mortality_tbl_processed <- covid_data_tbl_processed %>%
  group_by(location) %>%
  summarise(mortality_rate = sum(total_deaths_per_million, na.rm = TRUE) /10000000) %>%
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
  ))

world <- left_join(world, covid_mortality_tbl_processed, by = c("region" = "location"))

# Plot the world map with mortality rate
ggplot(world, aes(x = long, y = lat, group = group, fill = mortality_rate)) +
  geom_polygon(color = "black") +
  labs(title = "Confirmed COVID-19 Deaths relative to size of population", fill = "Mortality Rate") +
  scale_fill_gradient(low = "red", high = "black", na.value = "gray")

