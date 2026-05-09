library(tidyverse)
library(lubridate)
library(stringr)

# Colour theme from ColorBrewer BuGn
fuel_colours <- c(
  "91" = "#e5f5f9",
  "95" = "#99d8c9",
  "Diesel" = "#2ca25f"
)

logged_data <- read_csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vTza23ZPEHiCWJVenauqQbOdcfdmRuFY57vHYu-mUfvWvrZH5Js3bghpEdC3lazXXd0rgx7PMXV3fJ-/pub?gid=1634441430&single=true&output=csv"
)

# Clean and prepare data
logged_data <- logged_data %>%
  mutate(
    Timestamp = dmy_hms(Timestamp),
    Date = dmy(Date),
    `Fuel Type` = as.character(`Fuel Type`),
    `Fuel Type` = str_to_title(`Fuel Type`),
    `Fuel Type` = case_when(
      str_detect(`Fuel Type`, "91") ~ "91",
      str_detect(`Fuel Type`, "95") ~ "95",
      str_detect(`Fuel Type`, "Diesel") ~ "Diesel",
      TRUE ~ `Fuel Type`
    ),
    `Fuel Type` = factor(`Fuel Type`, levels = c("91", "95", "Diesel")),
    `Time Period` = factor(`Time Period`, levels = c("Morning", "Afternoon", "Evening")),
    Area = case_when(
      Area == "Cbd" ~ "CBD",
      TRUE ~ Area
    ),
    Station = str_to_title(Station),
    price = `Price ($/L)`
  )

# Plot 1 data: average price by fuel type
fuel_summary <- logged_data %>%
  group_by(`Fuel Type`) %>%
  summarise(
    average_price = mean(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(average_price))

plot1 <- fuel_summary %>%
  ggplot(aes(x = `Fuel Type`, y = average_price, fill = `Fuel Type`)) +
  geom_col() +
  scale_fill_manual(values = fuel_colours) +
  labs(
    title = "Average fuel price by fuel type",
    subtitle = "Average observed fuel prices",
    x = "Fuel type",
    y = "Average price ($/L)",
    caption = "Data source: observational logging using the Gaspy app"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plot1.png", plot1, width = 8, height = 5)

# Plot 2: prices over time using Timestamp
plot2 <- logged_data %>%
  ggplot(aes(x = Timestamp, y = price, colour = `Fuel Type`)) +
  geom_point(size = 2) +
  geom_line(aes(group = `Fuel Type`), linewidth = 0.7) +
  scale_colour_manual(values = fuel_colours) +
  labs(
    title = "Fuel prices recorded over time",
    subtitle = "Prices were logged during morning, afternoon, and evening observations",
    x = "Timestamp",
    y = "Price ($/L)",
    colour = "Fuel type",
    caption = "Timestamp values were converted using lubridate"
  ) +
  theme_minimal()

ggsave("plot2.png", plot2, width = 9, height = 5)

# Plot 3 data: average price by area and fuel type
area_summary <- logged_data %>%
  group_by(Area, `Fuel Type`) %>%
  summarise(
    average_price = mean(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(Area, `Fuel Type`)

plot3 <- area_summary %>%
  ggplot(aes(x = Area, y = average_price, fill = `Fuel Type`)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = fuel_colours) +
  labs(
    title = "Average fuel price by Auckland area",
    subtitle = "Area was added to compare regional differences in observed fuel prices",
    x = "Area",
    y = "Average price ($/L)",
    fill = "Fuel type",
    caption = "Data source: published CSV from Google Form responses"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("plot3.png", plot3, width = 9, height = 5)
