# ------------------------------------------------------------
# Project 5 visualisation.R
# Purpose:
# Explore how frequently AI and digital technology are mentioned
# in New Zealand government communications by year and political party.
# Data sources:
# Beehive.govt.nz search results and Wikipedia API minister data
# ------------------------------------------------------------


# ------------------------------------------------------------
# Load libraries
# ------------------------------------------------------------

library(tidyverse)
library(lubridate)


# ------------------------------------------------------------
# Read RDS files created in Part B
# ------------------------------------------------------------

beehive <- readRDS("beehive.rds")
ministers <- readRDS("ministers.rds")


# ------------------------------------------------------------
# Inspect the structure of the datasets
# ------------------------------------------------------------

glimpse(beehive)
glimpse(ministers)


# ------------------------------------------------------------
# Create a minister-party dataset from Wikipedia infobox data
# Standardise party names into National, Labour, and Other
# ------------------------------------------------------------

minister_party <- ministers %>%
  filter(label == "Party") %>%
  select(
    ministers = minister,
    party = value
  ) %>%
  mutate(
    party = case_when(
      str_detect(
        str_to_lower(party),
        "national"
      ) ~ "National",
      
      str_detect(
        str_to_lower(party),
        "labour"
      ) ~ "Labour",
      
      TRUE ~ "Other"
    )
  ) %>%
  distinct()

glimpse(minister_party)


# ------------------------------------------------------------
# Combine Beehive search results with Wikipedia minister data
# ------------------------------------------------------------

combined_data <- beehive %>%
  left_join(
    minister_party,
    by = "ministers"
  ) %>%
  mutate(
    party = ifelse(
      is.na(party),
      "Other",
      party
    )
  )

glimpse(combined_data)


# ------------------------------------------------------------
# Analyse text data using stringr and dplyr
# Detect AI and digital-related keywords in titles and summaries
# ------------------------------------------------------------

plot_data <- combined_data %>%
  mutate(
    date = dmy(date_text),
    
    year = year(date),
    
    text = str_to_lower(
      paste(title, summary)
    ),
    
    ai_or_digital = str_detect(
      text,
      "artificial intelligence|\\bai\\b|digital|digitisation|digitising|digitise|digitize"
    )
  ) %>%
  filter(
    !is.na(year),
    ai_or_digital
  ) %>%
  group_by(
    year,
    party
  ) %>%
  summarise(
    frequency = n()
  )

glimpse(plot_data)


# ------------------------------------------------------------
# Create purpose-driven grouped bar chart
# ------------------------------------------------------------

my_viz <- ggplot(
  data = plot_data,
  mapping = aes(
    x = factor(year),
    y = frequency,
    fill = party
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.8,
      preserve = "single"
    ),
    width = 0.7,
    alpha = 0.9,
    colour = "white",
    linewidth = 0.3,
    na.rm = TRUE,
    show.legend = TRUE
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(
      mult = c(0, 0.12)
    )
  ) +
  labs(
    title = "AI and Digital Topics in New Zealand Government Communications",
    
    subtitle = "Frequency of AI and digital-related Beehive search results by year and political party",
    
    x = "Year",
    
    y = "Frequency of search results",
    
    fill = "Political party",
    
    caption = "Data sources: Beehive.govt.nz search results and Wikipedia API minister infobox data"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    axis.title.x = element_text(
      face = "bold"
    ),
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      hjust = 0
    )
  )

my_viz


# ------------------------------------------------------------
# Save visualisation
# ------------------------------------------------------------

ggsave(
  filename = "my_viz.png",
  plot = my_viz,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)
