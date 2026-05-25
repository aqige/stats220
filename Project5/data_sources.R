# ------------------------------------------------------------
# Load packages
# ------------------------------------------------------------

library(tidyverse)
library(jsonlite)
library(rvest)
library(httr)

# ------------------------------------------------------------
# Beehive data: read saved HTML files
# ------------------------------------------------------------

html_files <- list.files(
  "html",
  pattern = "\\.html$",
  full.names = TRUE
)

# Load the scraping function
source("scrape_html.R")

# Apply the scraping function to each HTML file
beehive <- map_df(
  html_files,
  scrape_search_results
)

# Check for duplicate rows
nrow(beehive)
nrow(distinct(beehive))

# Remove duplicate rows if needed
beehive <- beehive %>%
  distinct()

# Save Beehive dataset locally
saveRDS(beehive, "beehive.rds")

# ------------------------------------------------------------
# Create a vector of minister names
# ------------------------------------------------------------

minister_names <- beehive %>%
  select(ministers) %>%
  separate_rows(ministers, sep = ";") %>%
  mutate(ministers = str_squish(ministers)) %>%
  filter(ministers != "") %>%
  pull(ministers) %>%
  unique()

minister_names

# ------------------------------------------------------------
# Wikipedia API data
# ------------------------------------------------------------

# Load the Wikipedia API function
source("get_wikipedia_infobox.R")

# Create ministers dataset using the Wikipedia API
# Do not run this repeatedly because it sends API requests
ministers <- map_df(
  minister_names,
  get_wikipedia_infobox
)

# Check the Wikipedia data because API matching may be imperfect
glimpse(ministers)
View(ministers)

# Check for duplicate rows
nrow(ministers)
nrow(distinct(ministers))

# Remove duplicate rows if needed
ministers <- ministers %>%
  distinct()

# Save ministers dataset locally
saveRDS(ministers, "ministers.rds")
