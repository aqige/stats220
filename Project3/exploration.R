############
# Part A
############
# Search words used: nice dog
# Search URL:https://www.pexels.com/search/nice%20dog/
# Describe three things:
# 1. The photos vary in size and orientation. Some are portrait while others are landscape.

# 2. Many of the photos appear to be landscape, suggesting that wide images are more common.

# 3. The photos are taken by different photographers, with names of varying lengths.


############
# Part B
############

#api_key <- "YOUR_API_KEY_HERE"

############
# Part C
############
library(tidyverse)
library(httr)
library(magick)
# My personal purpose: get the key from the Pexels API to get photo data 
api_key <- "lIAuQtsixxJfacvQSk45XhgymYu7lzIU7JrJQFxrKVP36TUdMNWOhi66"

# My personal purpose: get the url from the Pexels API to get photo data for my Project 3 search words,
# my search word is "nice dog",80 photos per page.

url <- "https://api.pexels.com/v1/search?query=nice%20dog&per_page=80"

response <- httr::GET(url, 
                      add_headers(Authorization = api_key))

data <- httr::content(response, 
                      as = "parsed", 
                      type = "application/json")

# My personal purpose: turn the nested API photo data into a flat data frame.
photo_data <- tibble(photos = data$photos) %>%
  unnest_wider(photos) %>%
  unnest_wider(src)
# My personal purpose: create new variables to help describe and select photos.
#1. mutate() function create three NEW variables aspect_ratio,orientation,photographer_name_length,
#2. categorical variable is a three level variable,so it is with no more than four levels.
#3.selected_photos has both numeric and character variables:we have numeric variable such as width,and 
# character variable such as orientation
#4. I use filter() to select around 20 photos based on a condition,and slice_head() to keep the first 20 photos.

selected_photos <- photo_data %>%
  mutate(
    aspect_ratio = width / height,
    orientation = case_when(
      aspect_ratio > 1 ~ "landscape",
      aspect_ratio < 1 ~ "portrait",
      TRUE ~ "square"
    ),
    photographer_name_length = nchar(photographer)
  ) %>%
  filter(
    photographer_name_length >= 15
  ) %>%
  select(
    id,
    width,
    height,
    aspect_ratio,
    orientation,
    photographer,
    photographer_name_length,
    url,
    original,
    medium
  )  %>%
  slice_head(n = 20)

# My personal purpose: save my selected photo data in the project.
write_csv(selected_photos, "selected_photos.csv")


############
# Part D
############
# My personal purpose: calculate summary values for my selected nice dog photos.

mean_width <- selected_photos$width %>%
  mean(na.rm = TRUE)
mean_width

median_height <- selected_photos$height %>%
  median(na.rm = TRUE)
median_height

number_landscape <- (selected_photos$orientation == "landscape") %>%
  sum(na.rm = TRUE)
number_landscape


# My personal purpose: compare photo size across different orientation groups.
grouped_photos <- selected_photos %>%
  group_by(orientation) %>%
  summarise(
    mean_width = mean(width, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE)
  )
grouped_photos

# My personal purpose: save one summary value from the grouped data frame.
mean_width_landscape <- grouped_photos$mean_width[
  grouped_photos$orientation == "landscape"
]
mean_width_landscape

############
# Part E
############


# My personal purpose: create an animated GIF using three selected dog photos.
#image_resize("500x500!") must have“！”, missing "!" will lead the image not scale,and show the black background. 
# Image 1
image1 <- image_read(selected_photos$medium[1]) %>%
  image_resize("500x500!") %>%
  image_annotate(
    text = "nice dog 1",
    size = 40,
    color = "white",
    boxcolor = "black",
    gravity = "south"
  )

# Image 2
image2 <- image_read(selected_photos$medium[2]) %>%
  image_resize("500x500!") %>%
  image_annotate(
    text = "nice dog 2",
    size = 40,
    color = "white",
    boxcolor = "black",
    gravity = "south"
  )

# Image 3
image3 <- image_read(selected_photos$medium[3]) %>%
  image_resize("500x500!") %>%
  image_annotate(
    text = "nice dog 3",
    size = 40,
    color = "white",
    boxcolor = "black",
    gravity = "south"
  )

# Combine images into frames
image_frames <- c(image1, image2, image3)

# Create animated GIF
creativity_gif <- image_animate(
  image_frames,
  fps = 1
)

# Save GIF
image_write(
  creativity_gif,
  "creativity.gif"
)
creativity_gif
