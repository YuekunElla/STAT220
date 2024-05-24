library(magick)
library(tidyverse)
library(jsonlite)
#Explore the code!
json_data <- fromJSON("pixabay_data.json")
pixabay_photo_data <- json_data$hits

selected_photos <- pixabay_photo_data %>%
  select(previewURL,pageURL,downloads,tags, likes, user_id, views, comments) %>%
  mutate(tags_length = str_count(tags, "\\S+")) %>%
  mutate(more_million_downloads = ifelse(downloads > 1000000, "yes", "no")) %>%
  mutate(if_contain_search_words = ifelse(str_detect(str_to_lower(tags),
                                                     "starry|sky"),
                                          "yes",
                                          "no")) %>%
  filter(more_million_downloads == "yes" | if_contain_search_words == "yes" ) 

write_csv(selected_photos, "selected_photos.csv")

#Calculate the mean downloads of selected photos
mean_downloads <- selected_photos$downloads %>%
  mean()
mean_downloads

#Calculate the mean tags_length of selected photos
mean_tags_length <- selected_photos$tags_length %>%
  mean()
mean_tags_length

#Calculate the sum of comments of selected photos
sum_comments <- selected_photos$comments %>%
  sum()
sum_comments

#Calculate the sum of more million downloads and less million downloads
sum_more_million_downloads <- selected_photos %>%
  group_by(more_million_downloads) %>%
  summarise(sum_downloads = sum(downloads))
sum_more_million_downloads

# Calculate the mean likes of different users
user_likes <- selected_photos %>%
  group_by(user_id) %>%
  summarise(mean_like = mean(likes))
user_likes

#Calculate the amount of photos which is more million downloads among the selected photos.
amount_more_million_downloads <- photo_data %>%
  group_by(more_million_downloads) %>%
  summarise(amount = n())
amount_more_million_downloads

#Calculate the ratio of between downloads and views for each users 
user_download_ratio <- selected_photos %>%
  group_by(user_id) %>%
  summarise(downloads_ratio = sum(downloads)/sum(views))
user_download_ratio

#Create an animated GIF of the photos in your selected_photos data frame
img_urls <- selected_photos$previewURL %>% na.omit()

img_urls %>%
  image_read() %>%
  image_scale(200) %>%
  image_animate(fps = 2) %>%
  image_write("my_photos.gif")

#Create a plot of the ratio of downloads to views for each user
user_download_ratio_plot <- user_download_ratio %>%
  ggplot() +
  geom_point(aes(x=user_id, y = downloads_ratio)) +
  labs(
    title = "The downloads ratio of users",
    subtitle = "The ratio of downloads to views for each user",
    caption = "Source: Pixabay",
    x = "User ID",
    y = "Downloads Ratio"
  )
options(scipen = 999)
user_download_ratio_plot













