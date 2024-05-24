library(magick)
library(tidyverse)
#A colour palette
my_colors <- c("#E59BE9","#FFCDEA", "#FB9AD1" ,"#86469C")

# Reading data from published CSV file into a data frame 
csv_file <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbRa6I6QQnUvKHE0RC8wnaPpUwPacwbtb6VqTJwGEQ1Sf3iW8Vbl3Yii--w43MAztzTHnVcEQf0Srz/pub?output=csv"
youtube_data <- read_csv(csv_file) 
#I want to explore what can influence the amount of views.
#Firstly, I want to explore the relationship between the duration and the view count of the video 
duration_views_data <- youtube_data  %>%
  mutate(duration_minutes = round(duration/60,2),
         views_in_thousands = round(viewCount/1000,2),
         views_amount_level = case_when(views_in_thousands <= 10 ~ "low views",
                                  views_in_thousands <= 100 ~ "Moderate views",
                                  views_in_thousands <= 1000 ~ "High views",
                                  views_in_thousands <= 10000 ~ "Very high views"))
duration_views_data %>%
  ggplot(aes(x = duration_minutes,
             y = reorder(views_amount_level, duration_minutes),
             colour = views_amount_level)) +
  geom_jitter(height = 0.2) +
  geom_boxplot(fill=NA) + 
  facet_wrap(vars(channelName)) +
  scale_color_manual(values = my_colors ) +
  labs( title = "What is the duration of a highly viewed video?" ,
        x = "duration minutes",
        y = "The level of views amount",
        caption = "Source: YouTube API"
  )+
  theme_minimal()+
  guides(color = FALSE)+
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )
ggsave("plot3.png")

# The mean duration minutes of high views videos is around 14 min for @aliazaita.
specific_duration_views_data_1 <- duration_views_data %>%
  filter(channelName =="@aliazaita") %>%
  group_by(views_amount_level) %>%
  summarise(mean_duration_minutes = mean(duration_minutes))
# The mean duration minutes of high views videos is around 10 min for @LaniPliopa.
specific_duration_views_data_2 <- duration_views_data %>%
  filter(channelName =="@LaniPliopa") %>%
  group_by(views_amount_level) %>%
  summarise(mean_duration_minutes = mean(duration_minutes))

#Then, I want to see the distribution of video duration of the two channel.
duration_data <- duration_views_data %>%
  select(duration_minutes,channelName) 

channel_colour <- c(my_colors[3],my_colors[4])
duration_data %>%
  ggplot() +
  geom_density(aes(x = duration_minutes,fill= channelName))+
  facet_wrap(vars(channelName)) +
  scale_fill_manual(values = channel_colour) +
  labs(title = "How long is typical video durations of two channels?",
       caption = "Source: YouTube API",
       x = "duration minutes") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  )
ggsave("plot4.png")

more_100_thousands_views <- duration_views_data %>%
  filter(views_in_thousands >= 100) %>%
  group_by(channelName) %>%
  summarise(ratio_of_vedieos = n()/100)

#Secondly, I want to explore the relationship between published time in a day and views count.
time_views_data <- duration_views_data %>%
  mutate(published_time = datePublished%>% str_sub(12, 13) %>% parse_number()) %>%
  select(channelName,published_time,views_in_thousands)%>%
  group_by(channelName,published_time)%>%
  summarise(mean_views_in_thousands = mean(views_in_thousands))

channel_colour <- c(my_colors[3],my_colors[4])

time_views_data %>% 
  ggplot() +
  geom_bar(aes(x = published_time, y = mean_views_in_thousands,fill = channelName), 
           stat = "identity",
           position = "dodge") +
  labs(title = "Average views at different times of day of two channels",
       x = "Published Time (hour of day)",
       y = "Mean Views (in thousands)") +
  scale_fill_manual(values = channel_colour) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold")  
  ) 
ggsave("plot2.png")

# When the published time was in the period from 1pm to 4pm, the videos in the LaniPliopa channel could get more views than other time.
# When the published time was in the period from 1pm to 5pm,or around 8pm, the videos in the LaniPliopa channel could get more views than other time.

#Thirdly, I want to explore the top 10 common words by two channel.
not_useful_words <- c("in", "i", "a","my","to","the","for","with","me","days","this","day","what","as","good","new","and","of")

#I choose top 10 common words between titles which are meaningful by each two channel.
top_ten_common_words_in_title<- duration_views_data %>%
  select(title,channelName) %>%
  separate_rows(title, sep = " ") %>%
  mutate(clean_word = str_to_lower(title)%>%
           str_remove_all("[^[:alnum:]]")%>%
           str_remove_all("[[:punct:]]")%>%
           str_remove_all("[[:digit:]]")) %>%
  filter(!clean_word == "",
         !clean_word %in% not_useful_words)%>%
  group_by(channelName, clean_word) %>%
  summarise(n = n()) %>%
  arrange(channelName, desc(n)) %>%
  group_by(channelName)%>%
  top_n(10) %>%
  ungroup()

channel_colour <- c(my_colors[3],my_colors[4])
top_ten_common_words_in_title %>%
  ggplot()+
  geom_bar(aes(x = n, 
               y = clean_word,
               fill = channelName),
           position = "dodge",
           stat = "identity")+
  scale_fill_manual(values = channel_colour) +
  labs(title = "Top 10 Common Words in Titles by Two Channel",
       caption = "Source: YouTube API",
       x = "The total number of words in titles",
       y = "Word"
  )+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 20, face = "bold"))
        
ggsave("plot1.png")
















