library(tidyverse)
library(rvest)
library(magick)

# I explored the releases published by Hon David Seymour.

#Features of text:The mean title words count is around 6.68
#The mean word count per release is around 384.05.
#The mean word count used per sentence in each release is around 23.28.
#The 15 most common words in release titles, sorted by frequency, are: government, attendance, new, school, ahead, back, charter, lift, medicines, plan, pseudoephedrine, rates, schools, shelves, and should.

# Obtain a vector of the URLs for each release listed on the minister’s page
url <- "https://www.beehive.govt.nz/minister/hon-david-seymour"

pages <- read_html(url) %>%
  html_elements(".release__wrapper") %>%
  html_elements("h2") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  paste0("https://www.beehive.govt.nz", .)

# Scraping the title and the content of the first release
page_url <- pages[1]

page <- read_html(page_url)

release_title <- page %>%
  html_elements(".article__title") %>%
  html_text2()
release_title

release_content <- page  %>%
  html_elements(".prose.field.field--name-body.field--type-text-with-summary.field--label-hidden.field--item") %>%
  html_text2()
release_content

# Creating a function of get_release
get_release <- function(page_url){
  Sys.sleep(2)
  # print the page_url so if things go wrong
  # we can see which page caused issues
  print(page_url)
  page <- read_html(page_url)
  
  # add code to scrape the release title and release content
  release_title <- page %>%
    html_elements(".article__title") %>%
    html_text2()
  
  release_content <- page  %>%
    html_elements(".prose.field.field--name-body.field--type-text-with-summary.field--label-hidden.field--item") %>%
    html_text2()
  
  # add code to return a tibble created using these data objects
  return (tibble(release_title, release_content))
}

release_data <- map_df(pages, get_release)


num_words_data <- release_data %>%
  mutate(title_words_num = str_count(release_title, "\\S+"),
         num_words = str_count(release_content, "\\S+"))

#The mean number of the title words  
mean_num_words_title <- mean(num_words_data $ title_words_num) %>%
  round(2)
mean_num_words_title

# The mean number of words per release
mean_num_words_for_releases <- mean(num_words_data$num_words) %>%
  round(2)
mean_num_words_for_releases


#Top 15 common words in releases titles
not_useful_words = c("for", "to", "of","a","and","on")

common_words <- release_data %>% 
  separate_rows(release_title, sep = " ") %>%
  mutate(title_word = str_to_lower(release_title)) %>%
  group_by(title_word)%>%
  filter(!title_word %in% not_useful_words) %>%
  summarise(word_amount = n()) %>%
  arrange(desc(word_amount)) %>%
  slice(1:15)


# The mean number of words used per sentence 
word_count_per_sentence <- release_data %>%
  mutate(sentences = str_split(release_content, "(?<=[.!?])\\s+|(?<=\\n\\n)"))%>%
  unnest(sentences) %>%
  filter(sentences != "") %>%
  mutate(word_count = str_count(sentences, "\\S+"))

mean_word_count_per_sentence <- mean(word_count_per_sentence$word_count)%>%
  round(2)

mean_word_count_per_sentence






























