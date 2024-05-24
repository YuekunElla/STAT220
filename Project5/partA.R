library(tidyverse)
library(rvest)
library(magick)

# My data context is the difference and similarity of popular movies in theaters and streaming.

# Movies title, year, critics consensus, synopsis and director are useful for my exploration.

# I used to try  https://www.netflix.com/tudum/top10/, however it is not appropriate for web scraping.
# Since, by checking the terms and conditions and reading the "robots.txt" file of the websit, I found the website is not allowed to use any robot, spider, scraper or other automated means to access the Website.

url <- "https://editorial.rottentomatoes.com/guide/popular-movies/"
page <- read_html(url)

# movies title
movie_title <- page %>%
  html_elements(".articleContentBody")%>%
  html_elements("h2")%>%
  html_elements("a")%>%
  html_text2()
movie_title

#movies year
movie_year <- page %>%
  html_elements(".countdown-item-content") %>%
  html_elements(".subtle.start-year") %>%
  html_text(trim = TRUE) %>%
  gsub("\\D", "", .)
movie_year

#movies url
movie_url <- page %>%
  html_elements(".article_movie_title")%>%
  html_elements("a")%>%
  html_attr("href")
movie_url

#movies director
movie_director <- page %>%
  html_elements(".countdown-item-content .info.director") %>%
  html_text2()%>%
  str_replace_all("Directed By: ", "")
movie_director 

# movies critics consensus
movie_consensus <- page %>% 
  html_elements(".critics-consensus") %>%
  html_text2()
movie_consensus

# movies synopsis
movie_synopsis <- page %>% 
  html_elements(".info.synopsis") %>%
  html_text2()
movie_synopsis









