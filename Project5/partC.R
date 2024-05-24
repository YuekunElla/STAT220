library(tidyverse)
library(rvest)
library(magick)

keywords <- c("education","immigration", "data")

search1 <- read_html(paste0("https://datalandscapes.online/scrapeable/speeches.php?search=", keywords[1])) %>%
  html_elements(".speech_summary") %>%
  html_text2() %>%
  tibble(keyword = keywords[1],
         results = .) %>%
  separate(results, into = c("year", "num_speeches"), sep = "--") %>%
  mutate(num_speeches = parse_number(num_speeches),
         year = parse_number(year))

search2 <- read_html(paste0("https://datalandscapes.online/scrapeable/speeches.php?search=", keywords[2])) %>%
  html_elements(".speech_summary") %>%
  html_text2() %>%
  tibble(keyword = keywords[2],
         results = .) %>%
  separate(results, into = c("year", "num_speeches"), sep = "--") %>%
  mutate(num_speeches = parse_number(num_speeches),
         year = parse_number(year))

search3 <- read_html(paste0("https://datalandscapes.online/scrapeable/speeches.php?search=", keywords[3])) %>%
  html_elements(".speech_summary") %>%
  html_text2() %>%
  tibble(keyword = keywords[3],
         results = .) %>%
  separate(results, into = c("year", "num_speeches"), sep = "--") %>%
  mutate(num_speeches = parse_number(num_speeches),
         year = parse_number(year))


combined_search <- bind_rows(search1,search2,search3)

speeches_governments <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRHTFJcFmsIkjaFUCEwFWASaBOAR4X2Upx66C5Bhgc_WNc2JxxdRbbvyoewmvt_EjNdCNZZzzkENwLg/pub?gid=0&single=true&output=csv")

speech_data <- left_join(combined_search, speeches_governments, by = "year")


speech_data_plot <- speech_data %>%
  ggplot(aes(x = year, y = num_speeches, fill = keyword)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Speeches Made Each Year by Keyword",
       x = "Year",
       y = "Number of Speeches") +
  theme_minimal()

speech_data_plot










