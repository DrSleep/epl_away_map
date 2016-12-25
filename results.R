### scrape all the current results
### from http://www.bbc.com/sport/football/premier-league/results

library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)

# scrape results
url <- "http://www.bbc.com/sport/football/premier-league/results"
raw_data <- read_html(url) %>%
  html_nodes(".match-details") %>%
  html_text()

# leave only lines with results
raw_data <- raw_data[!str_detect(raw_data, "Fixture")]

# extract teams
teams <- raw_data %>%
  str_replace_all(., "\n", "") %>%
  str_trim() %>%
  str_split(., "[0-9]-[0-9]") %>% 
  unlist() %>%
  str_trim()

# extract scores as string
scores_str <- raw_data %>%
  str_match(., "[0-9]-[0-9]")

# it will be useful later to have also scores
# in a numeric format - home_team goals, away_team goals
scores_int <- scores_str %>%
  str_split(., "-") %>%
  unlist %>%
  as.numeric()

# sanity check: the number of teams is twice as bigger than
# the number of scores (str)
assertthat::are_equal(length(scores_str) * 2, length(teams))

# combine all the data into a data frame
results_df <- data.frame("home_team"=teams[seq.int(1, length(teams), 2)],
                         "away_team"=teams[seq.int(2, length(teams), 2)],
                         "result"=scores_str,
                         "home_goals"=scores_int[seq.int(1, length(scores_int), 2)],
                         "away_goals"=scores_int[seq.int(2, length(scores_int), 2)])

write_delim(results_df, "data/results", delim="|")
