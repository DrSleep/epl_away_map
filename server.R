
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(shiny)

shinyServer(function(input, output) {
  
  lastUpdate <- as.Date(read_file('update'), format="%Y %b %d")
  currentDate <- as.Date(Sys.time(), format="%Y %b %d")
  days <- as.numeric(currentDate - lastUpdate)
  
  if (days > 4) {
  # Scrape data and create json.
  # Run only if last update happened more than 4 days ago.
    write(format(Sys.time(), format="%Y %b %d"), 'update')
    
    url <- "https://www.bbc.com/sport/football/premier-league/results"
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
    
    stadiums_df <- read.table("data/stadiums_cleaned",
                              header=TRUE,
                              sep="|",
                              stringsAsFactors=FALSE)
    stadiums_df$id <- seq.int(0, 19)
    
    # correspondence between team names
    results_df <- results_df %>%
      mutate(., home_team=str_replace(home_team,
                                      "Manchester United",
                                      "Man Utd")) %>%
      mutate(., home_team=str_replace(home_team,
                                      "Manchester City",
                                      "Man City")) %>%
      mutate(., away_team=str_replace(away_team,
                                      "Manchester United",
                                      "Man Utd")) %>%
      mutate(., away_team=str_replace(away_team,
                                      "Manchester City",
                                      "Man City"))
    
    results_teams <- results_df$home_team %>% unique()
    
    corr <- integer()
    for (i in 1:20) {
      corr <- append(corr,
                     grep(paste0(results_teams[i], collapse="|"), stadiums_df$club))
    }
    
    stadiums_df$team[corr] <- results_teams
    
    # join tables
    json_df <- results_df %>%
      left_join(., stadiums_df, by=c("home_team" = "team")) %>%
      left_join(., stadiums_df, by=c("away_team" = "team")) %>%
      select(., result, home_goals, away_goals, home_id=id.x, away_id=id.y)
    
    json_results <- toJSON(json_df)
    json_stadiums <- toJSON(stadiums_df %>%
                              select(., -team))
    json_all <- paste0('{"stadiums":', json_stadiums, ',', 
                       '"results":', json_results, '}')
    write(json_all, "www/data.json")
    }
  
})
