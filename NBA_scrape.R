library(nbastatR)
library(future)
library(tidyverse)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)  # Double the default size

plan(multisession) 
game_logs <- game_logs(seasons = 2014:2024, season_types =  c("Regular Season","Playoffs"))
game_ids <- game_logs %>% select(idGame) %>% unique() %>% arrange(idGame) %>% as_vector()
box_scores(game_ids = c(game_ids), box_score_types = c("Traditional"), result_types = c("player", "team"), join_data = TRUE, assign_to_environment = TRUE, return_message = TRUE)


box_scores(game_ids = c(game_ids), box_score_types = c("Traditional", "Advanced", "Scoring", "Misc", "Usage", "Four Factors", "Tracking", "Defence","Matchups","Hustle"), result_types = c("player", "team"), join_data = TRUE, assign_to_environment = TRUE, return_message = TRUE)


team_dict <- nba_teams(league = "NBA", join_blg = F) %>% filter(idLeague == 2 & isNonNBATeam == 0)
