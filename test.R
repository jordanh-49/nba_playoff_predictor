# Helper functions ---------------------------

## Function to install and load packages
install_and_load <- function(package_names) {
  # Check which packages are not installed
  new_packages <- package_names[!(package_names %in% installed.packages()[, "Package"])]
  
  # Install new packages
  if(length(new_packages)) {
    install.packages(new_packages)
  }
  
  # Load all packages
  sapply(package_names, require, character.only = TRUE)
}

## Function to count games in the last n days
count_games_last_n_days <- function(dates, n_days) {
  sapply(1:length(dates), function(i) {
    if (i == 1) {
      NA
    } else {
      sum(dates[i] - dates[1:(i-1)] <= n_days)
    }
  })
}

## Function to create playoff bracket including 1st Round pre-fill and blank entries for rest of playoffs
create_round_bracket <- function(initial_matchups, round_name, playoff_team_seed) {
  round_num <- round_name
  # Helper function to expand series into games based on home advantage pattern
  expand_series <- function(h_team, a_team, conf_name, round_num) {
    # Define the home team pattern based on the game number
    home_pattern <- c(h_team, h_team, a_team, a_team, h_team, a_team, h_team)
    away_pattern <- c(a_team, a_team, h_team, h_team, a_team, h_team, a_team)
    tibble(
      conference = ifelse(round_num == 4, "Both", conf_name),
      round_number = round_num,
      round_name = c("Round 1", "Round 2", "Conference Finals", "Finals")[round_num],
      game_number = 1:7,
      h_team = home_pattern,
      a_team = away_pattern
    )
  }
  
  # Mapping round names to numbers
  round_map <- c("Round 1" = 1, "Round 2" = 2, "Conference Finals" = 3, "Finals" = 4)
  round_num <- round_map[[round_name]]
  
  # Generate the data frame for the specified round from the initial matchups
  round_bracket <- bind_rows(
    lapply(names(initial_matchups), function(conf_name) {
      bind_rows(
        lapply(names(initial_matchups[[conf_name]]), function(h_team) {
          a_team <- initial_matchups[[conf_name]][[h_team]]
          expand_series(h_team, a_team, conf_name, round_num)
        })
      )
    })
  )
  
  if (round_name == 4) {
    round_bracket <- round_bracket %>%
      mutate(
        season = 2023,
        nbagameid = row_number(),
        gamedate = as.Date('2024-12-31')
      ) %>%
      left_join(
        playoff_team_seed %>% select(team_name,league_seed),
        by = c("h_team" = "team_name")
      ) %>%
      rename("h_seed" = "league_seed") %>%
      left_join(
        playoff_team_seed %>% select(team_name,league_seed),
        by = c("a_team" = "team_name")
      ) %>%
      rename("a_seed" = "league_seed")
    
  } else {
    round_bracket <- round_bracket %>%
      mutate(
        season = 2023,
        nbagameid = row_number(),
        gamedate = as.Date('2024-12-31')
      ) %>%
      left_join(
        playoff_team_seed,
        by = c("h_team" = "team_name")
      ) %>%
      rename("h_seed" = "seed") %>%
      left_join(
        playoff_team_seed,
        by = c("a_team" = "team_name")
      ) %>%
      rename("a_seed" = "seed")
  }
  
  return(round_bracket)
}

## Function to get features for current bracket
bracket_with_features <- function(bracket, most_recent_ratings,most_recent_rolling_features,most_recent_player_features) {
  bracket_features <- bracket %>%
    inner_join(
      most_recent_ratings,
      by = c("h_team" = "team")
    ) %>%
    rename("h_rating" = rating) %>%
    inner_join(
      most_recent_ratings,
      by = c("a_team" = "team")
    ) %>%
    rename("a_rating" = rating) %>%
    inner_join(
      most_recent_rolling_features,
      by = c("h_team" = "team")
    ) %>%
    rename_with(~ paste0("h_", .), fg2made:ft_rate) %>%
    inner_join(
      most_recent_rolling_features,
      by = c("a_team" = "team")
    ) %>%
    rename_with(~ paste0("a_", .), fg2made:ft_rate) %>%
    inner_join(
      most_recent_player_features,
      by = c("h_team" = "team")
    ) %>%
    rename_with(~ paste0("h_", .), mean_oreb_pct:cumulative_unique_lineups) %>%
    inner_join(
      most_recent_player_features,
      by = c("a_team" = "team")
    ) %>%
    rename_with(~ paste0("a_", .), mean_oreb_pct:cumulative_unique_lineups) %>%
    mutate(
      diff_rating = h_rating - a_rating,
      diff_fg2made = h_fg2made - a_fg2made,
      diff_fg2missed = h_fg2missed - a_fg2missed,
      diff_fg2attempted = h_fg2attempted - a_fg2attempted,
      diff_fg3made = h_fg3made - a_fg3made,
      diff_fg3missed = h_fg3missed - a_fg3missed,
      diff_fg3attempted = h_fg3attempted - a_fg3attempted,
      diff_fgmade = h_fgmade - a_fgmade,
      diff_fgmissed = h_fgmissed - a_fgmissed,
      diff_fgattempted = h_fgattempted - a_fgattempted,
      diff_ftmade = h_ftmade - a_ftmade,
      diff_ftmissed = h_ftmissed - a_ftmissed,
      diff_ftattempted = h_ftattempted - a_ftattempted,
      diff_reboffensive = h_reboffensive - a_reboffensive,
      diff_rebdefensive = h_rebdefensive - a_rebdefensive,
      diff_reboundchance = h_reboundchance - a_reboundchance,
      diff_assists = h_assists - a_assists,
      diff_stealsagainst = h_stealsagainst - a_stealsagainst,
      diff_turnovers = h_turnovers - a_turnovers,
      diff_blocksagainst = h_blocksagainst - a_blocksagainst,
      diff_defensivefouls = h_defensivefouls - a_defensivefouls,
      diff_offensivefouls = h_offensivefouls - a_offensivefouls,
      diff_shootingfoulsdrawn = h_shootingfoulsdrawn - a_shootingfoulsdrawn,
      diff_possessions = h_possessions - a_possessions,
      diff_points = h_points - a_points,
      diff_shotattempts = h_shotattempts - a_shotattempts,
      diff_andones = h_andones - a_andones,
      diff_shotattemptpoints = h_shotattemptpoints - a_shotattemptpoints,
      diff_ppa = h_ppa - a_ppa,
      diff_ppp = h_ppp - a_ppp,
      diff_tov_pct = h_tov_pct - a_tov_pct,
      diff_blk_pct = h_blk_pct - a_blk_pct,
      diff_ortg = h_ortg - a_ortg,
      diff_drtg = h_drtg - a_drtg,
      diff_ntrg = h_ntrg - a_ntrg,
      diff_efg_pct = h_efg_pct - a_efg_pct,
      diff_ts_pct = h_ts_pct - a_ts_pct,
      diff_ft_rate = h_ft_rate - a_ft_rate,
      diff_mean_oreb_pct = h_mean_oreb_pct - a_mean_oreb_pct,
      diff_mean_dreb_pct = h_mean_dreb_pct - a_mean_dreb_pct,
      diff_mean_tov_pct = h_mean_tov_pct - a_mean_tov_pct,
      diff_mean_stl_pct = h_mean_stl_pct - a_mean_stl_pct,
      diff_mean_blk_pct = h_mean_blk_pct - a_mean_blk_pct,
      diff_mean_usg_pct = h_mean_usg_pct - a_mean_usg_pct,
      diff_mean_ast_pct = h_mean_ast_pct - a_mean_ast_pct,
      diff_max_usg_pct = h_max_usg_pct - a_max_usg_pct,
      diff_avg_mp_starter = h_avg_mp_starter - a_avg_mp_starter,
      diff_avg_mp_bench = h_avg_mp_bench - a_avg_mp_bench,
      diff_pnts_by_starters = h_pnts_by_starters - a_pnts_by_starters,
      diff_pnts_by_bench = h_pnts_by_bench - a_pnts_by_bench,
      diff_sharp_shooters = h_sharp_shooters - a_sharp_shooters,
      diff_paint_specialists = h_paint_specialists - a_paint_specialists,
      diff_game_score_metric = h_game_score_metric - a_game_score_metric,
    ) %>%
    select(
      conference:a_seed,
      starts_with("diff_"),
      h_cumulative_unique_lineups,
      a_cumulative_unique_lineups
    )
  return(bracket_features)
}

## Function for Conference based playoffs rounds 
run_series <- function(bracket_features,xgb_last) {
  # Create bracket checker
  bracket_checker <- bracket_features %>%
    select(conference:seed_id) %>%
    distinct(conference,round_name,round_number, seed_id) %>%
    mutate(
      winner = NA,
      loser = NA,
      total_games = NA
    )
  
  # Create distinct conference, seed groups
  initial_seed_ids_df <- bracket_features %>%
    distinct(conference,seed_id)
  
  conference_groups <- split(initial_seed_ids_df, initial_seed_ids_df$conference)
  
  # Loop through each conference 
  for(conf in names(conference_groups)) {
    # Extract the current conference data frame
    conference_data <- conference_groups[[conf]]
    
    for(seed in conference_data$seed_id) {
      # Filter rows that match the current series_id
      indv_series <- bracket_features %>%
        filter(seed_id == seed & conference == conf)
      
      u_seed <- indv_series %>% slice_min(order_by = h_seed, n = 1) %>% distinct(h_team) %>% pull(h_team)
      b_seed <- indv_series %>% slice_max(order_by = h_seed, n = 1) %>% distinct(h_team) %>% pull(h_team)
      
      if (length(u_seed) > 1) {
        b_seed <- u_seed[2]
        u_seed <- u_seed[1]
      }
      
      u_seed_wins = 0
      u_seed_losses = 0
      b_seed_wins = 0
      b_seed_losses = 0
      
      for (row_n in 1:nrow(indv_series)) {
        indiv_game = indv_series %>%
          filter(game_number == row_n)
        
        home_team = indiv_game$h_team
        away_team = indiv_game$a_team
        
        pred_winner = predict(
          xgb_last %>% extract_workflow(), 
          new_data = indiv_game,
          type = "prob",
        )
        
        is_home_win = sample(x = c(1, 0), size = 1, replace = TRUE,
                             prob = c(pred_winner$.pred_1, pred_winner$.pred_0))
        
        # Updating the series_tracker based on the game outcome
        if (is_home_win == 1) {
          if (home_team == u_seed)  {
            # Upper seed wins
            u_seed_wins <- u_seed_wins + 1
            b_seed_losses <- b_seed_losses + 1
          } else {
            # Lower seed wins
            b_seed_wins <- b_seed_wins + 1
            u_seed_losses <- u_seed_losses + 1
          }
        } else {
          if (home_team == u_seed)  {
            # Upper seed loses
            b_seed_wins <- b_seed_wins + 1
            u_seed_losses <- u_seed_losses + 1
          } else {
            # Lower seed loses
            u_seed_wins <- u_seed_wins + 1
            b_seed_losses <- b_seed_losses + 1
          }
        }
        
        # Check if either team has won 4 games
        if (u_seed_wins >= 4 || b_seed_wins >= 4) {
          if (u_seed_wins >= 4) {
            # If Upper seed wins they advance
            total_games <- u_seed_wins + u_seed_losses
            bracket_checker$winner[bracket_checker$seed_id == seed & bracket_checker$conference == conf] <- u_seed
            bracket_checker$loser[bracket_checker$seed_id == seed & bracket_checker$conference == conf] <- b_seed
            bracket_checker$total_games[bracket_checker$seed_id == seed & bracket_checker$conference == conf] <- total_games
            
          } else{
            # If Lower seed wins they advance
            total_games <- b_seed_wins + b_seed_losses
            bracket_checker$winner[bracket_checker$seed_id == seed & bracket_checker$conference == conf] <- b_seed
            bracket_checker$loser[bracket_checker$seed_id == seed & bracket_checker$conference == conf] <- u_seed
            bracket_checker$total_games[bracket_checker$seed_id == seed & bracket_checker$conference == conf] <- total_games
            
          }
          break  # Exit the loop
        }
      }
    }
  }
  return(bracket_checker)
}

## Function to ensure that the higher seeded team starts as home team in case of upset
align_bracket_seeding <- function(bracket_with_features) {
  # Determine which matchups need swapping based on the first game
  swap_teams <- bracket_with_features %>%
    filter(game_number == 1) %>%
    mutate(need_swap = h_seed > a_seed) %>%
    select(matchup_id, need_swap)
  
  # Join this back to the original bracket_with_features
  bracket_with_features <- bracket_with_features %>%
    left_join(swap_teams, by = "matchup_id")
  
  bracket_with_features_aligned <- bracket_with_features %>%
    mutate(
      # Swap teams
      h_team_fixed = ifelse(need_swap, a_team, h_team),
      a_team_fixed = ifelse(need_swap, h_team, a_team),
      # Swap seeds
      h_seed_fixed = ifelse(need_swap, a_seed, h_seed),
      a_seed_fixed = ifelse(need_swap, h_seed, a_seed)
    ) %>%
    select(-c(need_swap,a_team, h_team, a_team, h_seed, a_seed)) %>%
    rename(
      "h_team" = h_team_fixed,
      "a_team" = a_team_fixed,
      "h_seed" = h_seed_fixed,
      "a_seed" = a_seed_fixed
    ) %>%
    select(
      conference:game_number,
      h_team,a_team,
      season:gamedate,
      h_seed,a_seed
    )
  
  return(bracket_with_features_aligned)
}

## Function to get all combinations of ECF or WCF conferences
get_cf_potential_matchups <- function() {
  # Define potential winners in the upper and lower brackets
  upper_bracket_winners <- c(1, 4, 5, 8)
  lower_bracket_winners <- c(2, 3, 6, 7)
  
  # Generate all combinations of these winners for the conference finals
  conference_finals_combinations <- expand.grid(upper_bracket = upper_bracket_winners, 
                                                lower_bracket = lower_bracket_winners) %>%
    # Ensure the format "higher seed-lower seed"
    mutate(Conference_Final_Matchup = ifelse(upper_bracket < lower_bracket, 
                                             paste(upper_bracket, lower_bracket, sep = "-"),
                                             paste(lower_bracket, upper_bracket, sep = "-"))) %>%
    # Remove duplicates (as some matchups might repeat with seeds flipping)
    distinct(Conference_Final_Matchup) %>%
    arrange(Conference_Final_Matchup)
  
  return(conference_finals_combinations$Conference_Final_Matchup)
}

## Function run n number of sims using the model and playoff seeding
playoff_sim <- function(sims, xgb_last, playoff_team_seed){
  
  results_list <- list()
  final_series_list <- list()
  
  for (sim_no in 1:sims) { # number of sims to run
    ## Get most recent features
    ### Ratings
    most_recent_ratings <- hist_ratings %>%
      left_join(
        game_level %>% select(nbagameid,season,gametype),
        by = c("season","nbagameid")
      ) %>%
      filter(season == 2023 & gametype == 2) %>%
      group_by(team) %>%
      top_n(n = 1, wt = nbagameid) %>%
      ungroup() %>%
      select(-season, -nbagameid, -rating_period, -nbagameid_prev, -gametype)
    
    ### Rolling features
    most_recent_rolling_features <- rolling_mean_features %>%
      left_join(
        game_level %>% select(nbagameid,season,gametype),
        by = c("season","nbagameid")
      ) %>%
      filter(season == 2023 & gametype == 2) %>%
      group_by(team) %>%
      top_n(n = 1, wt = nbagameid) %>%
      ungroup() %>%
      select(-season, -nbagameid, -gametype, -is_home)
    
    ### Player features
    most_recent_player_features <- player_features %>%
      left_join(
        game_level %>% select(nbagameid,season,gametype),
        by = c("season","nbagameid")
      ) %>%
      filter(season == 2023 & gametype == 2) %>%
      group_by(team) %>%
      top_n(n = 1, wt = nbagameid) %>%
      ungroup() %>%
      select(-season, -nbagameid, -gametype)
    
    ## Match-ups for 2024 playoff brackets
    ### Playoff seeds by team
    playoff_team_seeding <- data.frame(
      team_name = c("BOS", "MIA", "CLE", "ORL", "MIL", "IND", "NYK", "PHI",
                    "OKC", "NOP", "LAC", "DAL", "MIN", "PHX", "DEN", "LAL"),
      seed = c(1, 8, 4, 5, 3, 6, 2, 7,
               1, 8, 4, 5, 3, 6, 2, 7),
      stringsAsFactors = FALSE
    )
    
    ## Round 1 initial matchups
    initial_matchups <- list(
      "East" = list("BOS" = "MIA", "CLE" = "ORL", "MIL" = "IND", "NYK" = "PHI"),
      "West" = list("OKC" = "NOP", "LAC" = "DAL", "MIN" = "PHX", "DEN" = "LAL")
    )
    
    ## Create the playoff bracket
    initial_playoff_bracket <- create_round_bracket(initial_matchups,1, playoff_team_seed)
    
    ## Join in latest features from last regular season games for each team
    initial_bracket_features <- bracket_with_features(initial_playoff_bracket, most_recent_ratings,most_recent_rolling_features,most_recent_player_features) %>%
      # Add matchup_id
      mutate(
        matchup_id = pmap_chr(list(h_team, a_team, h_seed, a_seed), ~ {
          teams <- c(..1, ..2)
          seeds <- c(..3, ..4)
          sorted_teams <- teams[order(seeds)]
          paste(sorted_teams, collapse = "-")
        })
      ) %>%
      group_by(matchup_id) %>%
      # Add seed_id
      mutate(
        seed_id = pmap_chr(list(h_seed, a_seed), ~paste(sort(c(...)), collapse = "-"))
      ) %>%
      select(
        conference:a_seed,
        matchup_id,
        seed_id,
        starts_with("diff_"),
        h_cumulative_unique_lineups,
        a_cumulative_unique_lineups
      )
    
    ## Initial bracket tracker
    initial_bracket_checker <- run_series(initial_bracket_features,xgb_last)
    
    ## Track winners and losers from R1
    e_r1_1_to_8_winner <- initial_bracket_checker$winner[initial_bracket_checker$seed_id == "1-8" & initial_bracket_checker$conference == "East"]
    e_r1_1_to_8_loser <- initial_bracket_checker$loser[initial_bracket_checker$seed_id == "1-8" & initial_bracket_checker$conference == "East"]
    e_r1_4_to_5_winner <- initial_bracket_checker$winner[initial_bracket_checker$seed_id == "4-5" & initial_bracket_checker$conference == "East"]
    e_r1_4_to_5_loser <- initial_bracket_checker$loser[initial_bracket_checker$seed_id == "4-5" & initial_bracket_checker$conference == "East"]
    e_r1_3_to_6_winner <- initial_bracket_checker$winner[initial_bracket_checker$seed_id == "3-6" & initial_bracket_checker$conference == "East"]
    e_r1_3_to_6_loser <- initial_bracket_checker$loser[initial_bracket_checker$seed_id == "3-6" & initial_bracket_checker$conference == "East"]
    e_r1_2_to_7_winner <- initial_bracket_checker$winner[initial_bracket_checker$seed_id == "2-7" & initial_bracket_checker$conference == "East"]
    e_r1_2_to_7_loser <- initial_bracket_checker$loser[initial_bracket_checker$seed_id == "2-7" & initial_bracket_checker$conference == "East"]
    
    w_r1_1_to_8_winner <- initial_bracket_checker$winner[initial_bracket_checker$seed_id == "1-8" & initial_bracket_checker$conference == "West"]
    w_r1_1_to_8_loser <- initial_bracket_checker$loser[initial_bracket_checker$seed_id == "1-8" & initial_bracket_checker$conference == "West"]
    w_r1_4_to_5_winner <- initial_bracket_checker$winner[initial_bracket_checker$seed_id == "4-5" & initial_bracket_checker$conference == "West"]
    w_r1_4_to_5_loser <- initial_bracket_checker$loser[initial_bracket_checker$seed_id == "4-5" & initial_bracket_checker$conference == "West"]
    w_r1_3_to_6_winner <- initial_bracket_checker$winner[initial_bracket_checker$seed_id == "3-6" & initial_bracket_checker$conference == "West"]
    w_r1_3_to_6_loser <- initial_bracket_checker$loser[initial_bracket_checker$seed_id == "3-6" & initial_bracket_checker$conference == "West"]
    w_r1_2_to_7_winner <- initial_bracket_checker$winner[initial_bracket_checker$seed_id == "2-7" & initial_bracket_checker$conference == "West"]
    w_r1_2_to_7_loser <- initial_bracket_checker$loser[initial_bracket_checker$seed_id == "2-7" & initial_bracket_checker$conference == "West"]
    
    print("R1 successful")
    
    ## Get R2 matchups
    r2_matchups <- list(
      "East" = setNames(list(e_r1_4_to_5_winner, e_r1_3_to_6_winner),
                        c(e_r1_1_to_8_winner, e_r1_2_to_7_winner)),
      "West" = setNames(list(w_r1_4_to_5_winner, w_r1_3_to_6_winner),
                        c(w_r1_1_to_8_winner, w_r1_2_to_7_winner))
    )
    
    ## Get R2 bracket
    r2_playoff_bracket <- create_round_bracket(r2_matchups, 2, playoff_team_seed) %>%
      # Add matchup_id for alignment purposes
      mutate(
        matchup_id = pmap_chr(list(h_team, a_team), ~paste(sort(c(...)), collapse = "-"))
      )
    
    ## Align bracket before adding features
    r2_playoff_bracket_aligned <- align_bracket_seeding(r2_playoff_bracket)
    
    ## Add features to R2 bracket
    r2_playoff_bracket_features <- bracket_with_features(r2_playoff_bracket_aligned, most_recent_ratings,most_recent_rolling_features,most_recent_player_features) %>%
      # Add matchup_id
      mutate(
        matchup_id = pmap_chr(list(h_team, a_team, h_seed, a_seed), ~ {
          teams <- c(..1, ..2)
          seeds <- c(..3, ..4)
          sorted_teams <- teams[order(seeds)]
          paste(sorted_teams, collapse = "-")
        })
      ) %>%
      group_by(matchup_id) %>%
      # Add seed_id
      mutate(
        seed_id = pmap_chr(list(h_seed, a_seed), ~paste(sort(c(...)), collapse = "-"))
      ) %>%
      select(
        conference:a_seed,
        matchup_id,
        seed_id,
        starts_with("diff_"),
        h_cumulative_unique_lineups,
        a_cumulative_unique_lineups
      )
    
    ## R2 results
    r2_bracket_checker <- run_series(r2_playoff_bracket_features,xgb_last)
    
    ## Track winners and losers from R2
    e_r2_u_winner <- r2_bracket_checker$winner[
      (r2_bracket_checker$seed_id == "1-4" | r2_bracket_checker$seed_id == "1-5" | 
         r2_bracket_checker$seed_id == "4-8" | r2_bracket_checker$seed_id == "5-8") &
        r2_bracket_checker$conference == "East"]
    
    e_r2_u_loser <- r2_bracket_checker$loser[
      (r2_bracket_checker$seed_id == "1-4" | r2_bracket_checker$seed_id == "1-5" | 
         r2_bracket_checker$seed_id == "4-8" | r2_bracket_checker$seed_id == "5-8") &
        r2_bracket_checker$conference == "East"]
    
    e_r2_l_winner <- r2_bracket_checker$winner[
      (r2_bracket_checker$seed_id == "2-3" | r2_bracket_checker$seed_id == "2-6" | 
         r2_bracket_checker$seed_id == "3-7" | r2_bracket_checker$seed_id == "6-7") &
        r2_bracket_checker$conference == "East"]
    
    e_r2_l_loser <- r2_bracket_checker$loser[
      (r2_bracket_checker$seed_id == "2-3" | r2_bracket_checker$seed_id == "2-6" | 
         r2_bracket_checker$seed_id == "3-7" | r2_bracket_checker$seed_id == "6-7") &
        r2_bracket_checker$conference == "East"]
    
    w_r2_u_winner <- r2_bracket_checker$winner[
      (r2_bracket_checker$seed_id == "1-4" | r2_bracket_checker$seed_id == "1-5" | 
         r2_bracket_checker$seed_id == "4-8" | r2_bracket_checker$seed_id == "5-8") &
        r2_bracket_checker$conference == "West"]
    
    w_r2_u_loser <- r2_bracket_checker$loser[
      (r2_bracket_checker$seed_id == "1-4" | r2_bracket_checker$seed_id == "1-5" | 
         r2_bracket_checker$seed_id == "4-8" | r2_bracket_checker$seed_id == "5-8") &
        r2_bracket_checker$conference == "West"]
    
    w_r2_l_winner <- r2_bracket_checker$winner[
      (r2_bracket_checker$seed_id == "2-3" | r2_bracket_checker$seed_id == "2-6" | 
         r2_bracket_checker$seed_id == "3-7" | r2_bracket_checker$seed_id == "6-7") &
        r2_bracket_checker$conference == "West"]
    
    w_r2_l_loser <- r2_bracket_checker$loser[
      (r2_bracket_checker$seed_id == "2-3" | r2_bracket_checker$seed_id == "2-6" | 
         r2_bracket_checker$seed_id == "3-7" | r2_bracket_checker$seed_id == "6-7") &
        r2_bracket_checker$conference == "West"]
    
    print("R2 successful")
    
    ## Get Conference finals matchups
    r3_matchups <- list(
      "East" = setNames(list(e_r2_l_winner),
                        c(e_r2_u_winner)),
      "West" = setNames(list(w_r2_l_winner),
                        c(w_r2_u_winner))
    )
    
    ## Get Conference finals bracket
    r3_playoff_bracket <- create_round_bracket(r3_matchups, 3, playoff_team_seed) %>%
      # Add matchup_id for alignment purposes
      mutate(
        matchup_id = pmap_chr(list(h_team, a_team), ~paste(sort(c(...)), collapse = "-"))
      )
    
    ## Align seeding for R2 bracket
    r3_playoff_bracket_aligned <- align_bracket_seeding(r3_playoff_bracket)
    
    ## Add features to R2 bracket
    r3_playoff_bracket_features <- bracket_with_features(r3_playoff_bracket_aligned, most_recent_ratings,most_recent_rolling_features,most_recent_player_features) %>%
      # Add matchup_id
      mutate(
        matchup_id = pmap_chr(list(h_team, a_team, h_seed, a_seed), ~ {
          teams <- c(..1, ..2)
          seeds <- c(..3, ..4)
          sorted_teams <- teams[order(seeds)]
          paste(sorted_teams, collapse = "-")
        })
      ) %>%
      group_by(matchup_id) %>%
      # Add seed_id
      mutate(
        seed_id = pmap_chr(list(h_seed, a_seed), ~paste(sort(c(...)), collapse = "-"))
      ) %>%
      select(
        conference:a_seed,
        matchup_id,
        seed_id,
        starts_with("diff_"),
        h_cumulative_unique_lineups,
        a_cumulative_unique_lineups
      )
    
    ## Conference finals results
    r3_bracket_checker <- run_series(r3_playoff_bracket_features,xgb_last)
    
    ## Set R4 matchups
    cf_potential_matchups <- get_cf_potential_matchups()
    
    ## Track winners and losers for R4 bracket
    e_r3_winner <- r3_bracket_checker %>%
      filter(seed_id %in% cf_potential_matchups &
               conference == "East") %>%
      pull(winner)
    
    e_r3_loser <- r3_bracket_checker %>%
      filter(seed_id %in% cf_potential_matchups &
               conference == "East") %>%
      pull(loser)
    
    w_r3_winner <- r3_bracket_checker %>%
      filter(seed_id %in% cf_potential_matchups &
               conference == "West") %>%
      pull(winner)
    
    w_r3_loser <- r3_bracket_checker %>%
      filter(seed_id %in% cf_potential_matchups &
               conference == "West") %>%
      pull(loser)
    
    print("R3 successful")
    
    ## Get Finals matchups
    r4_matchups <- list(
      "Both" = setNames(list(e_r3_winner),
                        c(w_r3_winner))
    )
    
    ## Get Finals bracket
    r4_playoff_bracket <- create_round_bracket(r4_matchups, 4, playoff_team_seed) %>%
      # Add matchup_id for alignment purposes
      mutate(
        matchup_id = pmap_chr(list(h_team, a_team), ~paste(sort(c(...)), collapse = "-"))
      )
    
    # Align seeding for Finals bracket
    r4_playoff_bracket_aligned <- align_bracket_seeding(r4_playoff_bracket)
    
    # Add features to Finals bracket
    r4_playoff_bracket_features <- bracket_with_features(r4_playoff_bracket_aligned, most_recent_ratings,most_recent_rolling_features,most_recent_player_features) %>%
      # Add matchup_id
      mutate(
        matchup_id = pmap_chr(list(h_team, a_team, h_seed, a_seed), ~ {
          teams <- c(..1, ..2)
          seeds <- c(..3, ..4)
          sorted_teams <- teams[order(seeds)]
          paste(sorted_teams, collapse = "-")
        })
      ) %>%
      group_by(matchup_id) %>%
      # Add seed_id
      mutate(
        seed_id = pmap_chr(list(h_seed, a_seed), ~paste(sort(c(...)), collapse = "-"))
      ) %>%
      select(
        conference:a_seed,
        matchup_id,
        seed_id,
        starts_with("diff_"),
        h_cumulative_unique_lineups,
        a_cumulative_unique_lineups
      )
    
    ## Finals results
    r4_bracket_checker <- run_series(r4_playoff_bracket_features,xgb_last)
    
    ## Track winners and losers for Final
    f_r4_winner <- r4_bracket_checker %>% 
      filter(conference == "Both") %>%
      pull(winner)
    
    f_r4_loser <- r4_bracket_checker %>% 
      filter(conference == "Both") %>%
      pull(loser)
    
    print("R4 successful")

    # Add all series results to res list
    results_list[[sim_no]] <- bind_rows(initial_bracket_checker, r2_bracket_checker, r3_bracket_checker, r4_bracket_checker)
    results_list[[sim_no]]$sim_num <- sim_no
    
    final_series_list[[sim_no]] <- data.frame(
      simulation_id = rep(sim_no, 16),
      conference = c(
        "East","East","East","East","West","West","West","West",
        "East","East","West","West",
        "East" , "West",
        "Both",
        "Both"
      ),
      round_made = c(
        1,1,1,1,1,1,1,1,
        2,2,2,2,
        3,3,
        4,
        5
      ),
      team_name = c(
        e_r1_1_to_8_loser,e_r1_4_to_5_loser,e_r1_3_to_6_loser,e_r1_2_to_7_loser,w_r1_1_to_8_loser,w_r1_4_to_5_loser,w_r1_3_to_6_loser,w_r1_2_to_7_loser,
        e_r2_u_loser,e_r2_l_loser,w_r2_u_loser,w_r2_l_loser,
        e_r3_loser,w_r3_loser,
        f_r4_loser,
        f_r4_winner
      )
    )
    
    print(paste("Finished Sim:",sim_no))
  }
  
  results <- bind_rows(results_list)
  final_series <- bind_rows(final_series_list)
  
  resultdf <- list('results' = results, 'final_series' = final_series)
  print("All sims complete!")
  
  return(resultdf)
}

## Function to get probability of winning between 2 teams in a given round
get_series_prediction_2024 <- function(round, team1, team2, type = "Point Estimate", playoff_team_seed=playoff_team_seeding) {
  # Set league seeds for both inputted teams
  team1_seed <- playoff_team_seed %>% filter(team_name == team1) %>% pull(league_seed)
  team2_seed <-playoff_team_seed %>% filter(team_name == team2) %>% pull(league_seed)
  
  # Get matchup in format as sim results
  if (team1_seed < team2_seed) {
    matchup = paste0(team1,'-',team2)
  } else {
    matchup = paste0(team2,'-',team1)
  }
  
  # Adjust round text output for Finals series
  if (round == "Finals") {
    round_txt = "the Finals"
  } else {
    round_txt = round
  }
  
  if (type == "Point Estimate") {
    title <- "Series Win- Point Estimate"
    
    # Get probability of each team winning overall across rounds 
    results_summary <- all_results %>%
      filter(
        matchup_id == matchup
        & round_name == round
      ) %>%
      group_by(matchup_id, round_number, round_name,winner) %>%
      summarise(
        win_count = n(), # Count the number of times each team has won
        avg_total_games = mean(total_games), # Average number of total games played
        .groups = 'drop'
      ) %>%
      group_by(matchup_id, round_number) %>%
      mutate(
        win_pct = win_count / sum(win_count) # Calculate win percentage
      ) %>%
      ungroup() %>%
      # Join the logo URLs with the main data frame
      left_join(logo_mapping, by = c("winner" = "team_name")) %>%
      # Drop unncessary cols
      select(logo_url, win_pct, avg_total_games)
    
    # Create the table using gt
    res_table <- gt(results_summary) %>%
      tab_header(
        title = title,
        subtitle = paste(round,"series games between", team1, "and", team2)
      ) %>%
      cols_label(
        logo_url = "Team",
        win_pct = "Win %",
        avg_total_games = "Avg # of Games",
      ) %>%
      fmt_percent(
        columns = c(win_pct),
        decimals = 1
      ) %>%
      fmt_number(
        columns = c(avg_total_games),
        decimals = 1
      ) %>%
      tab_options(table.width = pct(20)) %>%
      gt_img_rows(logo_url) %>%
      tab_source_note("*The % chance that a team wins a series.") %>%
      gt_theme_538() %>%
      tab_style(
        style = cell_text(align = 'center'),
        locations = cells_column_labels(columns = everything())
      ) %>%
      tab_style(
        style = cell_text(align = 'center'),
        locations = cells_body(columns = everything())
      )
  } else {
    # Get probability of each team winning by n games
    title <- "Series Win- Probabilistic"
    
    results_summary <- all_results %>%
      filter(
        matchup_id == matchup
        & round_name == round
      ) %>%
      group_by(matchup_id, round_number, round_name,total_games,winner) %>%
      summarise(
        win_count = n(), # Count the number of times each team has won
        .groups = 'drop'
      ) %>%
      group_by(matchup_id, round_number,total_games) %>%
      mutate(
        win_pct = win_count / sum(win_count) # Calculate win percentage
      ) %>%
      ungroup() %>%
      # Join the logo URLs with the main data frame
      left_join(logo_mapping, by = c("winner" = "team_name")) %>%
      # Drop unncessary cols
      select(logo_url, win_pct,total_games)
      
    # Create the table using gt
    res_table <- gt(results_summary) %>%
      tab_header(
        title = title,
        subtitle = paste(round,"series games between", team1, "and", team2)
      ) %>%
      cols_label(
        logo_url = "Team",
        win_pct = "Win %",
        total_games = "# of Games",
      ) %>%
      fmt_percent(
        columns = c(win_pct),
        decimals = 1
      ) %>%
      tab_options(table.width = pct(20)) %>%
      gt_img_rows(logo_url) %>%
      tab_source_note("*The % chance that a team wins a given series in a given number of games.") %>%
      gt_theme_538() %>%
      tab_style(
        style = cell_text(align = 'center'),
        locations = cells_column_labels(columns = everything())
      ) %>%
      tab_style(
        style = cell_text(align = 'center'),
        locations = cells_body(columns = everything())
      )
  }
  return(res_table)
}

# Static variables ---------------------------

## Logo URLs mapped to team names
logo_mapping <- data.frame(
  team_name = c("BOS", "MIA", "CLE", "ORL", "MIL", "IND", "NYK", "PHI", "OKC", "NOP", "LAC", "DAL", "MIN", "PHX", "DEN", "LAL"),
  logo_url = c(
    "https://content.sportslogos.net/logos/6/213/thumbs/slhg02hbef3j1ov4lsnwyol5o.gif",
    "https://content.sportslogos.net/logos/6/214/thumbs/burm5gh2wvjti3xhei5h16k8e.gif",
    "https://content.sportslogos.net/logos/6/222/thumbs/22253692023.gif",
    "https://content.sportslogos.net/logos/6/217/thumbs/wd9ic7qafgfb0yxs7tem7n5g4.gif",
    "https://content.sportslogos.net/logos/6/225/thumbs/22582752016.gif",
    "https://content.sportslogos.net/logos/6/224/thumbs/22448122018.gif",
    "https://content.sportslogos.net/logos/6/216/thumbs/21671702024.gif",
    "https://content.sportslogos.net/logos/6/218/thumbs/21870342016.gif",
    "https://content.sportslogos.net/logos/6/2687/thumbs/khmovcnezy06c3nm05ccn0oj2.gif",
    "https://content.sportslogos.net/logos/6/4962/thumbs/496292922024.gif",
    "https://content.sportslogos.net/logos/6/236/thumbs/23655422025.gif",
    "https://content.sportslogos.net/logos/6/228/thumbs/22834632018.gif",
    "https://content.sportslogos.net/logos/6/232/thumbs/23296692018.gif",
    "https://content.sportslogos.net/logos/6/238/thumbs/23843702014.gif",
    "https://content.sportslogos.net/logos/6/229/thumbs/22989262019.gif",
    "https://content.sportslogos.net/logos/6/237/thumbs/23773242024.gif"
  )
)

## Seeding for playoffs
playoff_team_seeding <- data.frame(
  team_name = c("BOS", "MIA", "CLE", "ORL", "MIL", "IND", "NYK", "PHI",
                "OKC", "NOP", "LAC", "DAL", "MIN", "PHX", "DEN", "LAL"),
  seed = c(
    1, 8, 4, 5, 3, 6, 2, 7,
    1, 8, 4, 5, 3, 6, 2, 7
  ),
  league_seed = c(
    1, 16, 11, 12, 8, 15, 7, 14,
    2, 9, 5, 6, 4, 10, 3, 13
  ),
  stringsAsFactors = FALSE
)

# Load libraries/data ---------------------------

# Load in packages
packages <- c("tidyverse", "tidymodels", "ggplot2", "RcppRoll", "vip", "doParallel","xgboost","lme4", "finetune", "PlayerRatings", "gt","gtExtras")
install_and_load(packages)

# Import relevant csv data
player_data <- read_csv("data/player_game_data.csv", show_col_types = FALSE)
team_data <- read_csv("data/team_game_data.csv", show_col_types = FALSE)

# Initial setup ---------------------------

## Get game level data
game_level <- team_data %>%
  filter(season >= 2014 & off_home == 1) %>%
  arrange(season, gamedate, nbagameid) %>%
  mutate(gamedate = as.Date(gamedate)) %>%
  select(season:gamedate,off_team,off_win,fg2made:shotattemptpoints) %>%
  rename_with(~ paste0("h_", .), fg2made:shotattemptpoints) %>%
  rename("h_team" = off_team, "is_win" = off_win) %>%
  inner_join(team_data %>%
               filter(season >= 2014 & off_home == 0) %>%
               arrange(season, gamedate, nbagameid) %>%
               mutate(gamedate = as.Date(gamedate)) %>%
               select(season,nbagameid,off_team,fg2made:shotattemptpoints) %>%
               rename_with(~ paste0("a_", .), fg2made:shotattemptpoints) %>%
               rename("a_team" = off_team),
             by = c("season","nbagameid")
  ) %>%
  select(season:h_team,a_team,is_win,h_fg2made:h_shotattemptpoints,a_fg2made:a_shotattemptpoints)

# Feature Creation ---------------------------

## Advanced Boxscore Metrics
game_level <- game_level %>%
  # Offensive advanced team stats
  mutate(
    h_ppa = h_shotattemptpoints/h_shotattempts, # Points per attempt
    h_ppp = h_shotattemptpoints/h_possessions, # Points per possession
    h_tov_pct = h_turnovers/(h_shotattempts + h_turnovers), # Turnover %
    h_blk_pct = a_blocksagainst/a_fg2attempted, # Block %
    h_ortg = h_points/(h_possessions/100), # Offensive Rating
    h_drtg = a_points/(a_possessions/100), # Defensive Rating
    h_ntrg = h_ortg - h_drtg, # Net Rating
    h_efg_pct = (h_fgmade + (0.5 * h_fg3made)) / (h_fgattempted * 100), # Effective Field Goal %
    h_ts_pct = h_points / (2 * (h_fgattempted + .475 * h_ftattempted)), # True Shooting %
    h_ft_rate = h_ftmade / h_fgattempted, # Free Throw Rate
  ) %>%
  # Defensive advanced team stats
  mutate(
    a_ppa = a_shotattemptpoints/a_shotattempts, # Points per attempt
    a_ppp = a_shotattemptpoints/a_possessions, # Points per possession
    a_tov_pct = a_turnovers/(a_shotattempts + a_turnovers), # Turnover %
    a_blk_pct = h_blocksagainst/h_fg2attempted, # Block %
    a_ortg = a_points/(a_possessions/100), # Offensive Rating
    a_drtg = h_points/(h_possessions/100), # Defensive Rating
    a_ntrg = a_ortg - a_drtg, # Net Rating
    a_efg_pct = (a_fgmade + (0.5 * a_fg3made)) / (a_fgattempted * 100), # Effective Field Goal %
    a_ts_pct = a_points / (2 * (a_fgattempted + .475 * a_ftattempted)), # True Shooting %
    a_ft_rate = a_ftmade / a_fgattempted, # Free Throw Rate
  )

## Rolling Averages
### Need to convert back to origin 2 row per match df structure
team_level <- game_level %>%
  mutate(h_is_home = 1) %>%
  select(season,nbagameid, gamedate, h_team, h_is_home, h_fg2made:h_shotattemptpoints, h_ppa:h_ft_rate) %>%
  rename_with(~ str_remove_all(., "h_"), h_team:h_ft_rate) %>%
  bind_rows (
    game_level %>% 
      mutate(a_is_home = 0) %>%
      select(season,nbagameid, gamedate, a_team, a_is_home, a_fg2made:a_shotattemptpoints, a_ppa:a_ft_rate) %>%
      rename_with(~ str_remove_all(., "a_"), a_team:a_ft_rate)
  ) %>%
  arrange(season, nbagameid)

### Get rolling avg for box score and advanced stats
rolling_mean_features <- team_level %>%
  mutate_at(
    vars(fg2made:ft_rate), ## Columns for which we want a rolling mean
    .funs = ~ roll_mean(., 5, align = "right", fill = NA) ## Rolling mean for last 5 games 
  ) %>%
  ungroup() %>%
  select(season, nbagameid, team, is_home, fg2made:ft_rate) %>%
  filter(!is.na(fg2made))


## Time/Date Features
### Calculate days since last game and days until next game
days_since_stats <- team_level %>%
  select(season, nbagameid, gamedate, team) %>%
  arrange(season, team, gamedate) %>% # Arrange by season, team, and date
  group_by(season, team) %>% # Group by season and team
  mutate(
    days_since_last_game = c(0, diff(gamedate)),  # Calculate days since last game
    days_until_next_game = as.integer(lead(gamedate) - gamedate) # Calculate days until next game
  ) %>%
  mutate(
    # Reset the days since last game for the first game of each season
    days_since_last_game = if_else(row_number() == 1, NA, days_since_last_game),
    days_until_next_game = if_else(row_number() == n(), NA, as.integer(days_until_next_game))
  ) %>%
  ungroup()

### Calculate the number of games played in the last n days
schedule_features <- days_since_stats %>%
  group_by(season, team) %>%
  mutate(
    games_in_last_7_days = count_games_last_n_days(gamedate, 7),
    games_in_last_6_days = count_games_last_n_days(gamedate, 6),
    games_in_last_5_days = count_games_last_n_days(gamedate, 5),
    games_in_last_4_days = count_games_last_n_days(gamedate, 4),
    games_in_last_3_days = count_games_last_n_days(gamedate, 3),
    games_in_last_2_days = count_games_last_n_days(gamedate, 2),
  ) %>%
  ungroup() %>%
  arrange(season, nbagameid) %>%
  na.omit()

## Player features -> aggregating into team stats
### Get player level game data
player_level <- player_data %>%
  filter(season >= 2014) %>%
  mutate(gamedate = as.Date(gamedate)) %>%
  arrange(season, gamedate, nbagameid, nbateamid)

### Get number of players injured per team per match
player_features <- player_level %>%
  # Calculate various player-specific percentages and metrics
  mutate(
    oreb_pct = reboffensive / offensivereboundchances,  # Offensive rebound percentage
    dreb_pct = rebdefensive / defensivereboundchances,  # Defensive rebound percentage
    tov_pct = turnovers / (fgattempted + turnovers),  # Turnover percentage
    stl_pct = replace(steals / defensivepossessions, is.infinite(steals / defensivepossessions), NA),  # Steal percentage, handling infinite values
    blk_pct = replace(blocks / opponentteamfg2attempted, is.infinite(blocks / opponentteamfg2attempted), NA),  # Block percentage, handling infinite values
    usg_pct = (shotattempts + turnovers) / (teamshotattempts + teamturnovers),  # Usage percentage
    ast_pct = assists / (teamfgmade - (fg3made + fg2made)),  # Assist percentage
    pnt3_pct = fg3made / fg3attempted,  # 3-point success percentage
    pnt2_pct = fg2made / fg2attempted,  # 2-point success percentage
    h_ast_pct = assists / (fgattempted + (0.475 * (ftattempted + assists + turnovers))),  # Hybrid assist percentage
    game_score_metric = points + (0.4 * fgmade) - (0.7 * fgattempted) - (0.4 * (ftattempted - ftmade)) + (0.7 * reboffensive) + (0.3 * rebdefensive) + steals + (0.7 * assists) + (0.7 * blocks) - (0.4 * ((defensivefouls + offensivefouls) - turnovers))  # Game score metric calculation
  ) %>%
  # Group by season, game, and team for summary statistics
  group_by(season, nbagameid, team) %>%
  summarise(
    mean_oreb_pct = mean(oreb_pct, na.rm = TRUE),
    mean_dreb_pct = mean(dreb_pct, na.rm = TRUE),
    mean_tov_pct = mean(tov_pct, na.rm = TRUE),
    mean_stl_pct = mean(stl_pct, na.rm = TRUE),
    mean_blk_pct = mean(blk_pct, na.rm = TRUE),
    mean_usg_pct = mean(usg_pct, na.rm = TRUE),
    mean_ast_pct = mean(ast_pct, na.rm = TRUE),
    max_usg_pct = max(usg_pct, na.rm = TRUE),
    inj_players = sum(missed),  # Total injured players
    avg_mp_starter = mean(seconds[starter == 1], na.rm = TRUE) / 60,  # Average minutes played by starters
    avg_mp_bench = mean(seconds[starter == 0], na.rm = TRUE) / 60,  # Average minutes played by bench players
    pnts_by_starters = sum(points[starter == 1], na.rm = TRUE),  # Points by starters
    pnts_by_bench = sum(points[starter == 0], na.rm = TRUE),  # Points by bench
    sharp_shooters = sum(pnt3_pct > 0.35, na.rm = TRUE),  # Count of sharp shooters
    paint_specialists = sum(pnt2_pct > 0.50, na.rm = TRUE),  # Count of paint specialists
    game_score_metric = mean(game_score_metric, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Join with data to track unique lineups over time
  inner_join(
    player_level %>%
      filter(starter == 1) %>%
      arrange(season, team, nbagameid, nbapersonid) %>%
      group_by(season, team, nbagameid) %>%
      summarise(lineup = paste(nbapersonid, collapse = "-"), .groups = 'drop') %>%
      ungroup() %>%
      group_by(season, team) %>%
      arrange(season, team, nbagameid) %>%
      # Track unique lineups by cumulative count of first occurrences
      mutate(
        cumulative_unique_lineups = cumsum(!duplicated(lineup))
      ) %>%
      ungroup() %>%
      select(-lineup),
    by = c("season", "nbagameid", "team")
  )

## Ratings system
### Run the glicko-2. rating system with set parameters
glicko2_model <- glicko2(
  game_level %>% arrange(season,nbagameid) %>% mutate(nbagameid = row_number()) %>% select(nbagameid,h_team,a_team,is_win),
  status = NULL,
  init = c(2200,250,0.03),
  tau = 1.2,
  history = TRUE
)

### Get historical ratings for each game in training data
hist_ratings <- glicko2_model[2] %>%
  # Convert the matrix to a data frame
  as.data.frame() %>%
  # Add row names as a column for team names
  rownames_to_column(var = "team") %>%
  # Pivot data longer to transform the data from wide to long format
  pivot_longer(
    cols = -team,  # Exclude the team column from pivoting
    names_to = "rating_period",  # Define new column for the original column names
    values_to = "rating"  # Define new column for values
  ) %>%
  # Filter columns that end with '.Lag'
  filter(endsWith(rating_period, ".Lag")) %>%
  # Extract numbers from 'rating_period' strings
  mutate(rating_period = str_extract(rating_period, "\\d+")) %>%
  # Temporarily rename the 'rating' column for lag identification
  rename(is_lag = rating) %>%
  # Join with the main ratings from the Glicko2 model
  left_join(
    glicko2_model[2] %>%
      as.data.frame() %>%
      # Add row names as a column for team names
      rownames_to_column(var = "team") %>%
      # Pivot data longer to transform the data from wide to long format
      pivot_longer(
        cols = -team,  # Exclude the team column from pivoting
        names_to = "rating_period",  # Define new column for the original column names
        values_to = "rating"  # Define new column for values
      ) %>%
      # Filter columns that end with '.Rating'
      filter(endsWith(rating_period, ".Rating")) %>%
      # Extract numbers from 'rating_period' strings
      mutate(rating_period = str_extract(rating_period, "\\d+")),
    by = c("team", "rating_period")
  ) %>%
  # Filter for entries where 'is_lag' is zero and 'rating' is not the initial value (2200)
  filter(is_lag == 0 & rating != 2200) %>%
  # Convert 'rating_period' to numeric for sorting
  mutate(rating_period = as.numeric(rating_period)) %>%
  # Remove the 'is_lag' column
  select(-is_lag) %>%
  # Arrange by 'rating_period' to ensure chronological order
  arrange(rating_period) %>%
  # Map game and season IDs from another dataset
  mutate(
    nbagameid = team_level$nbagameid,
    season = team_level$season
  ) %>%
  # Group by 'season' and 'team' to handle game-level data
  group_by(season, team) %>%
  # Sort within each group by 'nbagameid'
  arrange(nbagameid) %>%
  # Create a lagged 'nbagameid' to link ratings to specific games
  mutate(nbagameid_prev = lag(nbagameid)) %>%
  # Ungroup for further operations
  ungroup() %>%
  # Filter out any missing values in 'nbagameid_prev'
  filter(!is.na(nbagameid_prev))

# Combine features ---------------------------

## Last game lookup helper
last_game_lookup <- team_level %>%
  rename(team = team) %>%
  group_by(season, team) %>%
  arrange(nbagameid) %>%
  mutate(nbagameid_prev = lag(nbagameid)) %>%
  select(season, team, nbagameid, nbagameid_prev) %>% 
  filter(!is.na(nbagameid_prev))

# Preparing the features data frame by joining game data with historical team ratings and player statistics
features <- game_level %>%
  select(season:is_win) %>%
  arrange(season,nbagameid) %>%
  inner_join( 
    last_game_lookup,
    by = c(
      "nbagameid" = "nbagameid",
      "h_team" = "team",
      "season" = "season"
    )
  ) %>%
  rename("h_nbagameid_prev" = nbagameid_prev) %>%
  inner_join(
    last_game_lookup,
    by = c(
      "nbagameid" = "nbagameid",
      "a_team" = "team",
      "season" = "season"
    )
  ) %>%
  rename("a_nbagameid_prev" = nbagameid_prev) %>%
  inner_join(
    hist_ratings %>% arrange(rating_period),
    by = c(
      "season" = "season",
      "h_nbagameid_prev" = "nbagameid",
      "h_team" = "team"
    )
  ) %>%
  rename("h_rating" = rating) %>%
  inner_join(
    hist_ratings %>% arrange(rating_period),
    by = c(
      "season" = "season",
      "a_nbagameid_prev" = "nbagameid",
      "a_team" = "team"
    )
  ) %>%
  rename("a_rating" = rating) %>%
  inner_join(
    rolling_mean_features %>% select(-is_home),
    by = c(
      "season" = "season",
      "h_nbagameid_prev" = "nbagameid",
      "h_team" = "team"
    )
  ) %>%
  rename_with(~ paste0("h_", .), fg2made:ft_rate) %>%
  inner_join(
    rolling_mean_features %>% select(-is_home),
    by = c(
      "season" = "season",
      "a_nbagameid_prev" = "nbagameid",
      "a_team" = "team"
    )
  ) %>%
  rename_with(~ paste0("a_", .), fg2made:ft_rate) %>%
  inner_join(
    player_features,
    by = c(
      "season" = "season",
      "h_nbagameid_prev" = "nbagameid",
      "h_team" = "team"
    )
  ) %>%
  rename_with(~ paste0("h_", .), mean_oreb_pct:cumulative_unique_lineups) %>%
  inner_join(
    player_features,
    by = c(
      "season" = "season",
      "a_nbagameid_prev" = "nbagameid",
      "a_team" = "team"
    )
  ) %>%
  rename_with(~ paste0("a_", .), mean_oreb_pct:cumulative_unique_lineups) %>%
  mutate(
    diff_rating = h_rating - a_rating,
    diff_fg2made = h_fg2made - a_fg2made,
    diff_fg2missed = h_fg2missed - a_fg2missed,
    diff_fg2attempted = h_fg2attempted - a_fg2attempted,
    diff_fg3made = h_fg3made - a_fg3made,
    diff_fg3missed = h_fg3missed - a_fg3missed,
    diff_fg3attempted = h_fg3attempted - a_fg3attempted,
    diff_fgmade = h_fgmade - a_fgmade,
    diff_fgmissed = h_fgmissed - a_fgmissed,
    diff_fgattempted = h_fgattempted - a_fgattempted,
    diff_ftmade = h_ftmade - a_ftmade,
    diff_ftmissed = h_ftmissed - a_ftmissed,
    diff_ftattempted = h_ftattempted - a_ftattempted,
    diff_reboffensive = h_reboffensive - a_reboffensive,
    diff_rebdefensive = h_rebdefensive - a_rebdefensive,
    diff_reboundchance = h_reboundchance - a_reboundchance,
    diff_assists = h_assists - a_assists,
    diff_stealsagainst = h_stealsagainst - a_stealsagainst,
    diff_turnovers = h_turnovers - a_turnovers,
    diff_blocksagainst = h_blocksagainst - a_blocksagainst,
    diff_defensivefouls = h_defensivefouls - a_defensivefouls,
    diff_offensivefouls = h_offensivefouls - a_offensivefouls,
    diff_shootingfoulsdrawn = h_shootingfoulsdrawn - a_shootingfoulsdrawn,
    diff_possessions = h_possessions - a_possessions,
    diff_points = h_points - a_points,
    diff_shotattempts = h_shotattempts - a_shotattempts,
    diff_andones = h_andones - a_andones,
    diff_shotattemptpoints = h_shotattemptpoints - a_shotattemptpoints,
    diff_ppa = h_ppa - a_ppa,
    diff_ppp = h_ppp - a_ppp,
    diff_tov_pct = h_tov_pct - a_tov_pct,
    diff_blk_pct = h_blk_pct - a_blk_pct,
    diff_ortg = h_ortg - a_ortg,
    diff_drtg = h_drtg - a_drtg,
    diff_ntrg = h_ntrg - a_ntrg,
    diff_efg_pct = h_efg_pct - a_efg_pct,
    diff_ts_pct = h_ts_pct - a_ts_pct,
    diff_ft_rate = h_ft_rate - a_ft_rate,
    diff_mean_oreb_pct = h_mean_oreb_pct - a_mean_oreb_pct,
    diff_mean_dreb_pct = h_mean_dreb_pct - a_mean_dreb_pct,
    diff_mean_tov_pct = h_mean_tov_pct - a_mean_tov_pct,
    diff_mean_stl_pct = h_mean_stl_pct - a_mean_stl_pct,
    diff_mean_blk_pct = h_mean_blk_pct - a_mean_blk_pct,
    diff_mean_usg_pct = h_mean_usg_pct - a_mean_usg_pct,
    diff_mean_ast_pct = h_mean_ast_pct - a_mean_ast_pct,
    diff_max_usg_pct = h_max_usg_pct - a_max_usg_pct,
    diff_avg_mp_starter = h_avg_mp_starter - a_avg_mp_starter,
    diff_avg_mp_bench = h_avg_mp_bench - a_avg_mp_bench,
    diff_pnts_by_starters = h_pnts_by_starters - a_pnts_by_starters,
    diff_pnts_by_bench = h_pnts_by_bench - a_pnts_by_bench,
    diff_sharp_shooters = h_sharp_shooters - a_sharp_shooters,
    diff_paint_specialists = h_paint_specialists - a_paint_specialists,
    diff_game_score_metric = h_game_score_metric - a_game_score_metric,
  ) %>%
  select(
    season,
    nbagameid,
    gamedate,
    h_team,
    a_team,
    is_win,
    starts_with("diff_"),
    h_cumulative_unique_lineups,
    a_cumulative_unique_lineups
  ) %>%
  mutate(
    is_win = as.factor(is_win)
  )

# Feature EDA ---------------------------

# Boxscore metrics
features %>%
  mutate(is_win = ifelse(is_win == 1,"Win","Loss")) %>%
  pivot_longer(diff_fg2made:diff_reboundchance, names_to = "stat", values_to = "value") %>%
  ggplot(aes(is_win, value, color = is_win)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~stat, scales = "free_y", nrow = 4, ncol = 4) +
  labs(y = NULL, color = NULL, fill = NULL)

features %>%
  mutate(is_win = ifelse(is_win == 1,"Win","Loss")) %>%
  pivot_longer(diff_assists:diff_shotattemptpoints, names_to = "stat", values_to = "value") %>%
  ggplot(aes(is_win, value, color = is_win)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~stat, scales = "free_y", nrow = 4, ncol = 4) +
  labs(y = NULL, color = NULL, fill = NULL)

# Advanced team and player metrics
features %>%
  mutate(is_win = ifelse(is_win == 1,"Win","Loss")) %>%
  pivot_longer(diff_ppa:diff_max_usg_pct, names_to = "stat", values_to = "value") %>%
  ggplot(aes(is_win, value, color = is_win)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~stat, scales = "free_y", nrow = 3, ncol = 6) +
  labs(y = NULL, color = NULL, fill = NULL)

# Rating feature
features %>%
  mutate(is_win = ifelse(is_win == 1,"Win","Loss")) %>%
  pivot_longer(diff_rating, names_to = "stat", values_to = "value") %>%
  ggplot(aes(is_win, value, color = is_win)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~stat, scales = "free_y", nrow = 1) +
  labs(y = NULL, color = NULL, fill = NULL)

# Cumulative lineup features
features %>%
  mutate(is_win = ifelse(is_win == 1,"Win","Loss")) %>%
  pivot_longer(h_cumulative_unique_lineups:a_cumulative_unique_lineups, names_to = "stat", values_to = "value") %>%
  ggplot(aes(is_win, value, color = is_win)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~stat, scales = "free_y", nrow = 2) +
  labs(y = NULL, color = NULL, fill = NULL)

# Misc Features
features %>%
  mutate(is_win = ifelse(is_win == 1,"Win","Loss")) %>%
  pivot_longer(diff_avg_mp_bench:diff_game_score_metric, names_to = "stat", values_to = "value") %>%
  ggplot(aes(is_win, value, color = is_win)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~stat, scales = "free_y", nrow = 3) +
  labs(y = NULL, color = NULL, fill = NULL)

# Model Creation ---------------------------
## Create Splits (80-20)
splits <- initial_split(
  features,
  prop = 0.8
)

## Create preprocessing recipe
preprocessing_recipe <-
  recipe(is_win ~ ., data = splits %>% training()) %>%
  # removes unecessary columns
  step_rm(season, nbagameid, gamedate, h_team, a_team) %>%
  # removes observations (rows of data) if they contain NA or NaN values
  step_naomit(everything(), skip= TRUE) %>%
  # removes any numeric variables that have zero variance
  step_zv(all_numeric(), -all_outcomes()) %>%
  # remove highly correlated variables
  step_corr(all_numeric(), threshold = 0.8, method = "spearman") 

# #Observe the recipe on features
features_proprocessed <- preprocessing_recipe %>%
  prep() %>%
  bake(splits %>% training())

## Set Seed for reproducibility 
set.seed(123)
feature_folds <- vfold_cv(training(splits), strata = is_win, v = 5)

## Create model specification
xgb_spec <- boost_tree(
  mode = "classification",
  trees = 500,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost")

xgb_spec

## Create model workflow
xgb_wf <- workflow() %>%
  add_recipe(preprocessing_recipe) %>%
  add_model(xgb_spec)

## Hyper-parameter tuning
### Use anova race to tune the grid and save time on poor performing parameter combinations
doParallel::registerDoParallel()

set.seed(345)
xgb_res <- tune_race_anova(
  xgb_wf,
  resamples = feature_folds,
  grid = 30,
  metrics = metric_set(roc_auc),
  control = control_race(verbose_elim = TRUE,save_pred=TRUE)
)

xgb_res

## Plot the parameter combination race
plot_race(xgb_res)

## Collect metrics for the model training 
collect_metrics(xgb_res)

## Show best combination of parameters
show_best(xgb_res, metric = "roc_auc")

## Refit best model on training data and assess performance on test set
xgb_last <-
  xgb_wf %>%
  finalize_workflow(select_best(xgb_res,metric = "roc_auc")) %>%
  last_fit(splits)

## Show metrics
collect_metrics(xgb_last)

## Capture training predictions
xgb_last_pred <- collect_predictions(xgb_last)

## Extract variable importance and plot
xgb_fit <- extract_fit_parsnip(xgb_last)
vip(xgb_fit, num_features = 15)

xgb_last <- readRDS('xgb_last.rds')

## Evaluate ROC curve
xgb_last %>%
  collect_predictions() %>%
  roc_curve(is_win, .pred_1, event_level = "second") %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(linewidth = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    linewidth = 1.2
  )

# 2024 Playoff Simulation ---------------------------
## Set playoff seeding by conference and by league
playoff_team_seeding

## Apply model to the 2024 NBA playoffs (2023 season)
### Set number of simulations
nr_sims <-  50

### Run the sims and get time elapsed
system.time(
  sim_results <- playoff_sim(nr_sims, xgb_last, playoff_team_seeding)
)
# saveRDS(sim_results, "sim_results.rds")

### Set sim outputs to variables
all_results <- sim_results$results
all_final_series <- sim_results$final_series 

results_extended <- 
  all_final_series %>% 
  group_by(round_made, team_name) %>% 
  summarise(
    total = n(),
    .groups = 'drop'
  ) %>% 
  pivot_wider(
    names_from = round_made,
    values_from = c(total),
    values_fill = 0
  ) 

### Processing and summarizing results
results_proportion <- results_extended %>%
  group_by(team_name) %>%
  reframe(across(c(`1`, `2`, `3`, `4`, `5`), 
                ~ .x / nr_sims,
                .names = "Round {col}")) %>%
  rename(
     'Conference Finals' = 'Round 3',
     'Finals' = 'Round 4',
     'Champion' = 'Round 5'
    ) %>%
  arrange(desc(Champion), desc(Finals), desc('Conference Finals')) 

### Join the logo URLs with the main data frame
results_proportion <- results_proportion %>%
  left_join(logo_mapping, by = "team_name")

### Create table
results_proportion %>%
  select(-team_name) %>%
  select(logo_url,"Round 1":"Champion") %>%
  gt() %>%
  tab_header(
    title = "2023 NBA Playoff Simulations",
    subtitle = "The probabilities of teams moving through each round *",
  ) %>%
  fmt_percent(
    columns = c("Round 1", "Round 2", "Conference Finals", "Finals", "Champion"),
    decimals = 1
  ) %>%
  cols_label(
    "logo_url" = "Team",
    "Round 1" = "Rnd 1",
    "Round 2" = "Rnd 2",
    "Conference Finals" = "Conf. Finals",
    "Finals" = "Finals",
    "Champion" = "Champion"
  ) %>%
  tab_options(table.width = pct(35)) %>%
  gt_img_rows(logo_url) %>%
  tab_source_note("*The % chance that a team is eliminated in that round, except for `Champion` stage.") %>%
  tab_source_note("i.e. BOS had a 22% chance of being eliminated or a 68% chance of advancing in Round 1.") %>%
  gt_theme_538() %>%
  tab_style(
    style = cell_text(align = 'center'),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(align = 'center'),
    locations = cells_body(columns = everything())
  )

# 2024 Playoff Series predictor ---------------------------
## Predict a series 
### Point estimate
get_series_prediction_2024("Finals","DAL","BOS", "Point Estimate")

### Probabilistic
get_series_prediction_2024("Finals","DAL","BOS", "Probabilistic")

# Part 3 ---------------------------

# Find teams that made 23 and 24 season playoffs and that under performed in Simulated 2024 Playoffs
p3_playoff_teams <- team_data %>%
  filter((season >= 2022 | season <= 2023) & gametype == 4) %>%
  distinct(off_team) %>%
  rename("team_name" = "off_team")

get_series_prediction_2024("Round 2","IND","NYK", "Point Estimate")
get_series_prediction_2024("Round 1","DEN","LAL", "Point Estimate")

# By assessing the simulation output for the 2024 NBA Playoffs it appears that despite making the playofs two consecutive seasons in a row 
# both the New York Knicks and Lakers underperformed against my models expectations.

# The Knicks had ~X % chance of beating the Indiana Pacers in x number of games to reach the Eastern Conference Finals but lost in 7 games.
# This particular example is a case of injuries affecting the outcome of the series. The Knicks had 4 starting rotation players that played only a combined 
# 3 games due to injury. Without their expected, strongest playoff lineup the Knicks fell in 7 games to the pacers. 

# The Los Angeles Lakers had a X chance of beating the Denver Nuggets in X number of games to reach Round 2 of the playoffs in the West but lost in 5 games.
# Despite the Denver Nuggets being the previous seasons champion, the model still favoured the Lakers due to the lack in decay in 
# the ratings for team strength since their Finals win in the year of the COVID-19 NBA bubble. The Denver Nuggets were clear favourites in this series 
# and the model does not fully account for this. To fix this limitation in the model, an adjustment to the input parameters to the Glicko-2 rating system model 
# that would reduce the numbers of (lost) games before rating decay begins would would be sufficient.


#F ind two teams that had a competitive window of 2 or more consecutive seasons making the playoffs 
# and that under performed your model’s expectations for them, losing series they were expected to win. 
# Why do you think that happened? Classify one of them as bad luck and one of them as relating to a 
# cause not currently accounted for in your model. 
# If given more time and data, how would you use what you found to improve your model?


