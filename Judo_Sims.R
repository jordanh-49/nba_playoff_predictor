library(PlayerRatings)
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(beepr)
library(tidyverse)
library(caret)
library(parallel)
library(zoo)
library(data.table)
library(gtools)
library(vip)
library(gt)
library(gtExtras)
library(sjPlot)
library(tibble)
library(rempsyc)
#install.packages("cli")

judo_W_63 <- readRDS('rds_files/Judo/63kg/judo_63_rating.rds')
trainData<- judo_W_63 %>%
  filter(COMPETITION_ID != 200000030 & MATCH_DATE < '2021-07-27')

sort(unique(trainData$ATHLETE1))
# TrainData
trainData1 <- trainData %>%
  mutate(ATHLETE1 = ifelse(ATHLETE1 == "Cristina Cabaña", "Cristina Cabana", ATHLETE1),
         ATHLETE2 = ifelse(ATHLETE1 == "Cristina Cabaña", "Cristina Cabana", ATHLETE2)
  )

# Bracket
tokyo_bracket <- read.csv('datasets/Judo_63kg_Women_Tokyo_Bracket.csv') %>%
  mutate(ATHLETE1 = ifelse(ATHLETE1 == "Cristina Cabaña", "Cristina Cabana", ATHLETE1),
         ATHLETE2 = ifelse(ATHLETE1 == "Cristina Cabaña", "Cristina Cabana", ATHLETE2)
  )

# Last 32 
last_32 <- read.csv('datasets/Last_32.csv') %>% rename_all(tolower) %>% rename(homeTeam = athlete1,
                                                                               awayTeam = athlete2)

# Get final ratings prior to Tokyo
testing_accuracy_steph <- readRDS("rds_files/Judo/63kg/steph_judo_63kg_accuracy_all.rds")
best_params_steph <- subset(testing_accuracy_steph, test_accuracy == max(testing_accuracy_steph$test_accuracy))

hist_ratings_steph_model <- steph(
  trainData1[,c('RATING_PERIOD','ATHLETE1','ATHLETE2','RESULT_CODE')] %>% na.omit(),
  init = c(best_params_steph$init[1],best_params_steph$dev[1]),
  gamma = best_params_steph$gamma[1],
  cval = best_params_steph$cval[1],
  hval = best_params_steph$hval[1],
  bval = best_params_steph$bval[1],
  lambda = best_params_steph$lambda[1],
  history = TRUE
)

hist_ratings_steph <- hist_ratings_steph_model[1] %>% as.data.frame()

# Join final ratings to Last 32
ratingLast32 <- last_32 %>% select(phase,homeTeam,awayTeam) %>% 
  group_by(homeTeam,awayTeam) %>%
  mutate(homeRating = hist_ratings_steph$ratings.Rating[hist_ratings_steph$ratings.Player == homeTeam],
         awayRating = hist_ratings_steph$ratings.Rating[hist_ratings_steph$ratings.Player == awayTeam])

ratingLast32$homeProb <- predict(hist_ratings_steph_model,
                                       ratingLast32,
                                       tng = 1,
                                       trat = c(1800, 150))

ratingLast32$awayProb <- 1 - ratingLast32$homeProb


judo_sim <- function(sims, ratingInitPhase, initBracket1){
  
  results_list <- list()
  standings <- list()
  
  for (sim_no in 1:sims) { # number of sims to run

    initBracket <- left_join(initBracket1, ratingInitPhase, by = c("homeTeam", "awayTeam","phase"))
    
    initBracket_output <- data.frame()
    
    # Last 32
    for (row_n in 1:nrow(initBracket)) {
      match_row <- initBracket[row_n, ]
      
      outcome <- sample(x = c(1, 0), size = 1, replace = TRUE,
                        prob = c(match_row$homeProb, match_row$awayProb))
      
      match_row$result_code <- outcome
      match_row$simulation_id <- sim_no
      
      initBracket_output <- bind_rows(initBracket_output, match_row)
    }
    
    initBracketResults <- initBracket_output %>% 
      mutate(
        winner = ifelse(result_code == 1, homeTeam, awayTeam),
        loser = ifelse(result_code == 0, homeTeam, awayTeam),
        result_code = as.integer(result_code),
        rating_period = 1,
        rating_period = as.integer(rating_period)
      )
    
    # Assigning Winners and losers
    AUL32w <- as.character(initBracketResults[1, 14])
    AUL32l <- as.character(initBracketResults[1, 15]) # ELIMINATED
    ALU32w <- as.character(initBracketResults[2, 14])
    ALU32l <- as.character(initBracketResults[2, 15]) # ELIMINATED
    ALL32w <- as.character(initBracketResults[3, 14])
    ALL32l <- as.character(initBracketResults[3, 15]) # ELIMINATED
    BUL32w <- as.character(initBracketResults[4, 14])
    BUL32l <- as.character(initBracketResults[4, 15]) # ELIMINATED
    BUU32w <- as.character(initBracketResults[5, 14])
    BUU32l <- as.character(initBracketResults[5, 15]) # ELIMINATED
    BLU32w <- as.character(initBracketResults[6, 14])
    BLU32l <- as.character(initBracketResults[6, 15]) # ELIMINATED
    BLL32w <- as.character(initBracketResults[7, 14])
    BLL32l <- as.character(initBracketResults[7, 15]) # ELIMINATED
    CUL32w <- as.character(initBracketResults[8, 14])
    CUL32l <- as.character(initBracketResults[8, 15]) # ELIMINATED
    CUU32w <- as.character(initBracketResults[9, 14])
    CUU32l <- as.character(initBracketResults[9, 15]) # ELIMINATED
    CLU32w <- as.character(initBracketResults[10, 14])
    CLU32l <- as.character(initBracketResults[10, 15]) # ELIMINATED
    CLL32w <- as.character(initBracketResults[11, 14])
    CLL32l <- as.character(initBracketResults[11, 15]) # ELIMINATED
    DUL32w <- as.character(initBracketResults[12, 14])
    DUL32l <- as.character(initBracketResults[12, 15]) # ELIMINATED
    DUU32w <- as.character(initBracketResults[13, 14])
    DUU32l <- as.character(initBracketResults[13, 15]) # ELIMINATED
    DLU32w <- as.character(initBracketResults[14, 14])
    DLU32l <- as.character(initBracketResults[14, 15]) # ELIMINATED
    DLL32w <- as.character(initBracketResults[15, 14])
    DLL32l <- as.character(initBracketResults[15, 15]) # ELIMINATED
    
    
    last_16 <- data.frame(
      phase = c("AU16", "AL16", "BU16", "BL16", "CU16", "CL16", "DU16", "DL16"),
      homeTeam = c("Clarisse Agbegnenou", ALU32w, BUU32w, BLU32w, CUU32w, CLU32w, DUU32w, DLU32w),
      awayTeam = c(AUL32w, ALL32w, BUL32w, BLL32w, CUL32w, CLL32w, DUL32w, DLL32w)
    )
    
    steph_last32_ratings <- steph(
          initBracketResults[,c('rating_period','homeTeam','awayTeam','result_code')] %>% na.omit(),
          status = hist_ratings_steph_model$ratings,
          init = c(best_params_steph$init[1],best_params_steph$dev[1]),
          gamma = best_params_steph$gamma[1],
          cval = best_params_steph$cval[1],
          hval = best_params_steph$hval[1],
          bval = best_params_steph$bval[1],
          lambda = best_params_steph$lambda[1],
          history = TRUE)
    
    # Join last 16 with ratings
    ratingLast16 <- last_16 %>% select(phase,homeTeam,awayTeam) %>% 
      group_by(homeTeam,awayTeam) %>%
      mutate(homeRating = steph_last32_ratings$ratings.Rating[steph_last32_ratings$ratings.Player == homeTeam],
             awayRating = steph_last32_ratings$ratings.Rating[steph_last32_ratings$ratings.Player == awayTeam])
    
    ratingLast16$homeProb <- predict(steph_last32_ratings,
                                     ratingLast16,
                                     tng = 1,
                                     trat = c(1800, 150))
    
    ratingLast16$awayProb <- 1 - ratingLast16$homeProb
    
    last16_output <- data.frame()
    
    # Last 16
    for (row_n in 1:nrow(ratingLast16)) {
      match_row <- ratingLast16[row_n, ]
      
      outcome <- sample(x = c(1, 0), size = 1, replace = TRUE,
                        prob = c(match_row$homeProb, match_row$awayProb))
      
      match_row$result_code <- outcome
      match_row$simulation_id <- sim_no
      
      last16_output <- bind_rows(last16_output, match_row)
    }
    
    last16Results <- last16_output %>% 
      mutate(
        winner = ifelse(result_code == 1, homeTeam, awayTeam),
        loser = ifelse(result_code == 0, homeTeam, awayTeam),
        result_code = as.integer(result_code),
        rating_period = 1,
        rating_period = as.integer(rating_period)
      )
    
    AU16w <- as.character(last16Results[1, 8])
    AU16l <- as.character(last16Results[1, 9]) # ELIMINATED
    AL16w <- as.character(last16Results[2, 8])
    AL16l <- as.character(last16Results[2, 9]) # ELIMINATED
    
    BU16w <- as.character(last16Results[3, 8])
    BU16l <- as.character(last16Results[3, 9]) # ELIMINATED
    BL16w <- as.character(last16Results[4, 8])
    BL16l <- as.character(last16Results[4, 9]) # ELIMINATED
    
    CU16w <- as.character(last16Results[5, 8])
    CU16l <- as.character(last16Results[5, 9]) # ELIMINATED
    CL16w <- as.character(last16Results[6, 8])
    CL16l <- as.character(last16Results[6, 9]) # ELIMINATED
    
    DU16w <- as.character(last16Results[7, 8])
    DU16l <- as.character(last16Results[7, 9]) # ELIMINATED
    DL16w <- as.character(last16Results[8, 8])
    DL16l <- as.character(last16Results[8, 9]) # ELIMINATED
    
    quarter_finals <- data.frame(
      phase = c("AUQ", "BUQ", "CUQ", "DUQ"),
      homeTeam = c(AU16w, BU16w, CU16w, DU16w),
      awayTeam = c(AL16w, BL16w, CL16w, DL16w)
    )
    
    steph_last16_ratings <- steph(
      last16Results[,c('rating_period','homeTeam','awayTeam','result_code')] %>% na.omit(),
      status = steph_last32_ratings$ratings,
      init = c(best_params_steph$init[1],best_params_steph$dev[1]),
      gamma = best_params_steph$gamma[1],
      cval = best_params_steph$cval[1],
      hval = best_params_steph$hval[1],
      bval = best_params_steph$bval[1],
      lambda = best_params_steph$lambda[1],
      history = TRUE)
    
  
    # Join last quarterfinals with ratings
    ratingQuarterFinal <- quarter_finals %>% select(phase,homeTeam,awayTeam) %>% 
      group_by(homeTeam,awayTeam) %>%
      mutate(homeRating = steph_last16_ratings$ratings.Rating[steph_last16_ratings$ratings.Player == homeTeam],
             awayRating = steph_last16_ratings$ratings.Rating[steph_last16_ratings$ratings.Player == awayTeam])
    
    ratingQuarterFinal$homeProb <- predict(steph_last16_ratings,
                                     ratingQuarterFinal,
                                     tng = 1,
                                     trat = c(1800, 150))
    
    ratingQuarterFinal$awayProb <- 1 - ratingQuarterFinal$homeProb
    
    quarterFinal_output <- data.frame()
    
    # Quarter Finals
    for (row_n in 1:nrow(ratingQuarterFinal)) {
      match_row <- ratingQuarterFinal[row_n, ]
      
      outcome <- sample(x = c(1,0), size = 1, replace = TRUE,
                        prob = c(match_row$homeProb, match_row$awayProb))
      
      match_row$result_code <- outcome
      match_row$simulation_id <- sim_no
      
      quarterFinal_output <- bind_rows(quarterFinal_output, match_row)
    }
    
    quarterFinalResults <- quarterFinal_output %>% 
      mutate(
        winner = ifelse(result_code == 1, homeTeam, awayTeam),
        loser = ifelse(result_code == 0, homeTeam, awayTeam),
        result_code = as.integer(result_code),
        rating_period = 1,
        rating_period = as.integer(rating_period)
      )
    quarterFinalResults$phase
    
    AUQw <- as.character(quarterFinalResults[1, 8]) # Goes to Semi upper bracket
    AUQl <- as.character(quarterFinalResults[1, 9]) # Goes to upper repechage bracket
    
    BUQw <- as.character(quarterFinalResults[2, 8]) # Goes to Semi upper bracket
    BUQl <- as.character(quarterFinalResults[2, 9]) # Goes to upper repechage bracket
    
    CUQw <- as.character(quarterFinalResults[3, 8]) # Goes to Semi lower bracket
    CUQl <- as.character(quarterFinalResults[3, 9]) # Goes to lower repechage bracket
    
    DUQw <- as.character(quarterFinalResults[4, 8]) # Goes to Semi lower bracket
    DUQl <- as.character(quarterFinalResults[4, 9]) # Goes to lower repechage bracket
    
    repechage <- data.frame(
      phase = c("UR", "LR"),
      homeTeam = c(AUQl, CUQl),
      awayTeam = c(BUQl, DUQl)
    )
    
    steph_qf_ratings <- steph(
      quarterFinalResults[,c('rating_period','homeTeam','awayTeam','result_code')] %>% na.omit(),
      status = steph_last16_ratings$ratings,
      init = c(best_params_steph$init[1],best_params_steph$dev[1]),
      gamma = best_params_steph$gamma[1],
      cval = best_params_steph$cval[1],
      hval = best_params_steph$hval[1],
      bval = best_params_steph$bval[1],
      lambda = best_params_steph$lambda[1],
      history = TRUE)
    
    # Join repechage with QF ratings
    ratingRepechage <- repechage %>% select(phase,homeTeam,awayTeam) %>% 
      group_by(homeTeam,awayTeam) %>%
      mutate(homeRating = steph_qf_ratings$ratings.Rating[steph_qf_ratings$ratings.Player == homeTeam],
             awayRating = steph_qf_ratings$ratings.Rating[steph_qf_ratings$ratings.Player == awayTeam])
    
    ratingRepechage$homeProb <- predict(steph_qf_ratings,
                                           ratingRepechage,
                                           tng = 1,
                                           trat = c(1800, 150))
    
    ratingRepechage$awayProb <- 1 - ratingRepechage$homeProb
    
    repechage_output <- data.frame()
    
    # Repechage
    for (row_n in 1:nrow(ratingRepechage)) {
      match_row <-  ratingRepechage[row_n, ]
      
      outcome <- sample(x = c(1,0), size = 1, replace = TRUE,
                        prob = c(match_row$homeProb, match_row$awayProb))
      
      match_row$result_code <- outcome
      match_row$simulation_id <- sim_no
      
      repechage_output <- bind_rows(repechage_output, match_row)
    }
    
    repechageResults <-  repechage_output %>% 
      mutate(
        winner = ifelse(result_code == 1, homeTeam, awayTeam),
        loser = ifelse(result_code == 0, homeTeam, awayTeam),
        result_code = as.integer(result_code),
        rating_period = 1,
        rating_period = as.integer(rating_period)
      )
    
    RUw <- as.character(repechageResults[1, 8]) # Goes to Bronze medal upper bracket
    RUl <- as.character(repechageResults[1, 9]) # Eliminated
    
    RLw <- as.character(repechageResults[2, 8]) # Goes to Bronze medal lower bracket
    RLl <- as.character(repechageResults[2, 9]) # Eliminated
    
    semi_finals <- data.frame(
      phase = c("US", "LS"),
      homeTeam = c(AUQw, CUQw),
      awayTeam = c(BUQw, DUQw)
    )
    
    steph_sf_ratings <- steph(
      repechageResults[,c('rating_period','homeTeam','awayTeam','result_code')] %>% na.omit(),
      status = steph_qf_ratings$ratings,
      init = c(best_params_steph$init[1],best_params_steph$dev[1]),
      gamma = best_params_steph$gamma[1],
      cval = best_params_steph$cval[1],
      hval = best_params_steph$hval[1],
      bval = best_params_steph$bval[1],
      lambda = best_params_steph$lambda[1],
      history = TRUE)
    
    # Join semis with repechage ratings
    ratingSemiFinal <- semi_finals %>% select(phase,homeTeam,awayTeam) %>% 
      group_by(homeTeam,awayTeam) %>%
      mutate(homeRating = steph_sf_ratings$ratings.Rating[steph_sf_ratings$ratings.Player == homeTeam],
             awayRating = steph_sf_ratings$ratings.Rating[steph_sf_ratings$ratings.Player == awayTeam])
    
    ratingSemiFinal$homeProb <- predict(steph_sf_ratings,
                                        ratingSemiFinal,
                                        tng = 1,
                                        trat = c(1800, 150))
    
    ratingSemiFinal$awayProb <- 1 - ratingSemiFinal$homeProb
    
    semiFinal_output <- data.frame()
    
    # Semi Finals
    for (row_n in 1:nrow( ratingSemiFinal)) {
      match_row <-   ratingSemiFinal[row_n, ]
      
      outcome <- sample(x = c(1,0), size = 1, replace = TRUE,
                        prob = c(match_row$homeProb, match_row$awayProb))
      
      match_row$result_code <- outcome
      match_row$simulation_id <- sim_no
      
      semiFinal_output <- bind_rows(semiFinal_output, match_row)
    }
    
    semiFinalResults <-  semiFinal_output %>% 
      mutate(
        winner = ifelse(result_code == 1, homeTeam, awayTeam),
        loser = ifelse(result_code == 0, homeTeam, awayTeam),
        result_code = as.integer(result_code),
        rating_period = 1,
        rating_period = as.integer(rating_period)
      )
    semiFinalResults$phase
    USw <- as.character(semiFinalResults[1, 8]) # Goes to Bronze medal upper bracket
    USl <- as.character(semiFinalResults[1, 9]) # Eliminated
    
    LSw <- as.character(semiFinalResults[2, 8]) # Goes to Bronze medal lower bracket
    LSl <- as.character(semiFinalResults[2, 9]) # Eliminated
    
    bronze_medal <- data.frame(
      phase = c("UB", "LB"),
      homeTeam = c(LSl, USl),
      awayTeam = c(RUw, RLw)
    )
    
    steph_bronze_ratings <- steph(
      semiFinalResults[,c('rating_period','homeTeam','awayTeam','result_code')] %>% na.omit(),
      status = steph_sf_ratings$ratings,
      init = c(best_params_steph$init[1],best_params_steph$dev[1]),
      gamma = best_params_steph$gamma[1],
      cval = best_params_steph$cval[1],
      hval = best_params_steph$hval[1],
      bval = best_params_steph$bval[1],
      lambda = best_params_steph$lambda[1],
      history = TRUE)
    
    # Join bronze with semi final ratings
    ratingBronze <- bronze_medal %>% select(phase,homeTeam,awayTeam) %>% 
      group_by(homeTeam,awayTeam) %>%
      mutate(homeRating = steph_bronze_ratings$ratings.Rating[steph_bronze_ratings$ratings.Player == homeTeam],
             awayRating = steph_bronze_ratings$ratings.Rating[steph_bronze_ratings$ratings.Player == awayTeam])
    
    ratingBronze$homeProb <- predict(steph_bronze_ratings,
                                        ratingBronze,
                                        tng = 1,
                                        trat = c(1800, 150))
    
    ratingBronze$awayProb <- 1 - ratingBronze$homeProb
    
    bronze_output <- data.frame()
    
    # Bronze Medal
    for (row_n in 1:nrow(ratingBronze)) {
      match_row <- ratingBronze[row_n, ]
      
      outcome <- sample(x = c(1,0), size = 1, replace = TRUE,
                        prob = c(match_row$homeProb, match_row$awayProb))
      
      match_row$result_code <- outcome
      match_row$simulation_id <- sim_no
      
      bronze_output <- bind_rows(bronze_output, match_row)
    }
    
    bronzeResults <-  bronze_output %>% 
      mutate(
        winner = ifelse(result_code == 1, homeTeam, awayTeam),
        loser = ifelse(result_code == 0, homeTeam, awayTeam),
        result_code = as.integer(result_code),
        rating_period = 1,
        rating_period = as.integer(rating_period)
      )
    bronzeResults$phase
    
    UBw <- as.character(bronzeResults[1, 8]) # Wins Bronze
    UBl <- as.character(bronzeResults[1, 9]) # Eliminated
    
    LBw <- as.character(bronzeResults[2, 8]) # Wins Bronze
    LBl <- as.character(bronzeResults[2, 9]) # Eliminated
    
    finals <- data.frame(
      phase = c("UF"),
      homeTeam = c(USw),
      awayTeam = c(LSw)
    )
    
    steph_final_ratings <- steph(
      bronzeResults[,c('rating_period','homeTeam','awayTeam','result_code')] %>% na.omit(),
      status = steph_bronze_ratings$ratings,
      init = c(best_params_steph$init[1],best_params_steph$dev[1]),
      gamma = best_params_steph$gamma[1],
      cval = best_params_steph$cval[1],
      hval = best_params_steph$hval[1],
      bval = best_params_steph$bval[1],
      lambda = best_params_steph$lambda[1],
      history = TRUE)
    
    # Join bronze with semi final ratings
    ratingFinal <- finals %>% select(phase,homeTeam,awayTeam) %>% 
      group_by(homeTeam,awayTeam) %>%
      mutate(homeRating = steph_final_ratings$ratings.Rating[steph_final_ratings$ratings.Player == homeTeam],
             awayRating = steph_final_ratings$ratings.Rating[steph_final_ratings$ratings.Player == awayTeam])
    
    ratingFinal$homeProb <- predict(steph_final_ratings,
                                     ratingFinal,
                                     tng = 1,
                                     trat = c(1800, 150))
    
    ratingFinal$awayProb <- 1 - ratingFinal$homeProb
    
    final_output <- data.frame()
    
    # Gold/Silver Medal
    for (row_n in 1:nrow(ratingFinal)) {
      match_row <- ratingFinal[row_n, ]
      
      outcome <- sample(x = c(1,0), size = 1, replace = TRUE,
                        prob = c(match_row$homeProb, match_row$awayProb))
      
      match_row$result_code <- outcome
      match_row$simulation_id <- sim_no
      
      final_output <- bind_rows(final_output, match_row)
    }
    
    finalResults <-  final_output %>% 
      mutate(
        winner = ifelse(result_code == 1, homeTeam, awayTeam),
        loser = ifelse(result_code == 0, homeTeam, awayTeam),
        result_code = as.integer(result_code),
        rating_period = 1,
        rating_period = as.integer(rating_period)
      )
    finalResults$phase
    
    UFw <- as.character(finalResults[1, 8]) # Wins Gold
    UFl <- as.character(finalResults[1, 9]) # Wins Silver
    
    
    results_list[[sim_no]] <- bind_rows(initBracketResults, last16Results, quarterFinalResults, repechageResults, semiFinalResults, bronzeResults, finalResults)
    
    standings[[sim_no]] <- data.frame(
      simulation_id = sim_no,
      Position = c("Gold","Silver","Bronze","Bronze","5","5","7","7","9","9","9","9","9","9","9","9","17","17","17","17","17","17","17","17","17","17","17","17","17","17","17"),
      Team = c(UFw,UFl,
               UBw,LBw,
               UBl,LBl,
               RUl,RLl,
               AU16l,AL16l,BU16l,BL16l,CU16l,CL16l,DU16l,DL16l,
               AUL32l,ALU32l,ALL32l,BUL32l,BUU32l,BLU32l,BLL32l,CUL32l,CUU32l,CLU32l,CLL32l,DUL32l,DUU32l,DLU32l,DLL32l)
    )
    
    print(paste("Finished Sim:",sim_no))
  }
  
  results <- bind_rows(results_list)
  finalStandings <- bind_rows(standings)
  
  resultdf <- list('fixtures' = results, 'standings' = finalStandings) 
  return(resultdf)
}


nr_sims <-  10

system.time(
  sim_data_steph <- judo_sim(nr_sims, ratingLast32, last_32)
)

results <- sim_data_steph$standings

fixture <- sim_data_steph$fixtures 

results_extended <- 
  results %>% 
  mutate(Position = factor(Position, levels = c("Gold","Silver","Bronze","5","7","9","17"), ordered = TRUE)) %>%
  group_by(Position, Team) %>% 
  summarise(
    Total = n()
  ) %>% 
  pivot_wider(
    names_from = Position,
    values_from = c(Total),
    values_fill = 0
  )

results_proportion <- results_extended %>%
  group_by(Team) %>%
  summarise_all(function(x) x /nr_sims * 100) %>%
  arrange(desc(Gold),desc(Silver),desc(Bronze)) %>%
  rename("5th"="5",
         "7th"="7",
         "9th"="9",
         "17th"="17")

library(rempsyc)
prop_tbl <- nice_table(results_proportion)

print(prop_tbl, preview = "docx")

saveRDS(results,"Simulations/results_10000.rds")
saveRDS(fixture,"Simulations/fixtures_10000.rds")

# Get number of wins/losses and win % of athletes in a given phase for the simulations
fixture_extended <-
fixture %>%
  group_by(phase,winner,loser) %>%
  summarise(
    TotalWins = n()
  ) %>%
  arrange(winner,phase)

winsbyPhasebyAthlete<- fixture %>%
  group_by(phase,winner) %>%
  summarise(
    TotalWins = n()
  ) %>%
  arrange(winner,phase) %>%
  rename(athlete = winner)

lossbyPhasebyAthlete<- fixture %>%
  group_by(phase,loser) %>%
  summarise(
    TotalLosses = n()
  ) %>%
  arrange(loser,phase) %>%
  rename(athlete = loser)
# final result
simByPhaseByAthlete <- full_join(winsbyPhasebyAthlete,lossbyPhasebyAthlete, by = c("athlete","phase")) %>% 
  na.fill(0) %>% 
  as.data.frame() %>%
  mutate(TotalWins = as.integer(TotalWins),
         TotalLosses = as.integer(TotalLosses),
         WinPercentage = round((TotalWins/(TotalWins+TotalLosses) * 100),2)) %>%
  filter(athlete %in% 
           c("Clarisse Agbegnenou","Tina Trstenjak",
             "Catherine Beauchemin-Pinard","Maria Centracchio","Katharina Haecker") &
           !grepl("16", phase) & !grepl("32", phase), 
         !grepl("A", phase) & !grepl("B", phase) & 
           !grepl("C", phase) & !grepl("D", phase)) %>%
  select(athlete,phase,3:5) %>%
  mutate(athlete = factor(athlete, levels = c("Clarisse Agbegnenou","Tina Trstenjak","Catherine Beauchemin-Pinard","Maria Centracchio","Katharina Haecker")),
         phase = factor(phase, levels = c("UF","US","LS","UR","LR"))
  ) %>%
  arrange(athlete,phase)

print(nice_table(simByPhaseByAthlete), preview = "docx")


# 
# 
# # Only Tokyo results
# judo_W_63 <- readRDS('rds_files/Judo/63kg/judo_63_rating.rds')
# tokyo_judo <- judo_W_63 %>%
#   filter(MATCH_DATE >= '2021-07-27' & COMPETITION_ID == "200000030" & PHASE == "Last 32") %>%
#   select(ATHLETE1,ATHLETE2,PHASE)
# 
# unique_judoka <- sort(unique(c(tokyo_judo$ATHLETE1,tokyo_judo$ATHLETE2)))
# 
# unique_seeded <- unique(tokyo_bracket$ATHLETE1)
# 
# # Filter out the names
# unique_unseeded <- unique_judoka[!unique_judoka %in% unique_seeded]
# 
# 


