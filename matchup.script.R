library(tidyverse)
library(jsonlite)
library(httr)
library(future)
library(nbastatR)
library(furrr)
library(data.table)

headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

ids <- games$idGame


games <- seasons_schedule(seasons=2021, season_types = "Playoffs") %>% 
  filter(slugMatchup%like%"LAC") %>% 
  slice_max(idGame, n =6)

get_matchup_data <- function(id_vector){
  
  for (id in id_vector){
    url <-paste0("https://stats.nba.com/stats/boxscorematchupsv3?GameID=00",id,"&LeagueID=00&endPeriod=0&endRange=28800&rangeType=0&startPeriod=0&startRange=0")  
    res <- GET(url = url, add_headers(.headers=headers))
    json_resp <- fromJSON(content(res, "text"))
    
    ran <- length(json_resp$boxScoreMatchups$homeTeam$players$matchups)
    ran2 <- length(json_resp$boxScoreMatchups$awayTeam$players$matchups)
    data <- data.frame()
    
    for (x in 1:ran){
      home <- data.frame(json_resp$boxScoreMatchups$homeTeam$players$matchups[[x]]) %>% 
        select(firstName, familyName)
      home$matchup_min <- pull(json_resp$boxScoreMatchups$homeTeam$players$matchups[[x]]$statistics, matchupMinutes)
      home$game_id <- as.character(json_resp$boxScoreMatchups$gameId)
      home$off_player_first <- (as.character(json_resp$boxScoreMatchups$homeTeam$players$firstName[[x]]))
      home$off_player_last <- (as.character(json_resp$boxScoreMatchups$homeTeam$players$familyName[[x]]))
      data <- (bind_rows(data, home))
    }
    
    for (x in 1:ran2){
      away <- data.frame(json_resp$boxScoreMatchups$awayTeam$players$matchups[[x]]) %>% 
        select(firstName, familyName)
      away$matchup_min <- pull(json_resp$boxScoreMatchups$awayTeam$players$matchups[[x]]$statistics, matchupMinutes)
      away$game_id <- as.character(json_resp$boxScoreMatchups$gameId)
      away$off_player_first <- (as.character(json_resp$boxScoreMatchups$awayTeam$players$firstName[[x]]))
      away$off_player_last <- (as.character(json_resp$boxScoreMatchups$awayTeam$players$familyName[[x]]))
      data <- (bind_rows(data, away))
      
      
    }
  }
  
  
  data <- data %>% 
    mutate(off_player = paste0(off_player_first, " ", off_player_last),
           def_player = paste0(firstName, " ", familyName),
           game_id = as.double(game_id)) %>% 
    left_join(games, by = c("game_id" = "idGame")) %>% 
    select(off_player, game_id, slugMatchup, dateGame,def_player, matchup_min) 
  return(data)
}

df <- future_map_dfr(ids, get_matchup_data)
