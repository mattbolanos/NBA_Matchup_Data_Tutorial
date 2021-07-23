Before we start gathering the matchup data, we need to load the
necessary packages.

    library(tidyverse)
    library(jsonlite)
    library(httr)
    library(future)
    library(nbastatR)
    library(furrr)
    library(data.table)

You don’t need to understand why the below code works (I don’t have a
deep understanding of them myself), but it essentially sets the headers
for us to request data from the NBA API.

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

Then, we pick the games and their IDs from nbastatR. To pick the games
you want, browse the seasons\_schedule() function and filter for those
games. Below are the 6 games from the 2021 Western Conference Finals,
and the ids variable contains the IDs we need.

    games <- seasons_schedule(seasons=2021, season_types = "Playoffs") %>% 
      filter(slugMatchup%like%"LAC") %>% 
      slice_max(idGame, n =6)
    ids <- games$idGame

This function parses the JSON response from the NBA’s site and returns
the matchup data from the game IDs provided in a dataframe.

    get_matchup_data <- function(id_vector){
      
      for (id in id_vector){
        url <-paste0("https://stats.nba.com/stats/boxscorematchupsv3?GameID=00",id,"&LeagueID=00&endPeriod=0&endRange=28800&rangeType=0&startPeriod=0&startRange=0")  
        res <- GET(url = url, add_headers(.headers=headers))
        json_resp <- fromJSON(content(res, "text"))
        
        ran <- length(json_resp$boxScoreMatchups$homeTeam$players$matchups)
        ran2 <- length(json_resp$boxScoreMatchups$awayTeam$players$matchups)
        matchups <- data.frame()
        
        for (x in 1:ran){
          home <- data.frame(json_resp$boxScoreMatchups$homeTeam$players$matchups[[x]]) %>% 
            select(firstName, familyName)
          home$matchup_min <- pull(json_resp$boxScoreMatchups$homeTeam$players$matchups[[x]]$statistics, matchupMinutes)
          home$game_id <- as.character(json_resp$boxScoreMatchups$gameId)
          home$off_player_first <- (as.character(json_resp$boxScoreMatchups$homeTeam$players$firstName[[x]]))
          home$off_player_last <- (as.character(json_resp$boxScoreMatchups$homeTeam$players$familyName[[x]]))
          matchups <- (bind_rows(matchups, home))
        }
        
        for (x in 1:ran2){
          away <- data.frame(json_resp$boxScoreMatchups$awayTeam$players$matchups[[x]]) %>% 
            select(firstName, familyName)
          away$matchup_min <- pull(json_resp$boxScoreMatchups$awayTeam$players$matchups[[x]]$statistics, matchupMinutes)
          away$game_id <- as.character(json_resp$boxScoreMatchups$gameId)
          away$off_player_first <- (as.character(json_resp$boxScoreMatchups$awayTeam$players$firstName[[x]]))
          away$off_player_last <- (as.character(json_resp$boxScoreMatchups$awayTeam$players$familyName[[x]]))
          matchups <- (bind_rows(matchups, away))
          
          
        }
      }
      
      
      matchups <- matchups %>% 
        mutate(off_player = paste0(off_player_first, " ", off_player_last),
               def_player = paste0(firstName, " ", familyName),
               game_id = as.double(game_id)) %>% 
        left_join(games, by = c("game_id" = "idGame")) %>% 
        select(off_player, game_id, slugMatchup, dateGame,def_player, matchup_min) 
      return(matchups)
    }

And to use the function, we will use a dataframe mapping function from
the furrr package.

    matchups <- future_map_dfr(ids, get_matchup_data)
    head(matchups)

    ##     off_player  game_id slugMatchup   dateGame    def_player matchup_min
    ## 1 Terance Mann 42000316   PHX @ LAC 2021-06-30 Deandre Ayton        3:01
    ## 2 Terance Mann 42000316   PHX @ LAC 2021-06-30  Devin Booker        1:58
    ## 3 Terance Mann 42000316   PHX @ LAC 2021-06-30 Mikal Bridges        0:19
    ## 4 Terance Mann 42000316   PHX @ LAC 2021-06-30  Torrey Craig        0:27
    ## 5 Terance Mann 42000316   PHX @ LAC 2021-06-30   Jae Crowder        1:41
    ## 6 Terance Mann 42000316   PHX @ LAC 2021-06-30   Abdel Nader        0:01
