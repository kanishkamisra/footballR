library(dplyr)
library(RSQLite)
library(XML)
library(data.table)
library(tidyr)

# Make a connection to the sqlite database.
con <- dbConnect(SQLite(), dbname="database.sqlite")
dbListTables(con)

# Extract matches data into a R readable dataframe. This data set contains all matches that have played in major leagues since the 2008/09 season
matches <- tbl_df(dbGetQuery(con,
                             "SELECT m.id, 
                             m.season, 
                             m.date, 
                             t1.team_long_name as 'home',
                             t1.team_short_name as 'home_short', 
                             t2.team_long_name as 'away',
                             t2.team_short_name as 'away_short',
                             m.home_team_goal as 'home_goal',
                             m.away_team_goal as 'away_goal',
                             m.goal,
                             m.shoton,
                             m.shotoff,
                             m.foulcommit,
                             m.card,
                             m.cross,
                             m.corner,
                             m.possession
                             FROM Match m 
                             inner join Team t1 on
                             m.home_team_api_id = t1.team_api_id 
                             inner join Team t2 on
                             m.away_team_api_id = t2.team_api_id
                             WHERE league_id = 1729"
))

# add result variable as a column to the dataframe
matches <- matches %>%
  mutate(result = ifelse(home_goal > away_goal, "home", 
                         ifelse (away_goal > home_goal, "away",
                                 ifelse(home_goal == away_goal, "draw", "NA"))))

# A function to retrieve player name from a given player id in the goal data
playername <- function(x) {
  return(lapply(x, function(x) {
    if(!is.na(x)) {
      return(as.character(dbGetQuery(con,paste("SELECT player_name FROM Player WHERE player_api_id = ", x))))
    }
    else {
      return(NA)
    }
  }))
}

goals <- data.frame()

match_dt <- data.table(matches)
match_dt <- match_dt %>%
  select(id, season, goal)

goals_dt <- match_dt[, list(goal = unlist(xmlToDataFrame(goal) %>% dplyr::select(comment, subtype, elapsed, player1, player2))), by = list(id, season)]

View(unnest(match_dt, goal))

for (goal in matches$goal) {
  add <- tryCatch({
    xmlToDataFrame(goal) %>%
      dplyr::select(comment, subtype, elapsed, player1, player2)
  }, error = function(e) {
    data.frame(comment = NA, subtype = NA, elapsed = NA, player1 = NA, player2 = NA)
  })
  goals <- rbind(goals, add)
  rm(add)
}

goals <- goals %>% 
  mutate(scorer = playername(player1), assister = playername(player2)) %>%
  dplyr::select(comment, subtype, elapsed, scorer, assister)

goals$subtype = as.character(goals$subtype)
goals$scorer = as.character(goals$scorer)
goals$assister = as.character(goals$assister)
goals <- data.frame(goals, stringsAsFactors = F)

goals$subtype[goals$comment == "p"] = "penalty"
goals$subtype[goals$comment == "o"] = "own-goal"
goals$subtype[goals$comment == "rp"] = "retake-penalty"
goals$subtype[goals$comment == "dg"] = "disallowed"
goals$subtype[goals$comment == "psm"] = "penalty-missed"

scorers <- goals %>%
  select(scorer, subtype, elapsed)

assisters <- goals %>%
  select(assister, subtype, elapsed)


scorers <- scorers %>%
  group_by(scorer, subtype) %>%
  summarise(goals = n())

