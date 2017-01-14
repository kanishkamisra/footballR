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


# extractComments <- function(x) {
#   lapply(x, function(x) {
#     return(as.vector(xpathApply(xmlParseDoc(x), "//*/comment", xmlValue)))
#   })
# }

extractComments <- function(x) {
  sapply(x, function(x) {
    return(xmlToDataFrame(x)$comment)
  })
}

extractSubtypes <- function(x) {
  sapply(x, function(x) {
    return(xmlToDataFrame(x)$subtype)
  })
}

extractElapsed <- function(x) {
  sapply(x, function(x) {
    return(xmlToDataFrame(x)$elapsed)
  })
}

extractPlayer1 <- function(x) {
  sapply(x, function(x) {
    return(xmlToDataFrame(x)$player1)
  })
}

extractPlayer2 <- function(x) {
  sapply(x, function(x) {
    return(xmlToDataFrame(x)$player2)
  })
}

#TESTING  

# test_matches1 <- matches[2000:3040,] %>%
#   select(id, season, goal) %>%
#   mutate(comment = extractComments(goal),
#          subtype = extractSubtypes(goal),
#          elapsed = extractElapsed(goal),
#          player1 = extractPlayer1(goal),
#          player2 = extractPlayer2(goal)) %>%
#   select(-goal)
# 
# test_matches1$subtype[test_matches1$subtype == "NULL"] = test_matches1$comment[test_matches1$subtype == "NULL"]
# 
# what <- test_matches1[1:15,] %>%
#   filter(player1 != "NULL") %>%
#   unnest(comment, subtype, elapsed, player1, player2)

goals <- matches %>%
  select(id, season, goal) %>%
  mutate(comment = extractComments(goal),
         subtype = extractSubtypes(goal),
         elapsed = extractElapsed(goal),
         player1 = extractPlayer1(goal),
         player2 = extractPlayer2(goal)) %>%
  select(-goal) %>%
  filter(comment != "NULL", player1 != "NULL")
  

goals$subtype[goals$subtype == "NULL"] = goals$comment[goals$subtype == "NULL"]
goals$player2[goals$player2 == "NULL"] = goals$player1[goals$player2 == "NULL"]

goals <- unnest(goals, comment, subtype, elapsed, player1, player2)

goals$subtype[goals$comment == "p"] = "penalty"
goals$subtype[goals$comment == "o"] = "own-goal"
goals$subtype[goals$comment == "rp"] = "retake-penalty"
goals$subtype[goals$comment == "dg"] = "disallowed"
goals$subtype[goals$comment == "psm"] = "penalty-missed"


goals_data <- goals %>% 
  mutate(scorer = playername(player1), assister = playername(player2)) %>%
  dplyr::select(id, season, comment, subtype, elapsed, scorer, assister)

write.csv(goals, file = "goals.csv",row.names=FALSE, na="")
          

# goals$subtype = as.character(goals$subtype)
# goals$scorer = as.character(goals$scorer)
# goals$assister = as.character(goals$assister)
# goals <- data.frame(goals, stringsAsFactors = F)



scorers <- goals %>%
  select(scorer, subtype, elapsed)

assisters <- goals %>%
  select(assister, subtype, elapsed)


scorers <- scorers %>%
  group_by(scorer, subtype) %>%
  summarise(goals = n())

