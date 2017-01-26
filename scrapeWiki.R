library(rvest)
library(dplyr)
library(xml2)

goals <- read.csv("goals_data.csv")
scorers <- goals %>%
  select(scorer,subtype, elapsed, id, season) %>%
  mutate(scorer = as.character(scorer))

wikiurl <- "https://en.wikipedia.org/wiki/"

gsub(" ", "_", "Wayne Rooney")

xpath = '//*[@id="mw-content-text"]/table[2]'

rooney <- "https://en.wikipedia.org/wiki/Fernando_Torres" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_table(fill=T)
rooney <- rooney [[1]]
colnames(rooney) <- c("Club" ,"Season", "League Apps", "League Goals", "FA Cup Apps", "FA Cup Goals", "League Cup Apps", "League Cup Goals", "Europe Apps", "Europe Goals", "Other Apps", "Other Goals", "Total Apps", "Total Goals")
rooney <- rooney[-1,] %>%
  mutate(Season = gsub("–", "/20", Season))

rooney %>%
  filter(Season == "2008/2009")

appearances <- function(name, season) {
  name <- gsub(" ", "_", name)
  wikiurl <- "https://en.wikipedia.org/wiki/"
  apps <- paste(wikiurl,name)%>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
    html_table(fill = T)
  apps <- apps[[1]]
  apps <- apps[, - (5:length(apps))] 
  colnames(apps) <- apps[1,]
  apps <- apps %>%
    mutate(Season = gsub("–", "/20", Season))
  apps <- apps %>%
    filter(Season == season)
  return (sum(as.integer(apps$Apps)))
  
}

appearances("Darren Fletcher", "2014/2015")


colnames(rooney)[3]
rooneytest <- rooney[,-(5:length(rooney))]
colnames(rooneytest) <- c("Club", "Season", "League", "League")
paste(colnames(rooneytest), rooneytest[1,], sep = "_")

colnames(rooneytest) <- rooneytest[1,]
goalstest <- head(goals)
goalstest <- goalstest %>%
  mutate(scorer_apps = appearances(scorer), assister_apps = appearances(assister))
