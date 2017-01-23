library(rvest)
library(dplyr)
library(xml2)

goals <- read.csv("goals_data.csv")
scorers <- goals %>%
  select(scorer,subtype, elapsed, id, season) %>%
  mutate(scorer = as.character(scorer))

url <- "https://en.wikipedia.org/wiki/"

gsub(" ", "_", "Wayne Rooney")

xpath = '//*[@id="mw-content-text"]/table[2]'

rooney <- "https://en.wikipedia.org/wiki/Wayne_Rooney" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
  html_table(fill=T)

colnames(rooney) <- c("Club" ,"Season", "League Apps", "League Goals", "FA Cup Apps", "FA Cup Goals", "League Cup Apps", "League Cup Goals", "Europe Apps", "Europe Goals", "Other Apps", "Other Goals", "Total Apps", "Total Goals")
rooney <- rooney[-1,] %>%
  mutate(Season = gsub("–", "/20", Season))



