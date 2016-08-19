## Football Data Set from Kaggle
setwd("~/Documents/5. Semester/Social Data Science")

library("RSQLite")
library(dplyr)

dbfile = "~/Documents/5. Semester/Social Data Science/database.sqlite"
sqlite = dbDriver("SQLite")
mydb = dbConnect(sqlite, dbfile)
dbListTables(mydb)

## Generate data frames
country      <- tbl_df(dbGetQuery( mydb,"SELECT * FROM COUNTRY"))
league       <- tbl_df(dbGetQuery( mydb,"SELECT * FROM LEAGUE"))
match        <- tbl_df(dbGetQuery( mydb,"SELECT * FROM MATCH"))
player       <- tbl_df(dbGetQuery( mydb,"SELECT * FROM PLAYER"))
player_stats <- tbl_df(dbGetQuery( mydb,"SELECT * FROM PLAYER_STATS"))
team         <- tbl_df(dbGetQuery( mydb,"SELECT * FROM TEAM"))

## Clean data - simple renaming
country <- country %>% 
  rename(country = name, country_id = id)
league  <- league %>%
  rename(league = name) %>%
  select(country_id, league) %>%
  left_join(country, by = "country_id")
team  <- team %>% 
  rename(team_id = id)
head(league)

player_stats <- player_stats %>%
  rename(player_stats_id = id) %>%
  left_join(player, by = "player_api_id")



