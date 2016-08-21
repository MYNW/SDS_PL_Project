## SDS - Final Group Project - Premier League Performance Prediction

##-----------------------------------------------
## Load libraries
library("rvest")
library("purrr")
library(dplyr)
library(readr)
library(stringr)
library(stringi)
library(tidyr)
library(httr)
library(ggplot2)
library(XML)
##-----------------------------------------------
## Scraping team rankings for different seasons

## Create links to the pages to be scraped
base.link = "http://www.statto.com/football/stats/england/premier-league/"
year1 = seq(1992, 2015, by = 1)
year2 = seq(1993, 2016, by = 1)
rank.links = paste0(base.link, year1,"-",year2)
rank.links[1:5]

## Test statto site
# NB! HTTP Error 403 when scraping! Site might be protected. Use the GET function! 
## Grab the team ranking from the first link
x <- GET(rank.links[1], add_headers('user-agent' = 'r'))
x %>%
  read_html() %>%  # Read html 
  html_node(".league-table-mini td:nth-child(5)") %>%  # Grab the text 
  html_text()  # Create text

## Create a scraping function
scrape_ranks = function(link){
  parsed.link = GET(link, add_headers('user-agent' = 'r')) %>% 
    read_html
  season = parsed.link %>%
    html_node(".sz0") %>%
    html_text()
  position = parsed.link %>% 
    html_nodes(".league-table-mini td:nth-child(1)") %>%
    html_text() %>%
    as.numeric()
  club = parsed.link %>% 
    html_nodes(".league-table-mini td:nth-child(2)") %>%
    html_text()
  points = parsed.link %>% 
    html_nodes(".league-table-mini td:nth-child(5)") %>%
    html_text() %>%
    as.numeric()
  return(
    data.frame(season = season,
               position = position,
               club = club,
               points = points)
  )
}

## Scrape away!
PLranks.df = rank.links %>% 
  map_df(scrape_ranks)



##------------------------------------------------------------------------

## Get Team Stats
base.team.link = "http://www.transfermarkt.co.uk/premier-league/startseite/wettbewerb/GB1/plus/?saison_id="
year1 = seq(1992, 2015, by = 1)
team.links = paste0(base.team.link, year1)
team.links[1:5]

## Test
clubs.92 = team.links[1] %>%
  read_html(encoding = "UTF-8") %>%  # Read html 
  html_node(".large-8 :nth-child(2) .table-header") %>%  # Grab the text 
  html_text()  # Create text
clubs.92

## Create a scraping function
scrape_team = function(link){
  parsed.link = link %>% 
    read_html(encoding = "UTF-8")
  season = parsed.link %>%
    html_node(".large-8 :nth-child(2) .table-header") %>%
    html_text()
  club = parsed.link %>%
    html_nodes(".hide-for-pad .vereinprofil_tooltip") %>%
    html_text()
  squad_size = parsed.link %>% 
    html_nodes("#yw1 tbody .zentriert:nth-child(4)") %>%
    html_text() %>%
    as.numeric()
  avg_age = parsed.link %>% 
    html_nodes("tbody .hide-for-pad:nth-child(5)") %>%
    html_text() %>%
    str_replace_all(pattern = "," ,
                    replacement = ".") %>%
    str_trim() 
    as.numeric()
  foreign_players = parsed.link %>% 
    html_nodes("tbody .hide-for-small:nth-child(6)") %>%
    html_text() %>%
    as.numeric()
  return(
    data.frame(season = season,
               club = club,
               squad_size = squad_size,
               avg_age = avg_age,
               foreign_players = foreign_players)
  )
}

## Scrape away again!
PLteams.df = team.links %>% 
  map_df(scrape_team)

##---------------------------------------------------------------

## Get clubs' transfer spending
base.transfer.link = "http://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id="
year1 = seq(1992, 2015, by = 1)
base1.transfer.link = "&s_w=&leihe=0&leihe=1&intern=0&intern=1"
transfer.links = paste0(base.transfer.link, year1, base1.transfer.link)
transfer.links[1:5]

## Testing
transfer.92 = transfer.links[1] %>%
  read_html(encoding = "UTF-8") %>%  # Read html 
  html_nodes(".transfer-einnahmen-ausgaben.redtext") %>%  # Grab the text 
  html_text() %>%
  str_replace_all(pattern = "\\n|\\t|\\r|Expenditures:" ,
                replacement = "") %>%
  str_trim()
transfer.92

## Create a scraping function
scrape_transfer = function(link){
  parsed.link = link %>% 
    read_html(encoding = "UTF-8")
  season = parsed.link %>%
    html_node(".box:nth-child(1) .table-header") %>%
    html_text()
  club = parsed.link %>%
    html_nodes(".table-header .vereinprofil_tooltip:nth-child(2)") %>%
    html_text()
  transfer_spending = parsed.link %>% 
    html_nodes(".transfer-einnahmen-ausgaben.redtext") %>%
    html_text() %>%
    str_replace_all(pattern = "\\n|\\t|\\r|Expenditures:" ,
                    replacement = "") %>%
    str_trim()
  return(
    data.frame(season = season,
               club = club,
               transfer_spending = transfer_spending)
  )
}

## Scrape away again! Takes a couple of minutes to complete. 
PLtransfer.df = transfer.links %>% 
  map_df(scrape_transfer)

##------------------------------------------------------------------------

## Clean/Tidy data - The club names and season codes should be the same
PL_transfer = PLtransfer.df %>%
  mutate(season = str_replace_all(season, "[A-z]", "") %>%
           str_replace_all(pattern = "\\n|\\t|\\r",
                           replacement = "") %>%
           str_trim(), 
         club = str_replace_all(club, "FC|AFC", "") %>%
           str_trim())

PL_teams = PLteams.df %>%
  mutate(season = str_replace_all(season, "[A-z]", "") %>%
           str_trim(), 
         club = str_replace_all(club, "FC|AFC", "") %>%
  str_trim())

PL_ranks = PLranks.df %>%
  mutate(club = ifelse(club == "Wolverhampton Wndrs", "Wolverhampton Wanderers", club))

## Create new data frame with start and end of season
PLranks1 <- PL_ranks %>%
  mutate(season = str_replace_all(season, "[A-z]", "") %>%
           str_trim(),
         season_start = as.numeric(strtrim(season,4)),
         season_end = season_start+1) %>%
  select(season, season_start, season_end, club, points, position)
PLranks1$season = PL_teams$season

## Check if it's the same identifiers in all data frames
sort(unique(PLranks1$club))==sort(unique(PL_teams$club))
sort(unique(PL_transfer$club))==sort(unique(PL_teams$club))
sort(unique(PLranks1$season))==sort(unique(PL_teams$season))
sort(unique(PL_transfer$season))==sort(unique(PL_teams$season))

## Convert transfer spending to full amount 
PL_transfer1 <- PL_transfer %>%
  mutate(amount = as.numeric(str_extract(transfer_spending,
                                         "[[:digit:]]+\\.*[[:digit:]]*")),
         multiplier = str_extract(transfer_spending, "[A-z]"),
         scale = if_else(multiplier == "m", 1000000, 1000),
         total_transfer_spending = amount*scale) %>%
  select(season, club, total_transfer_spending)

## Create new data frame for transfers
PL_transfer <- PL_transfer1 %>%
  mutate(total_transfer_spending = ifelse(is.na(total_transfer_spending),
                                          0,total_transfer_spending))


## Merge into one big data frame
PL_final <- full_join(PLranks1, PL_teams, by = c("season", "club"))
PL_data <- full_join(PL_final, PL_transfer, by = c("season", "club"))

##---------------------------------------------------------------------
## Manager Dummy (Helge)

# scrape data from wikipedia table
link = "https://en.wikipedia.org/wiki/List_of_Premier_League_managers"

css.selector = "td:nth-child(3) a"
manager1.data = link %>% 
  read_html() %>% 
  html_nodes(css = css.selector) %>% 
  html_text()

css.selector = ":nth-child(17) td:nth-child(4)"
manager2.data = link %>% 
  read_html() %>% 
  html_nodes(css = css.selector) %>% 
  html_text()

df = data.frame(month_year = manager2.data,
                club = manager1.data)

# create new variables for year and month
n<-length(df$month_year)
manager_year<-rep(NA,n)
for (i in 1:n) {
  x<-df$month_year[i]
  if(grepl("+1992", x)) {manager_year[i]="1992"}
  else if (grepl("+1993", x)) {manager_year[i]="1993"}
  else if (grepl("+1994", x)) {manager_year[i]="1994"}
  else if (grepl("+1995", x)) {manager_year[i]="1995"}
  else if (grepl("+1996", x)) {manager_year[i]="1996"}
  else if (grepl("+1997", x)) {manager_year[i]="1997"}
  else if (grepl("+1998", x)) {manager_year[i]="1998"}
  else if (grepl("+1999", x)) {manager_year[i]="1999"}
  else if (grepl("+2000", x)) {manager_year[i]="2000"}
  else if (grepl("+2001", x)) {manager_year[i]="2001"}
  else if (grepl("+2002", x)) {manager_year[i]="2002"}
  else if (grepl("+2003", x)) {manager_year[i]="2003"}
  else if (grepl("+2004", x)) {manager_year[i]="2004"}
  else if (grepl("+2005", x)) {manager_year[i]="2005"}
  else if (grepl("+2006", x)) {manager_year[i]="2006"}
  else if (grepl("+2007", x)) {manager_year[i]="2007"}
  else if (grepl("+2008", x)) {manager_year[i]="2008"}
  else if (grepl("+2009", x)) {manager_year[i]="2009"}
  else if (grepl("+2010", x)) {manager_year[i]="2010"}
  else if (grepl("+2011", x)) {manager_year[i]="2011"}
  else if (grepl("+2012", x)) {manager_year[i]="2012"}
  else if (grepl("+2013", x)) {manager_year[i]="2013"}
  else if (grepl("+2014", x)) {manager_year[i]="2014"}
  else if (grepl("+2015", x)) {manager_year[i]="2015"}
  else if (grepl("+2016", x)) {manager_year[i]="2016"}
  else {manager_year[i]="NA" }
}

n<-length(df$month_year)
manager_month<-rep(NA,n)
for (i in 1:n) {
  x<-df$month_year[i]
  if(grepl("+January", x)) {manager_month[i]="January"}
  else if (grepl("+February", x)) {manager_month[i]="February"}
  else if (grepl("+March", x)) {manager_month[i]="March"}
  else if (grepl("+April", x)) {manager_month[i]="April"}
  else if (grepl("+May", x)) {manager_month[i]="May"}
  else if (grepl("+June", x)) {manager_month[i]="June"}
  else if (grepl("+July", x)) {manager_month[i]="July"}
  else if (grepl("+August", x)) {manager_month[i]="August"}
  else if (grepl("+September", x)) {manager_month[i]="September"}
  else if (grepl("+October", x)) {manager_month[i]="October"}
  else if (grepl("+November", x)) {manager_month[i]="November"}
  else if (grepl("+December", x)) {manager_month[i]="December"}
  else {manager_month[i]="NA" }
}

n<-length(df$month_year)
month_number<-rep(NA,n)
for (i in 1:n) {
  x<-df$month_year[i]
  if(grepl("+January", x)) {month_number[i]="1"}
  else if (grepl("+February", x)) {month_number[i]="2"}
  else if (grepl("+March", x)) {month_number[i]="3"}
  else if (grepl("+April", x)) {month_number[i]="4"}
  else if (grepl("+May", x)) {month_number[i]="5"}
  else if (grepl("+June", x)) {month_number[i]="6"}
  else if (grepl("+July", x)) {month_number[i]="7"}
  else if (grepl("+August", x)) {month_number[i]="8"}
  else if (grepl("+September", x)) {month_number[i]="9"}
  else if (grepl("+October", x)) {month_number[i]="10"}
  else if (grepl("+November", x)) {month_number[i]="11"}
  else if (grepl("+December", x)) {month_number[i]="12"}
  else {manager_month[i]="NA" }
}

df$month_number = month_number
df$manager_month = manager_month
df$manager_year = manager_year


# create a subset, that only contains the manager changes from 1992 or later 
# (the wiki table had some information about the 1980s).
df2 = subset(df, manager_year!="NA")
df2$manager_year = as.numeric(df2$manager_year)
class(df2$manager_year)

# create a variable that contains both club name and season.
n<-length(df2$month_year)
change_id<-rep(NA,n)
for (i in 1:n) {
  x<-df2$month_number[i]
  if(x>=5){change_id[i] = paste0(df2$club[i], "_", df2$manager_year[i], "/", df2$manager_year[i] + 1)}
  else if(x<5){change_id[i] = paste0(df2$club[i],"_", df2$manager_year[i] - 1, "/", df2$manager_year[i])}
  else {change_id[i] = "NA"}
}

df2$change_id = change_id

# create in PL_data a similar variable to the change_id variable in df2.
n <- length(PL_data$season)
team_year_id <- rep(NA,n)
for (i in 1:n) {
  x <- PL_data$season[i]
  if (x == "92/93"){team_year_id[i] = paste0(PL_data$club[i], "_", "1992/1993")}
  else if (x == "93/94"){team_year_id[i] = paste0(PL_data$club[i], "_", "1993/1994")}
  else if (x == "94/95"){team_year_id[i] = paste0(PL_data$club[i], "_", "1994/1995")}
  else if (x == "95/96"){team_year_id[i] = paste0(PL_data$club[i], "_", "1995/1996")}
  else if (x == "96/97"){team_year_id[i] = paste0(PL_data$club[i], "_", "1996/1997")}
  else if (x == "97/98"){team_year_id[i] = paste0(PL_data$club[i], "_", "1997/1998")}
  else if (x == "98/99"){team_year_id[i] = paste0(PL_data$club[i], "_", "1998/1999")}
  else if (x == "99/00"){team_year_id[i] = paste0(PL_data$club[i], "_", "1999/2000")}
  else if (x == "00/01"){team_year_id[i] = paste0(PL_data$club[i], "_", "2000/2001")}
  else if (x == "01/02"){team_year_id[i] = paste0(PL_data$club[i], "_", "2001/2002")}
  else if (x == "02/03"){team_year_id[i] = paste0(PL_data$club[i], "_", "2002/2003")}
  else if (x == "03/04"){team_year_id[i] = paste0(PL_data$club[i], "_", "2003/2004")}
  else if (x == "04/05"){team_year_id[i] = paste0(PL_data$club[i], "_", "2004/2005")}
  else if (x == "05/06"){team_year_id[i] = paste0(PL_data$club[i], "_", "2005/2006")}
  else if (x == "06/07"){team_year_id[i] = paste0(PL_data$club[i], "_", "2006/2007")}
  else if (x == "07/08"){team_year_id[i] = paste0(PL_data$club[i], "_", "2007/2008")}
  else if (x == "08/09"){team_year_id[i] = paste0(PL_data$club[i], "_", "2008/2009")}
  else if (x == "09/10"){team_year_id[i] = paste0(PL_data$club[i], "_", "2009/2010")}
  else if (x == "10/11"){team_year_id[i] = paste0(PL_data$club[i], "_", "2010/2011")}
  else if (x == "11/12"){team_year_id[i] = paste0(PL_data$club[i], "_", "2011/2012")}
  else if (x == "12/13"){team_year_id[i] = paste0(PL_data$club[i], "_", "2012/2013")}
  else if (x == "13/14"){team_year_id[i] = paste0(PL_data$club[i], "_", "2013/2014")}
  else if (x == "14/15"){team_year_id[i] = paste0(PL_data$club[i], "_", "2014/2015")}
  else if (x == "15/16"){team_year_id[i] = paste0(PL_data$club[i], "_", "2015/2016")}
  else {team_year_id[i] = "NA"}
}

PL_data$team_year_id = team_year_id


# test
PL_data$team_year_id %in% df2$change_id

# create a new variable that says TRUE for every element in team_year_id that is also in change_id.
# SO it will give me TRUE for every row (club and season) where a manager change took place.
manager_change2 = PL_data$team_year_id %in% df2$change_id

# Make a 1 - 0 rather than TRUE - FALSE variable out of it.
n <- length(PL_data$season)
manager_change <- rep(NA,n)
for (i in 1:n) {
  x = manager_change2[i]
  if (x == "TRUE") {manager_change[i] = 1}
  else {manager_change[i] = 0}
}  

# add the final manager_change dummy to Mai's PL_data framme.
PL_data$manager_change = manager_change

##------------------------------------------------------------------------
## Star-Index by Karolis

##RAW TABLE EXTRACTION
##TOP3 BALLON AWARD 2010-2015
url1 <- "https://en.wikipedia.org/wiki/FIFA_Ballon_d%27Or"
award1 <- url1 %>%
  html() %>%
  ##xPath to First Wiki table
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table(fill = TRUE)
award1 <- award1[[1]]


##TOP3 World Player Award 1991 - 2009
url2 <- "https://en.wikipedia.org/wiki/FIFA_World_Player_of_the_Year"
award2 <- url2 %>%
  html() %>%
  ##xPath to First Wiki table
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table(fill = TRUE)
award2 <- award2[[1]]

##TABLE CLEANUP
##Removing random descriptive text from data frames
award2.clean = award2[-20,]
award1.clean = award1[-1,]
colnames(award1.clean) = award1.clean[1, ]
award1.clean = award1.clean[-1,]

##Removing not needed columns
award1.clean = subset(award1.clean, select = -c(5:6) )
award2.clean = subset(award2.clean, select = -c(2, 4, 6) )

##Removing Playernames and only leaving team namesfrom first dataframe by dropping and creating new variables

n<-length(award1.clean$Year)
First_Place<-rep(NA,n)
for (i in 1:n) {
  x<-award1.clean$"First place"[i]
  if(grepl("+Barcelona", x)) {First_Place[i]="Barcelona"}
  else if (grepl("+Real Madrid", x)) {First_Place[i]="Real Madrid"}
  else if (grepl("+Bayern Munich", x)) {First_Place[i]="Bayern Munich"}
  else {First_Place[i]="NA" }
}

award1.clean$First_Place = First_Place
award1.clean$"First place" = NULL

n<-length(award1.clean$Year)
Second_place<-rep(NA,n)
for (i in 1:n) {
  x<-award1.clean$"Second place"[i]
  if(grepl("+Barcelona", x)) {Second_place[i]="Barcelona"}
  else if (grepl("+Real Madrid", x)) {Second_place[i]="Real Madrid"}
  else if (grepl("+Bayern Munich", x)) {Second_place[i]="Bayern Munich"}
  else {Second_place[i]="NA" }
}

award1.clean$Second_place = Second_place
award1.clean$"Second place" = NULL

n<-length(award1.clean$Year)
Third_Place<-rep(NA,n)
for (i in 1:n) {
  x<-award1.clean$"Third place"[i]
  if(grepl("+Barcelona", x)) {Third_Place[i]="Barcelona"}
  else if (grepl("+Real Madrid", x)) {Third_Place[i]="Real Madrid"}
  else if (grepl("+Bayern Munich", x)) {Third_Place[i]="Bayern Munich"}
  else {Third_Place[i]="NA" }
}

award1.clean$Third_Place = Third_Place
award1.clean$"Third place" = NULL


##Changing column Names to match both frames

##Frame 1
colnames(award1.clean)[2] <- "Club.1"
colnames(award1.clean)[3] <- "Club.2"
colnames(award1.clean)[4] <- "Club.3"

##Frame 2
colnames(award2.clean)[2] <- "Club.1"
colnames(award2.clean)[3] <- "Club.2"
colnames(award2.clean)[4] <- "Club.3"

##Removing 1991 since we only need 1992 and later

award2.clean = award2.clean[-1,]


##Merging both data frames into one
awardsmerged <- merge(award2.clean, award1.clean, all = TRUE)

## Tidy the data
awardsmerged.tidy <- awardsmerged %>%
  gather(ranking, player_club, Club.1:Club.3) %>%
  arrange(Year) %>%
  mutate(star_players = 1,
         Year = as.numeric(Year)) %>%
  select(-ranking)

## Merge datasets
PL_data1 <- left_join(PL_data, awardsmerged.tidy, by = c("season_start" = "Year", 
                                                         "club" = "player_club")) 
## Clean new dummy
PL_data1$star_players[is.na(PL_data1$star_players)] <- 0

##------------------------------------------------------------------------
## Generate a dummy for teams that played internationally that season

#Champions League
link1 = paste0("https://en.wikipedia.org/wiki/English_football_clubs_in_international_competitions")  #link to wiki page
link.data1 = link1 %>%   #
  read_html() %>% 
  html_node(".wikitable:nth-child(16) , tr:nth-child(48) td:nth-child(1) a , tr:nth-child(47) a , tr:nth-child(46) a") %>% 
  # extract first node with class wikitable
  html_table(fill = TRUE) %>% 
  select(Year, Team) %>% 
  filter(Year >= 1992)
# then convert the HTML table into a data frame

#Europe League
link2 = paste0("https://en.wikipedia.org/wiki/English_football_clubs_in_international_competitions")  #link to wiki page
link.data2 = link2 %>%   #
  read_html() %>% 
  html_node(".wikitable:nth-child(19)") %>% 
  # extract first node with class wikitable
  html_table(fill = TRUE) %>% 
  select(Year, Team) %>% 
  filter(Year >= 1992)

#new approach: combining two data frames
df1 = data.frame(link.data1)
df2 = data.frame(link.data2)

df3 = rbind(df1, df2) %>%
  arrange(Year, Team) %>%
  mutate(Year = as.numeric(strtrim(Year,4)),
         played_internationally = 1)

# remove duplicates based on "Year" and "Team"
df4 <-df3[!duplicated(df3),]

# merge with existing data frame
PL_data2 <- left_join(PL_data1, df4, by = c("season_start" = "Year", 
                                                         "club" = "Team")) 
## Clean new dummy
PL_data2$played_internationally[is.na(PL_data2$played_internationally)] <- 0

##------------------------------------------------------------------------
## Prepare a data frame for the first table - check classes of vectors
sapply(PL_data, class)
PL_data$avg_age <- as.numeric(PL_data$avg_age)

## Generate new variables
PL_Table1 <- PL_data2 %>%
  group_by(season) %>%
  mutate(season_median = median(total_transfer_spending),
         season_mean = mean(total_transfer_spending),
         club_transfer_ratio = total_transfer_spending/season_median,
         club_transfer_ratio1 = total_transfer_spending/season_mean,
         log_transfer = log10(total_transfer_spending),
         log_points = log10(points)) %>%
  ungroup() # %>%
  # arrange(club, season) %>%
  # mutate(future_points = points[])

## Highest transfer ratio
PL_Topratio <- PL_Table1 %>%
  arrange(-club_transfer_ratio) %>%
  select(season, club, club_transfer_ratio)
knitr::kable(PL_Topratio[1:5, ])


## Descriptive statistics
# Development in transfer spending (mean and average)
ggplot(PL_Table1, aes(x = season_start, y = season_median)) + 
  geom_bar(stat = "identity")

ggplot(PL_Table1, aes(x = season_start, y = season_mean)) + 
  geom_line()

# Box-plots - Manager changes and points distribution 
ggplot(PL_Table1, aes(x = as.character(manager_change), y = points)) + 
  geom_boxplot() + 
  coord_flip() + 
  xlab("Change in manager during the season?") + 
  ylab("Total points accumulated") +
  scale_x_discrete(breaks = c(0,1), labels = c("No", "Yes"))

# Box-plots - Manager changes and points distribution 
ggplot(PL_Table1, aes(x = as.character(star_players), y = points)) + 
  geom_boxplot() + 
  coord_flip() + 
  xlab("Star player (Ballon d'or nominee) on the team?") + 
  ylab("Total points accumulated") +
  scale_x_discrete(breaks = c(0,1), labels = c("No", "Yes"))

## Make scatterplots - we should consider making log-log models
  # Transfer spending and points (absolute value)
ggplot(PL_Table1, aes(x = total_transfer_spending, y = points)) + 
  geom_point(alpha = .25) + 
  geom_smooth()

  # Transfer spending and points (relative value)
ggplot(PL_Table1, aes(x = log_transfer, y = log_points)) + 
  geom_point(alpha = .25) + 
  geom_smooth()

  # Average team age and points (seemingly negative correlation)
ggplot(PL_Table1, aes(x = avg_age, y = points)) + 
  geom_point(alpha = .25) + 
  geom_smooth()

  # Club transfer ratio and points (using the median)
ggplot(PL_Table1, aes(x = club_transfer_ratio, y = points)) + 
  geom_point(alpha = .25) + 
  geom_smooth() + 
  geom_vline(xintercept = 1)

  # Club transfer ratio and points (using the mean)
ggplot(PL_Table1, aes(x = club_transfer_ratio1, y = points)) + 
  geom_point(alpha = .25) + 
  geom_smooth() + 
  geom_vline(xintercept = 1)


## New data frame for points in past and current season
PL_points = PL_data %>%
  arrange(club, -season_start) %>%
  mutate(points_last = ifelse(season_start != season_end[-1], NA, lead(points)))

## Graph relationship between past and current points accumulated
ggplot(PL_points, aes(x = points_last, y = points)) + 
  geom_point(alpha = .25) + 
  geom_smooth() + 
  labs(x = "Previous Season",
       y = "Current Season",
       title = "Total Points Accumulated, 1992-2016")




