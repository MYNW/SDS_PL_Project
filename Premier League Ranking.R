## Scraping team rankings for different seasons - TEST

## Load libraries
library("rvest")
library("purrr")
library(dplyr)
library(readr)
library(stringr)
library(stringi)
library(httr)
library(ggplot2)

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


##------------------------------------------------------------------------
## Prepare a data frame for the first table - check classes of vectors
sapply(PL_data, class)
PL_data$avg_age <- as.numeric(PL_data$avg_age)

## Gen new variables
PL_Table1 <- PL_data %>%
  group_by(season) %>%
  mutate(season_median = median(total_transfer_spending),
         season_mean = mean(total_transfer_spending),
         club_transfer_ratio = total_transfer_spending/season_median,
         club_transfer_ratio1 = total_transfer_spending/season_mean,
         log_transfer = log10(total_transfer_spending),
         log_points = log10(points)) %>%
  ungroup() %>%
  # arrange(club, season) %>%
  mutate(future_points = points[])

## Highest transfer ratio
PL_Topratio <- PL_Table1 %>%
  arrange(-club_transfer_ratio) %>%
  select(season, club, club_transfer_ratio)
knitr::kable(PL_Topratio[1:5, ])

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


##------------------------------------------------------------------------
##STOP
##------------------------------------------------------------------------

## Transfer data (detailed)
t.base.link = "http://www.transfermarkt.co.uk/premier-league/transferrekorde/wettbewerb/GB1/ajax/yw1/saison_id/"
t.season = seq(1992, 2016, by = 1)
t.filler = "/land_id/alle/ausrichtung//spielerposition_id//altersklasse//leihe//w_s//zuab/zu/plus/1/page/"
t.pages = seq(1, 4, by = 1)

## Create links
transfer.links = paste0(t.base.link,t.season,t.filler)
transfer.links[1:5]
transfer1.links = sort(outer(transfer.links, t.pages, FUN = paste0))
transfer1.links[1:5]

## Testing
 %>%
  read_html() %>%  # Read html 
  html_node(".league-table-mini td:nth-child(2)") %>%  # Grab the text 
  html_text()  # Create text

##------------------------------------------------------------------------
## Test - Wikipedia
## Grab infor from the first link
rank2016.data = "https://en.wikipedia.org/wiki/2015–16_Premier_League" %>%
  read_html() %>%  # Read html 
  html_node(":nth-child(36)") %>%  # Grab the text 
  html_table()  # Create text

transfer.total.data = "http://www.transferleague.co.uk/premier-league-1992-to-date/transfer-league-tables/premier-league-table-1992-to-date" %>%
  read_html() %>%  # Read html 
  html_node("table") %>%  # Grab the text 
  html_table(fill = FALSE)  # Create text  

