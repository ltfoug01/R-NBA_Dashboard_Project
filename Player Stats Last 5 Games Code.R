'
 Code calculates the players per game averages over the last 5 games played.
 Use with the NBA_Team_Stats_2021 Function. Need the team gamelog.
'

#Players Stats Last 5 games----------------------------------------------------------------------

#LAST 5 GAMES
Last5 <- tail(Team_Game_Log, 5) %>%  
  select(Date, `H/A`, Opponent)

#EMPTY DF FOR THE FOR LOOP TO ADD TOO
Players_Last5 <- data.frame()

#FOR LOOP
for (i in seq(1, 5, by = 1)) {
  
  date <- gsub('-', '', Last5$Date[i])       #correct date string
  home_team <- ifelse(Last5$`H/A`[i] == '@', #get last 5 home teams
                      Last5$Opponent[i], 
                      team_abbreviations)
  
  team_url <- str_c('https://www.basketball-reference.com/boxscores/', date ,'0', home_team ,'.html')
  
  raw_data2 <- read_html(team_url) %>%
    html_table(fill = T, header = F)         #raw data - box score
  
  ifelse(Last5$Opponent[i] == home_team,     #choose correct team based on element
         stats <- data.frame(raw_data2[[1]])[-1,],  
         stats <- data.frame(raw_data2[[9]])[-1,])
  
  headers <- head(stats, 1)       #correct headers
  stats <- stats[-c(1,7,16),]     #remove reserves & starters headings  
  stats <- head(stats, -1)        #remove team totals
  colnames(stats) <- c(headers)   #correct headers
  
  stats <- stats %>%
    mutate(MP = round(as.numeric(as.period(ms(MP, quiet = TRUE)), unit = "minutes"))) %>%
    select(Starters, MP, PTS, `FG%`, `3P`, `3PA`, `3P%`, TRB, AST, BLK, STL) %>%
    filter(MP >= 10)
  
  Players_Last5 <- rbind(Players_Last5, stats) #combine results from last 5 games
}

num.col <- colnames(Players_Last5[,-1])                               #numeric cols.
Players_Last5[num.col] <- lapply(Players_Last5[num.col], as.numeric)  #numeric cols.

#PLAYERS LAST 5 GAME AVG.
Players_Last5_Avg <- Players_Last5 %>%
  group_by(Starters) %>%
  summarise(GP = n(),
            MP = mean(MP),
            `PTS/G` = mean(`PTS`),
            `FG%` = mean(`FG%`),
            `3P` = mean(`3P`), 
            `3PA` = mean(`3PA`), 
            `3P%` = mean(`3P%`, na.rm =TRUE), 
            RB = mean(TRB), 
            AST = mean(AST),
            BLK = mean(BLK),
            STL = mean(STL)) %>%
  arrange(desc(GP, MP))


