'
 Code to calculate a team`s record over the last 10 games.
 Use with the NBA_Team_Stats_2021 Function code.
'
Team1_Last10 <- Team_Game_Log %>%
  select(G, `W/L`) %>%
  top_n(10, G) %>%
  mutate(Win_Count = ifelse(`W/L` == 'W', 1, 0),
         Loss_Count = ifelse(`W/L` == 'L', 1, 0)) %>%
  summarise(Wins = sum(Win_Count),
            Losses = sum(Loss_Count)) %>%
  mutate(`Last 10` = str_c(Wins, '-', Losses)) %>%
  select(`Last 10`) 
