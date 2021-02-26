'
 Code to calculate a team`s total record.
 Use with the NBA_Team_Stats_2021 Function code.
'
team1_record <- Team_Game_Log %>%
  select(`W/L`) %>%
  mutate(Win_Count = ifelse(`W/L` == 'W', 1, 0),
         Loss_Count = ifelse(`W/L` == 'L', 1, 0)) %>%
  summarise(Wins = sum(Win_Count),
            Loss = sum(Loss_Count)) %>%
  mutate(Record = str_c(Wins, '-', Loss)) %>%
  select(Record)
