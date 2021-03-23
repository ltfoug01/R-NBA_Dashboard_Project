'
 Code to calculate a team`s home record and away record.
 Use with the NBA_Team_Stats_2021 Function.
'
Team1_Home_Away <- Team_Game_Log %>%
  select(`H/A`, `W/L`) %>%
  mutate(Win_Count_A = ifelse((`W/L` == 'W') & (`H/A` == '@'), 1, 0),     
         Loss_Count_A = ifelse((`W/L` == 'L') & (`H/A` == '@'), 1, 0),
         Win_Count_H = ifelse((`W/L` == 'W') & (`H/A` == ''), 1, 0),
         Loss_Count_H = ifelse((`W/L` == 'L') & (`H/A` == ''), 1, 0)) %>%
  summarise(Home_Wins = sum(Win_Count_H),
            Home_Losses = sum(Loss_Count_H),
            Away_Wins = sum(Win_Count_A),
            Away_Losses = sum(Loss_Count_A)) %>%
  mutate(`Home Record` = str_c(Home_Wins, '-', Home_Losses),
         `Away Record` = str_c(Away_Wins, '-', Away_Losses)) %>%
  select(`Home Record`, `Away Record`)
