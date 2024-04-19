
# import the package
pacman::p_load(tidyverse, lubridate, readxl)

# accessing all the sheets  
sheet <- excel_sheets("combined.xlsx") 

# applying sheet names to dataframe names 
data_frame <- lapply(setNames(sheet, sheet),  
                    function(x) read_excel("combined.xlsx", sheet=x) %>% 
                      mutate_at(c("WRank", "LRank","WPts", "LPts"), as.double)) 

# attaching all dataframes together 
data_frame <- bind_rows(data_frame, .id="Sheet") 

odds_table <- data_frame %>% 
  select(-c("Sheet",
            "ATP",
            "Tournament",
            "Court",
            "W1", "L1",
            "W2", "L2",
            "W3", "L3",
            "W4", "L4",
            "W5", "L5",
            "Wsets", "Lsets")) %>% 
  mutate_at(
    c("Location", "Series", "Surface", "Best of"),
    as.factor) %>% 
  mutate(Date = as.Date(Date))


## BCM compute p1 p2
# convert odds to probabilities
prob_table <- odds_table %>% 
  mutate(P_B365W = B365L / (B365W+B365L), 
         P_B365L = B365W / (B365W+B365L),
         P_CBW = CBL / (CBW+CBL),
         P_CBL = CBW / (CBW+CBL),
         P_EXW = EXL / (EXW+EXL),
         P_EXL = EXW / (EXW+EXL),
         P_LBW = LBL / (LBW+LBL),
         P_LBL = LBW / (LBW+LBL),
         P_IWW = IWL / (IWW+IWL),
         P_IWL = IWW / (IWW+IWL),
         P_PSW = PSL / (PSW+PSL),
         P_PSL = PSW / (PSW+PSL),
         P_SJW = SJL / (SJW+SJL),
         P_SJL = SJW / (SJW+SJL)) %>% 
  select(-c("B365W", "B365L",
            "CBW", "CBL",
            "EXW", "EXL",
            "LBW", "LBL",
            "IWW", "IWL",
            "PSW", "PSL",
            "SJW", "SJL",
            "MaxW", "MaxL",
            "AvgW", "AvgL"))

# convert to logit p
prob_table <- prob_table %>% 
  mutate( L_B365W = log(P_B365W/(1-P_B365W)),
          L_B365L = log(P_B365L/(1-P_B365L)),
          L_CBW = log(P_CBW/(1-P_CBW)),
          L_CBL = log(P_CBL/(1-P_CBL)),
          L_EXW = log(P_EXW/(1-P_EXW)),
          L_EXL = log(P_EXL/(1-P_EXL)),
          L_LBW = log(P_LBW/(1-P_LBW)),
          L_LBL = log(P_LBL/(1-P_LBL)),
          L_IWW = log(P_IWW/(1-P_IWW)),
          L_IWL = log(P_IWL/(1-P_IWL)),
          L_PSW = log(P_PSW/(1-P_PSW)),
          L_PSL = log(P_PSL/(1-P_PSL)),
          L_SJW = log(P_SJW/(1-P_SJW)),
          L_SJL = log(P_SJL/(1-P_SJL)) )

# keep the rows with at least 1 pair of probabilities
prob_table <- prob_table[rowSums(is.na(prob_table[28:41]))!=14,]
N <- rowSums(!is.na(prob_table[28:41]))
# replace NA with 0
prob_table[28:41] <- replace(prob_table[28:41], is.na(prob_table[28:41]), 0)
# remove inf and -inf (odds 1 or 0)
prob_table[28:41] <- replace(prob_table[28:41], prob_table[28:41]==Inf, 0)
prob_table[28:41] <- replace(prob_table[28:41], prob_table[28:41]==-Inf, 0)

# p1(prob of the winner wining) p2(prob of the loser winning) 
prob_table <- prob_table %>% 
  mutate(logit_P1 = rowSums( across(c(L_B365W,L_CBW,L_EXW,
                                      L_LBW,L_IWW,L_PSW,L_SJW))) / (N/2) ) %>% 
  mutate(logit_P2 = rowSums( across(c(L_B365L,L_CBL,L_EXL,
                                      L_LBL,L_IWL,L_PSL,L_SJL))) / (N/2) ) 
prob_table <- prob_table %>% 
  mutate(P1 = exp(logit_P1) / (1+exp(logit_P1)),
         P2 = exp(logit_P2) / (1+exp(logit_P2)) )


## Measure the performance
# Create higher_rank_won column & prob of higher ranked player winning
prob_table <- prob_table %>% 
  mutate(WRank = replace_na(WRank, 100000)) %>% 
  mutate(LRank = replace_na(LRank, 100000)) %>% 
  mutate(higher_R_won = WRank < LRank) %>% 
  mutate(Prob = P1 * higher_R_won + P2 * (1 - higher_R_won))

# Accuracy
W_odds <- prob_table$higher_R_won
acc <- mean(ifelse(prob_table$Prob > 0.5, 1, 0) == W_odds)
acc
# Calibration C
C_odds <- sum(prob_table$Prob) / sum(W_odds)
C_odds
# Log-loss
ll_odds <- -1 / nrow(prob_table) * sum(W_odds * log(prob_table$Prob) +
                                      (1 - W_odds) * log(1 - prob_table$Prob))

ll_odds



##### Compare with ELO method using the same data set ######

C_df <- prob_table %>% select(-c(14:45))

C_wide <- C_df %>% 
  pivot_longer(cols = Winner:Loser, names_to = "result", values_to = "player") %>% 
  arrange(Date)

### Constant K ###
# set the value for k-factor
k <- 25
# player table
player_Cons <- tibble(Name="Last.First", Constant_elo= 1500)
scoresW <- numeric(nrow(C_df))
scoresL <- numeric(nrow(C_df))

for ( n in seq(1,nrow(C_df)) ) {
  id_n <- C_df[n,]$Winner
  id_op <- C_df[n,]$Loser
  if (!id_n %in% player_Cons$Name) {
    player_Cons <- rbind(player_Cons, tibble(Name=id_n, Constant_elo=1500))
  }
  if (!id_op %in% player_Cons$Name) {
    player_Cons <- rbind(player_Cons, tibble(Name=id_op, Constant_elo=1500))
  }
  
  # pull the elo scores of both players
  score_n <- player_Cons %>% filter(Name == id_n) %>% pull(Constant_elo)
  score_op <- player_Cons %>% filter(Name == id_op) %>% pull(Constant_elo)
  # update the df
  scoresW[n] <- score_n
  scoresL[n] <- score_op
  # prob of the first player wins
  prob_1win <- ( 1+10^( (score_op-score_n)/400 ) )^(-1)

  # update the scores of both players accordingly
  score_n <- score_n + k*(1-prob_1win)
  score_op <- score_op + k*(-1+prob_1win)
  
  # update the atp list
  player_Cons["Constant_elo"][player_Cons["Name"] == id_n] <- score_n
  player_Cons["Constant_elo"][player_Cons["Name"] == id_op] <- score_op
  

  if ((n+1) %% 10000 == 0) {print(n+1)}
}
C_df <- C_df %>% mutate(C_eloW = scoresW, C_eloL = scoresL)


C_df <- C_df %>% 
  mutate( pred_C_win = 1 / (1 + 10^((C_eloL-C_eloW)/400)) )

mean(ifelse(C_df$pred_C_win>0.5, 1, 0)== 1)



### FiveThirtyEight ###
delta <- 100
nu <- 5
sigma <- 0.1

# player table for 538
player_538 <- tibble(Name="Last.First", FTE_elo= 1, mt=0)
score_FTEW <- numeric(nrow(C_df))
score_FTEL <- numeric(nrow(C_df))
for ( n in seq(1,nrow(C_df)) ) {
  # get the IDs
  id_n <- C_df[n,]$Winner
  id_op <- C_df[n,]$Loser 
  if (!id_n %in% player_538$Name) {
    player_538 <- rbind(player_538, tibble(Name=id_n, FTE_elo=1500, mt=0))
  }
  if (!id_op %in% player_538$Name) {
    player_538 <- rbind(player_538, tibble(Name=id_op, FTE_elo=1500, mt=0))
  }
  
  # pull the elo scores of both players
  score_n <- player_538 %>% filter(Name == id_n) %>% pull(FTE_elo)
  score_op <- player_538 %>% filter(Name == id_op) %>% pull(FTE_elo)
  # pull mt for both players
  mt_n <- player_538 %>% filter(Name == id_n) %>% pull(mt)
  mt_op <- player_538 %>% filter(Name == id_op) %>% pull(mt)
  
  # update the df 
  score_FTEW[n] <- score_n
  score_FTEL[n] <- score_op
  # compute Ks using FTE
  k_n <- delta/(mt_n+nu)^sigma
  k_op <- delta/(mt_op+nu)^sigma
  # prob of the first player wins
  prob_1win <- ( 1+10^( (score_op-score_n)/400 ) )^(-1)   
  # update the scores of both players accordingly
  score_n <- score_n + k_n*(1-prob_1win)
  score_op <- score_op + k_op*(-1+prob_1win)
  
  # update the atp list, scores and mt
  player_538["FTE_elo"][player_538["Name"] == id_n] <- score_n
  player_538["FTE_elo"][player_538["Name"] == id_op] <- score_op
  player_538["mt"][player_538["Name"] == id_n] <- mt_n + 1
  player_538["mt"][player_538["Name"] == id_op] <- mt_op + 1 
  
  
  if ((n+1) %% 10000 == 0) {print(n+1)}
}
C_df <- C_df %>% mutate(FTE_eloW = score_FTEW, FTE_eloL = score_FTEL)


C_df <- C_df %>% 
  mutate( pred_FTE_win = 1 / (1 + 10^((FTE_eloL-FTE_eloW)/400)) )

mean(ifelse(C_df$pred_FTE_win>0.5, 1, 0)== 1)


# Create higher_score_win columns
C_df <- C_df %>% 
  mutate(higher_ConScore_won = C_eloW > C_eloL) %>% 
  mutate(higher_ConScore = C_eloW * higher_ConScore_won + 
           C_eloL * (1 - higher_ConScore_won)) %>% 
  mutate(lower_ConScore = C_eloW * (1 - higher_ConScore_won) + 
           C_eloL * higher_ConScore_won) %>% 
  mutate(higher_FTEScore_won = FTE_eloW > FTE_eloL) %>% 
  mutate(higher_FTEScore = FTE_eloW* higher_FTEScore_won + 
           FTE_eloL * (1 - higher_FTEScore_won)) %>% 
  mutate(lower_FTEScore = FTE_eloW * (1 - higher_FTEScore_won) + 
           FTE_eloL * higher_FTEScore_won)

C_df <- C_df %>% 
  mutate( pred_C = 1 / (1 + 10^((lower_ConScore-higher_ConScore)/400)) ) %>% 
  mutate( pred_FTE = 1 / (1 + 10^((lower_FTEScore-higher_FTEScore)/400)) )

W_C <- C_df$higher_ConScore_won
W_FTE <- C_df$higher_FTEScore_won

# Accuracy

a_Con <- mean(ifelse(C_df$pred_C > 0.5, 1, 0) == W_C)
a_FTE <- mean(ifelse(C_df$pred_FTE > 0.5, 1, 0) == W_FTE)

# Calibration C
Cali_C <- sum(C_df$pred_C) /sum(W_C)
Cali_C

Cali_FTE <- sum(C_df$pred_FTE) /sum(W_FTE)
Cali_FTE

# log-loss
N <- nrow(C_df)
ll_C <- -1 / N * sum(W_C * log(C_df$pred_C) +
                       (1 - W_C) * log(1 - C_df$pred_C))
ll_C

ll_FTE <- -1 / N * sum(W_FTE * log(C_df$pred_FTE) +
                         (1 - W_FTE) * log(1 - C_df$pred_FTE))
ll_FTE

tibble(name = c("Odds", "constant K", "FTE"), 
       accuracy = c(acc,a_Con,a_FTE),
       log_loss = c(ll_odds,ll_C,ll_FTE),
       calibration = c(C_odds,Cali_C,Cali_FTE))



