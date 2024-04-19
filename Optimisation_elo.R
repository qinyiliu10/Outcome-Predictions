
# import the package
pacman::p_load(tidyverse, lubridate, patchwork)

# Optimising the hyper-parameters 

Op_table <- prob_table %>% 
  filter(Comment=="Completed") %>% select(-c(11:45))

### split the data ###
# -2018, 2019-
Op_train <- Op_table %>% filter(Date <= "2018-12-31" )
Op_test <- Op_table %>% filter(Date > "2018-12-31")

# different data sets would have different optimal K, delta, nu and sigma, 
# so training and testing sets to prevent overfit.


######### Constant K tuning ########
# the greater the K, the larger the variance in the ELO scores. 
# rewarding the unexpected wins
Op_train <- Op_train %>% mutate(C_scoreW1=0, C_scoreL1=0, 
                                C_scoreW2=0, C_scoreL2=0,
                                C_scoreW3=0, C_scoreL3=0,
                                C_scoreW4=0, C_scoreL4=0,
                                C_scoreW5=0, C_scoreL5=0,)
  
K <- c(5, 10, 25, 50, 100)
for (i in seq(1,5)) {
  
  player_Cons <- tibble(Name="Last.First", Constant_elo= 1500)
  C_scoreW <- numeric(nrow(Op_train))
  C_scoreL <- numeric(nrow(Op_train))
  
  for (n in seq(1,nrow(Op_train))) {
    id_n <- Op_train[n,]$Winner
    id_op <- Op_train[n,]$Loser
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
    C_scoreW[n] <- score_n
    C_scoreL[n] <- score_op
    # prob of the first player wins
    prob_1win <- ( 1+10^( (score_op-score_n)/400 ) )^(-1)
    # update the scores of both players accordingly
    score_n <- score_n + K[i]*(1-prob_1win)
    score_op <- score_op + K[i]*(-1+prob_1win)
    
    # update the atp list
    player_Cons["Constant_elo"][player_Cons["Name"] == id_n] <- score_n
    player_Cons["Constant_elo"][player_Cons["Name"] == id_op] <- score_op
    
    if ((n+1) %% 5000 == 0) {print(n+1)}
  }
  Op_train[,2*i+11] <- C_scoreW
  Op_train[,2*i+12] <- C_scoreL
}

# C1
Op_train <- Op_train %>%
  mutate(higher_won1 = C_scoreW1 > C_scoreL1) %>%
  mutate(higher1 = C_scoreW1 * (higher_won1) +
           C_scoreL1 * (1 - higher_won1)) %>%
  mutate(lower1 = C_scoreW1 * (1 - higher_won1) +
           C_scoreL1 * (higher_won1))

Op_train <- Op_train %>% mutate( pred1 =1/(1+10^((lower1-higher1)/400)))

# C2
Op_train <- Op_train %>%
  mutate(higher_won2 = C_scoreW2 > C_scoreL2) %>%
  mutate(higher2 = C_scoreW2 * (higher_won2) +
           C_scoreL2 * (1 - higher_won2)) %>%
  mutate(lower2 = C_scoreW2 * (1 - higher_won2) +
           C_scoreL2 * (higher_won2))

Op_train <- Op_train %>% mutate( pred2 =1/(1+10^((lower2-higher2)/400)))

# C3
Op_train <- Op_train %>%
  mutate(higher_won3 = C_scoreW3 > C_scoreL3) %>%
  mutate(higher3 = C_scoreW3 * (higher_won3) +
           C_scoreL3 * (1 - higher_won3)) %>%
  mutate(lower3 = C_scoreW3 * (1 - higher_won3) +
           C_scoreL3 * (higher_won3))

Op_train <- Op_train %>% mutate( pred3 =1/(1+10^((lower3-higher3)/400)))

# C4
Op_train <- Op_train %>%
  mutate(higher_won4 = C_scoreW4 > C_scoreL4) %>%
  mutate(higher4 = C_scoreW4 * (higher_won4) +
           C_scoreL4 * (1 - higher_won4)) %>%
  mutate(lower4 = C_scoreW4 * (1 - higher_won4) +
           C_scoreL4 * (higher_won4))

Op_train <- Op_train %>% mutate( pred4 =1/(1+10^((lower4-higher4)/400)))

# C5
Op_train <- Op_train %>%
  mutate(higher_won5 = C_scoreW5 > C_scoreL5) %>%
  mutate(higher5 = C_scoreW5 * (higher_won5) +
           C_scoreL5 * (1 - higher_won5)) %>%
  mutate(lower5 = C_scoreW5 * (1 - higher_won5) +
           C_scoreL5 * (higher_won5))

Op_train <- Op_train %>% mutate( pred5 =1/(1+10^((lower5-higher5)/400)))

########### Metrics ################
# Accuracy
W1 <- Op_train$higher_won1
W2 <- Op_train$higher_won2
W3 <- Op_train$higher_won3
W4 <- Op_train$higher_won4
W5 <- Op_train$higher_won5

Acc1 <- mean(ifelse(Op_train$pred1 > 0.5, 1, 0)==W1)
Acc2 <- mean(ifelse(Op_train$pred2 > 0.5, 1, 0)==W2)
Acc3 <- mean(ifelse(Op_train$pred3 > 0.5, 1, 0)==W3)
Acc4 <- mean(ifelse(Op_train$pred4 > 0.5, 1, 0)==W4)
Acc5 <- mean(ifelse(Op_train$pred5 > 0.5, 1, 0)==W5)
c(Acc1,Acc2,Acc3,Acc4,Acc5)

# Calibration C
C1 <- sum(Op_train$pred1) /sum(W1)
C2 <- sum(Op_train$pred2) /sum(W2)
C3 <- sum(Op_train$pred3) /sum(W3)
C4 <- sum(Op_train$pred4) /sum(W4)
C5 <- sum(Op_train$pred5) /sum(W5)
c(C1,C2,C3,C4,C5)

# log-loss
N <- nrow(Op_train)
ll1 <- -1/N * sum(W1 *log(Op_train$pred1) + (1 - W1) *log(1 - Op_train$pred1))
ll2 <- -1/N * sum(W2 *log(Op_train$pred2) + (1 - W2) *log(1 - Op_train$pred2))
ll3 <- -1/N * sum(W3 *log(Op_train$pred3) + (1 - W3) *log(1 - Op_train$pred3))
ll4 <- -1/N * sum(W4 *log(Op_train$pred4) + (1 - W4) *log(1 - Op_train$pred4))
ll5 <- -1/N * sum(W5 *log(Op_train$pred5) + (1 - W5) *log(1 - Op_train$pred5))
c(ll1,ll2,ll3,ll4,ll5)
#################################


# testing the constant 
Op_k <- 25

#player_Cons <- tibble(Name="Last.First", Constant_elo= 1500)
C_scoreW <- numeric(nrow(Op_test))
C_scoreL <- numeric(nrow(Op_test))
for (n in seq(1,nrow(Op_test))) {
  id_n <- Op_test[n,]$Winner
  id_op <- Op_test[n,]$Loser
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
  C_scoreW[n] <- score_n
  C_scoreL[n] <- score_op
  # prob of the first player wins
  prob_1win <- ( 1+10^( (score_op-score_n)/400 ) )^(-1)
  # update the scores of both players accordingly
  score_n <- score_n + Op_k*(1-prob_1win)
  score_op <- score_op + Op_k*(-1+prob_1win)
  
  # update the atp list
  player_Cons["Constant_elo"][player_Cons["Name"] == id_n] <- score_n
  player_Cons["Constant_elo"][player_Cons["Name"] == id_op] <- score_op
  
  if ((n+1) %% 5000 == 0) {print(n+1)}
}
Op_test <- Op_test %>% mutate(C_scoreWT=C_scoreW, C_scoreLT=C_scoreL)

Op_test <- Op_test %>%
  mutate(higher_wonT = C_scoreWT > C_scoreLT) %>%
  mutate(higherT = C_scoreWT * (higher_wonT) +
           C_scoreLT * (1 - higher_wonT)) %>%
  mutate(lowerT = C_scoreWT * (1 - higher_wonT) +
           C_scoreLT * (higher_wonT))

Op_test <- Op_test %>% mutate( predT =1/(1+10^((lowerT-higherT)/400)))

WT <- Op_test$higher_wonT
mean(ifelse(Op_test$predT > 0.5, 1, 0) == WT) #Accuracy







######## FTE Delta, Nu and Sigma tuning ########

### split the data ###
# -2015, 2016-
FTE_train <- Op_table %>% filter(Date <= "2015-12-31" )
FTE_test <- Op_table %>% filter(Date > "2015-12-31")

FTE_train <- FTE_train %>% mutate(F_scoreW1=0, F_scoreL1=0, 
                                  F_scoreW2=0, F_scoreL2=0,
                                  F_scoreW3=0, F_scoreL3=0,
                                  F_scoreW4=0, F_scoreL4=0,
                                  F_scoreW5=0, F_scoreL5=0,)
FTE_train2 <- FTE_train
FTE_train3 <- FTE_train
FTE_train4 <- FTE_train
FTE_train5 <- FTE_train
train_lst <- list(FTE_train, FTE_train2, FTE_train3, FTE_train4, FTE_train5)

# delta is equivalent to k
Delta <- c(50, 100, 150, 200, 250)
nu <- 5
Sigma <- c(0.5, 0.2, 0.1, 0.05, 0.02)

for (i in seq(1,5)) {
  print(i)
  for (j in seq(1,5)) {
  
    player_538 <- tibble(Name="Last.First", FTE_elo= 1500, mt=0)
    F_scoreW <- numeric(nrow(FTE_train))
    F_scoreL <- numeric(nrow(FTE_train))
    
    for (n in seq(1,nrow(FTE_train))) {
      id_n <- FTE_train[n,]$Winner
      id_op <- FTE_train[n,]$Loser
      if (!id_n %in% player_538$Name) {
        player_538 <- rbind(player_538, tibble(Name=id_n, FTE_elo= 1500, mt=0))
      }
      if (!id_op %in% player_538$Name) {
        player_538 <- rbind(player_538, tibble(Name=id_op, FTE_elo= 1500, mt=0))
      }
      
      # pull the elo scores of both players
      score_n <- player_538 %>% filter(Name == id_n) %>% pull(FTE_elo)
      score_op <- player_538 %>% filter(Name == id_op) %>% pull(FTE_elo)
      # pull mt for both players
      mt_n <- player_538 %>% filter(Name == id_n) %>% pull(mt)
      mt_op <- player_538 %>% filter(Name == id_op) %>% pull(mt)
      # update the df
      F_scoreW[n] <- score_n
      F_scoreL[n] <- score_op
      # compute Ks using FTE
      k_n <- Delta[i]/(mt_n+nu)^Sigma[j]
      k_op <- Delta[i]/(mt_op+nu)^Sigma[j]
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
      
      if ((n+1) %% 5000 == 0) {print(n+1)}
    }
    train_lst[[i]][,2*j+11] <- F_scoreW
    train_lst[[i]][,2*j+12] <- F_scoreL
  }
}
train_lst[[5]] %>% view()

for (i in seq(1,5)) {
  train_lst[[i]] <- train_lst[[i]] %>%
    mutate(higher_won1 = F_scoreW1 > F_scoreL1) %>%
    mutate(higher1 = F_scoreW1 * (higher_won1) +
             F_scoreL1 * (1 - higher_won1)) %>%
    mutate(lower1 = F_scoreW1 * (1 - higher_won1) +
             F_scoreL1 * (higher_won1))
  train_lst[[i]] <- train_lst[[i]] %>% mutate( pred1 =1/(1+10^((lower1-higher1)/400)))
  
  train_lst[[i]] <- train_lst[[i]] %>%
    mutate(higher_won2 = F_scoreW2 > F_scoreL2) %>%
    mutate(higher2 = F_scoreW2 * (higher_won2) +
             F_scoreL2 * (1 - higher_won2)) %>%
    mutate(lower2 = F_scoreW2 * (1 - higher_won2) +
             F_scoreL2 * (higher_won2))
  train_lst[[i]] <- train_lst[[i]] %>% mutate( pred2 =1/(1+10^((lower2-higher2)/400)))
  
  train_lst[[i]] <- train_lst[[i]] %>%
    mutate(higher_won3 = F_scoreW3 > F_scoreL3) %>%
    mutate(higher3 = F_scoreW3 * (higher_won3) +
             F_scoreL3 * (1 - higher_won3)) %>%
    mutate(lower3 = F_scoreW3 * (1 - higher_won3) +
             F_scoreL3 * (higher_won3))
  train_lst[[i]] <- train_lst[[i]] %>% mutate( pred3 =1/(1+10^((lower3-higher3)/400)))
  
  train_lst[[i]] <- train_lst[[i]] %>%
    mutate(higher_won4 = F_scoreW4 > F_scoreL4) %>%
    mutate(higher4 = F_scoreW4 * (higher_won4) +
             F_scoreL4 * (1 - higher_won4)) %>%
    mutate(lower4 = F_scoreW4 * (1 - higher_won4) +
             F_scoreL4 * (higher_won4))
  train_lst[[i]] <- train_lst[[i]] %>% mutate( pred4 =1/(1+10^((lower4-higher4)/400)))
  
  train_lst[[i]] <- train_lst[[i]] %>%
    mutate(higher_won5 = F_scoreW5 > F_scoreL5) %>%
    mutate(higher5 = F_scoreW5 * (higher_won5) +
             F_scoreL5 * (1 - higher_won5)) %>%
    mutate(lower5 = F_scoreW5 * (1 - higher_won5) +
             F_scoreL5 * (higher_won5))
  train_lst[[i]] <- train_lst[[i]] %>% mutate( pred5 =1/(1+10^((lower5-higher5)/400)))
  
}

metrics <- tibble(Accuracy = c(1,2,3,4,5),
                  Cali = c(1,2,3,4,5),
                  LogLoss = c(1,2,3,4,5))
for (i in seq(1,5)) {
  W1 <- train_lst[[i]]$higher_won1
  W2 <- train_lst[[i]]$higher_won2
  W3 <- train_lst[[i]]$higher_won3
  W4 <- train_lst[[i]]$higher_won4
  W5 <- train_lst[[i]]$higher_won5
  
  Acc1 <- mean(ifelse(train_lst[[i]]$pred1 > 0.5, 1, 0)==W1)
  Acc2 <- mean(ifelse(train_lst[[i]]$pred2 > 0.5, 1, 0)==W2)
  Acc3 <- mean(ifelse(train_lst[[i]]$pred3 > 0.5, 1, 0)==W3)
  Acc4 <- mean(ifelse(train_lst[[i]]$pred4 > 0.5, 1, 0)==W4)
  Acc5 <- mean(ifelse(train_lst[[i]]$pred5 > 0.5, 1, 0)==W5)
  
  # Calibration C
  C1 <- sum(train_lst[[i]]$pred1) /sum(W1)
  C2 <- sum(train_lst[[i]]$pred2) /sum(W2)
  C3 <- sum(train_lst[[i]]$pred3) /sum(W3)
  C4 <- sum(train_lst[[i]]$pred4) /sum(W4)
  C5 <- sum(train_lst[[i]]$pred5) /sum(W5)
  
  # log-loss
  N <- nrow(train_lst[[i]])
  ll1 <- -1/N * sum(W1 *log(train_lst[[i]]$pred1) + (1 - W1) *log(1 - train_lst[[i]]$pred1))
  ll2 <- -1/N * sum(W2 *log(train_lst[[i]]$pred2) + (1 - W2) *log(1 - train_lst[[i]]$pred2))
  ll3 <- -1/N * sum(W3 *log(train_lst[[i]]$pred3) + (1 - W3) *log(1 - train_lst[[i]]$pred3))
  ll4 <- -1/N * sum(W4 *log(train_lst[[i]]$pred4) + (1 - W4) *log(1 - train_lst[[i]]$pred4))
  ll5 <- -1/N * sum(W5 *log(train_lst[[i]]$pred5) + (1 - W5) *log(1 - train_lst[[i]]$pred5))
  
  metrics <- cbind(metrics, tibble(Accuracy = c(Acc1,Acc2,Acc3,Acc4,Acc5),
                                   Cali = c(C1,C2,C3,C4,C5),
                                   LogLoss = c(ll1,ll2,ll3,ll4,ll5)))
}
metrics %>% view()

#################### FTE test ###########################


D <- 250
n <- 5
s <- 0.1
# player_538 <- tibble(Name="Last.First", FTE_elo= 1500, mt=0)
F_scoreW <- numeric(nrow(FTE_test))
F_scoreL <- numeric(nrow(FTE_test))
for (n in seq(1,nrow(FTE_test))) {
  id_n <- FTE_test[n,]$Winner
  id_op <- FTE_test[n,]$Loser
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
  F_scoreW[n] <- score_n
  F_scoreL[n] <- score_op
  # compute Ks using FTE
  k_n <- D / (mt_n+n)^s
  k_op <- D / (mt_op+n)^s
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
  
  if ((n+1) %% 5000 == 0) {print(n+1)}
}

FTE_test <- FTE_test %>% mutate(F_scoreWT=F_scoreW, F_scoreLT=F_scoreL)

FTE_test <- FTE_test %>%
  mutate(higher_wonT = F_scoreWT > F_scoreLT) %>%
  mutate(higherT = F_scoreWT * (higher_wonT) +
           F_scoreLT * (1 - higher_wonT)) %>%
  mutate(lowerT = F_scoreWT * (1 - higher_wonT) +
           F_scoreLT * (higher_wonT))

FTE_test <- FTE_test %>% mutate( predT =1/(1+10^((lowerT-higherT)/400)))

WT <- FTE_test$higher_wonT
mean(ifelse(FTE_test$predT > 0.5, 1, 0) == WT) #Accuracy




########################################


df_C1 <- tibble(k = c(5, 10, 25, 50, 100),
                Accuracy = c(0.6515500, 0.6611528, 0.6699177, 0.6700896, 0.6619047), 
                calibration= c(0.9271356, 0.9636376, 1.0091871, 1.0496663, 1.1111207),
                logloss = c(0.6245721, 0.6108037, 0.6021384, 0.6077849, 0.6345802)) |>
  pivot_longer(!k, names_to = "metric", values_to = "value") |>
  mutate_at("metric", as.factor)
ggplot(aes(x = k, y = value, color = metric), data = df_C1) +
  geom_point(size=2)



df_C2 <- tibble(k = c(15, 20, 25, 30, 35), 
               Accuracy = c(0.6651271, 0.6687362, 0.6699177, 0.6707770, 0.6708200), 
               calibration= c(0.9850043, 0.9976277, 1.0091871, 1.0185657, 1.0274575),
               logloss = c(0.6052762, 0.6029067, 0.6021384, 0.6023186, 0.6031220)) |>
  pivot_longer(!k, names_to = "metric", values_to = "value") |>
  mutate_at("metric", as.factor)
ggplot(aes(x = k, y = value, color = metric), data = df_C2) +
  geom_point(size=2) 




