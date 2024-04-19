
# import the package
pacman::p_load(tidyverse, lubridate, patchwork)

# one predictor without intercept
theta_vals <- seq(0.005, 0.02, by = 0.001)
D <- seq(-500, 500, by = 1)
logistic <- function(x, theta){
  probs <- 1 / (1 + exp(-theta * x))
  return(probs)
}
theta <- c()
probs <- c()
D_vec <- c()
for (t in theta_vals){
  probs <- c(probs, logistic(D, t))
  theta <- c(theta, rep(t, length(D)))
  D_vec <- c(D_vec, D)
}

probs_df <- tibble(p = probs, theta = theta, D = D_vec) |>
  mutate(theta = as.factor(theta))
ggplot(aes(x = D, y = p, color = theta), data = probs_df) +
  geom_line() +
  theme_bw()



files <- str_glue("../tennis_atp/atp_matches_{1968:2023}.csv")
# concatenate each csv file into a big table.
raw_matches <- files %>%
  map_dfr(function(x) read_csv(x,show_col_types = FALSE))

### pre-processing ###
matches_df <- raw_matches %>% 
  select(c(
    "tourney_date",
    "tourney_name",
    "surface",
    "draw_size",
    "tourney_level",
    "match_num",
    "winner_id",
    "loser_id",
    "best_of",
    "winner_rank",
    "winner_rank_points",
    "loser_rank",
    "loser_rank_points")) %>% 
  mutate_at(
    c("tourney_name", "surface","tourney_level", "best_of"),
    as.factor) %>% 
  mutate_at(c("winner_id", "loser_id"), as.integer) %>% 
  mutate(tourney_date = ymd(tourney_date))

matches_df <- matches_df %>% 
  mutate(loser_rank = replace_na(loser_rank, 100000)) %>% 
  mutate(winner_rank = replace_na(winner_rank, 100000))

matches_df <- matches_df %>% 
  na.omit() %>% 
  mutate(higher_rank_won = winner_rank < loser_rank) %>% 
  mutate(higher_rank_points = winner_rank_points * (higher_rank_won) +
           loser_rank_points * (1 - higher_rank_won)) %>% 
  mutate(lower_rank_points = winner_rank_points * (1 - higher_rank_won) +
           loser_rank_points * (higher_rank_won))

matches_df <- matches_df %>% 
  mutate(diff = higher_rank_points - lower_rank_points)

### EDA
# 
matches_df %>% ggplot(aes(best_of, fill=higher_rank_won))  +
  geom_bar()

matches_df %>% ggplot(aes(surface, fill=higher_rank_won)) +
  geom_bar()

matches_df %>% ggplot(aes(diff, fill=higher_rank_won)) +
  geom_histogram(bins = 40) 

matches_df %>% ggplot(aes(tourney_level, fill=higher_rank_won)) +
  geom_bar()

#
matches_df %>% ggplot() +
  geom_mosaic(aes(x = product(tourney_level), fill = higher_rank_won)) + 
  facet_wrap(~best_of)
# tourney_level & best_of correlated, Grand slam best of 5 only
# best of 3 has a higher percentage in lower_rank_won


# Logistic model
### split the data ###

# 1968-2018, 2019-23
train0009 <- matches_df %>% filter(tourney_date <= "2018-12-31" )
test2010 <- matches_df %>% filter(tourney_date > "2018-12-31")


### fit the model ###

fit_diff <- glm(
  higher_rank_won ~ diff + 0,
  data = train0009,
  family = binomial(link = 'logit')
)
summary(fit_diff)

W <- train0009$higher_rank_won
W_test <- test2010$higher_rank_won

# ~~~~~~~~~~~~~~~~ naive model ~~~~~~~~~~~~~~~~~~~
#accuracy
naive_accuracy <- mean(W_test)
mean(W_test)
# testing
ll_naive <- -1 / nrow(test2010) * sum(W_test * log(mean(W_test)) +
                                 (1 - W_test) * log(1 - mean(W_test)))
c_naive <- mean(W_test) * nrow(test2010) / sum(W_test)

valid_stats <- tibble(model = "naive", pred_acc = naive_accuracy,
                           log_loss = ll_naive, cali = c_naive)
valid_stats
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p <- 0.5
pred_1 <- predict(fit_diff, train0009, type = "response")
mean(ifelse(pred_1 > p, 1, 0) == W)

test_pred1 <- predict(fit_diff, test2010, type = "response")
mean(ifelse(test_pred1 > p, 1, 0) == W_test)


## multiple predictors & accuracy ##

fit_2 <- glm( higher_rank_won ~ diff + surface,
              data = train0009,
              family = binomial(link = 'logit')
)
summary(fit_2)

pred_2 <- predict(fit_2, train0009, type = "response")
mean(ifelse(pred_2 > p, 1, 0) == W)

test_pred2 <- predict(fit_2, test2010, type = "response")
mean(ifelse(test_pred2 > p, 1, 0) == W_test)


fit_3 <- glm( higher_rank_won ~ diff + surface + tourney_level,
              data = train0009,
              family = binomial(link = 'logit'))

summary(fit_3)



test_pred3 <- predict(fit_3, test2010, type = "response")
mean(ifelse(test_pred3 > p, 1, 0) == W_test)


### Calibration C ###

#training
C_1 <- sum(pred_1) / sum(W)
C_2 <- sum(pred_2) / sum(W)
C_3 <- sum(pred_3) / sum(W)
# testing
Ct_1 <- sum(test_pred1) /sum(W_test)
Ct_2 <- sum(test_pred2) /sum(W_test)
Ct_3 <- sum(test_pred3) /sum(W_test)
c(C_1, C_2, C_3)
c(Ct_1, Ct_2, Ct_3)

### log-loss ###

# training 
log_loss1 <- -1 / nrow(train0009) * sum(W * log(pred_1) +
                                        (1 - W) * log(1 - pred_1), na.rm = T)

log_loss2 <- -1 / nrow(train0009) * sum(W * log(pred_2) +
                                        (1 - W) * log(1 - pred_2), na.rm = T)

log_loss3 <- -1 / nrow(train0009) * sum(W * log(pred_3) +
                                        (1 - W) * log(1 - pred_3), na.rm = T)
# testing 
logt1 <- -1 / nrow(test2010) * sum(W_test * log(test_pred1) +
                                  (1 - W_test) * log(1 - test_pred1), na.rm = T)

logt2 <- -1 / nrow(test2010) * sum(W_test * log(test_pred2) +
                                  (1 - W_test) * log(1 - test_pred2), na.rm = T)

logt3 <- -1 / nrow(test2010) * sum(W_test * log(test_pred3) +
                                  (1 - W_test) * log(1 - test_pred3), na.rm = T)
c(log_loss1, log_loss2, log_loss3)
c(logt1, logt2, logt3)



### ELO ###

matches_wide <- matches_df %>% mutate(winner = winner_id, loser = loser_id)
matches_wide <- matches_wide %>% 
  pivot_longer(cols = winner:loser, names_to = "result", values_to = "player") %>% 
  arrange(tourney_date)
atp_players <- read_csv("atp_players.csv")

### with constant K ###
atp_players <- atp_players %>% select(-c(dob, wikidata_id, height)) %>% 
  mutate(elo_score = 1500)
scores <- numeric(nrow(matches_wide))
for ( n in seq(1,nrow(matches_wide),2) ) {
  # set the value for k-factor
  k <- 25
  id_n <- matches_wide[n,]$player
  id_op <- matches_wide[n+1,]$player
  # pull the elo scores of both players
  score_n <- atp_players %>% filter(player_id == id_n) %>% pull(elo_score)
  score_op <- atp_players %>% filter(player_id == id_op) %>% pull(elo_score)
  
  scores[n] <- score_n
  scores[n+1] <- score_op
  # prob of the first player wins
  prob_1win <- ( 1+10^( (score_op-score_n)/400 ) )^(-1)
  # update the scores of both players accordingly
  score_n <- score_n + k*(1-prob_1win)
  score_op <- score_op + k*(-1+prob_1win)
  
  # update the atp list
  atp_players["elo_score"][atp_players["player_id"] == id_n] <- score_n
  atp_players["elo_score"][atp_players["player_id"] == id_op] <- score_op
  # update the df

  if ((n+1) %% 5000 == 0) {print(n+1)}

}
matches_wide <- matches_wide %>% mutate(elo_rating = scores)



### FiveThirtyEight ###

delta <- 100
nu <- 5
sigma <- 0.1

atp_players <- atp_players %>% mutate(mt = 0, elo_FiveTE = 1500)
score_FTE <- numeric(nrow(matches_wide))
for ( n in seq(1,nrow(matches_wide),2) ) {
  # get the IDs
  id_n <- matches_wide[n,]$player
  id_op <- matches_wide[n+1,]$player  
  # pull the elo scores of both players
  score_n <- atp_players %>% filter(player_id == id_n) %>% pull(elo_FiveTE)
  score_op <- atp_players %>% filter(player_id == id_op) %>% pull(elo_FiveTE)
  # pull mt for both players
  mt_n <- atp_players %>% filter(player_id == id_n) %>% pull(mt)
  mt_op <- atp_players %>% filter(player_id == id_op) %>% pull(mt)
  # update the df
  score_FTE[n] <- score_n
  score_FTE[n+1] <- score_op
  
  # compute Ks using FTE
  k_n <- delta/(mt_n+nu)^sigma
  k_op <- delta/(mt_op+nu)^sigma
  # prob of the first player wins
  prob_1win <- ( 1+10^( (score_op-score_n)/400 ) )^(-1)   
  # update the scores of both players accordingly
  score_n <- score_n + k_n*(1-prob_1win)
  score_op <- score_op + k_op*(-1+prob_1win)
  
  # update the atp list, scores and mt
  atp_players["elo_FiveTE"][atp_players["player_id"] == id_n] <- score_n
  atp_players["elo_FiveTE"][atp_players["player_id"] == id_op] <- score_op
  atp_players["mt"][atp_players["player_id"] == id_n] <- mt_n + 1
  atp_players["mt"][atp_players["player_id"] == id_op] <- mt_op + 1 

  if ((n+1) %% 10000 == 0) {print(n+1)}
}
matches_wide <- matches_wide %>% mutate(elo_FTE = score_FTE)


### plots ###

matches_wide %>% 
  ggplot(aes(x=tourney_date, y=elo_FTE, colour=as.factor(player))) + 
  geom_line(show.legend = FALSE)

# top players
tops <- atp_players %>% 
  arrange(desc(elo_score)) %>% filter(elo_score>1900) %>% 
  arrange(player_id) %>% pull(player_id)

name <- numeric(length(tops))
j <- 1
for (i in tops) {
  name[j] <- atp_players["name_last"][atp_players["player_id"] == i]
  j <- j+1
  print(i)
  print(name[j-1])
}

matches_wide %>% filter(player %in% tops) %>% 
  mutate(player=as.factor(player)) %>% 
  ggplot(aes(x=tourney_date, y=elo_FTE, colour=player)) + 
  geom_line(size=1) +
  labs(x = "year",
       y = "Elo score") + 
  scale_colour_discrete(labels = name)


##################### Long form of computing ELO ###################
matches_test <- matches_df %>% mutate(winner = winner_id, loser = loser_id) %>% 
  arrange(tourney_date) %>% filter(tourney_date <= "2018-12-31")
atp_players <- read_csv("atp_players.csv")
### with constant K ### 
# set the value for k-factor
k <- 25
atp_players <- atp_players %>% select(-c(dob, wikidata_id, height)) %>% 
  mutate(elo_score = 1500)
C_scoreW <- numeric(nrow(matches_test))
C_scoreL <- numeric(nrow(matches_test))
for ( n in seq(1,nrow(matches_test)) ) {
  id_n <- matches_test[n,]$winner
  id_op <- matches_test[n,]$loser
  # pull the elo scores of both players
  score_n <- atp_players %>% filter(player_id == id_n) %>% pull(elo_score)
  score_op <- atp_players %>% filter(player_id == id_op) %>% pull(elo_score)
    # update the df
  C_scoreW[n] <- score_n
  C_scoreL[n] <- score_op
  
  # prob of the first player wins
  prob_1win <- ( 1+10^( (score_op-score_n)/400 ) )^(-1)
  # update the scores of both players accordingly
  score_n <- score_n + k*(1-prob_1win)
  score_op <- score_op + k*(-1+prob_1win)
  
  # update the atp list
  atp_players["elo_score"][atp_players["player_id"] == id_n] <- score_n
  atp_players["elo_score"][atp_players["player_id"] == id_op] <- score_op


  if ((n+1) %% 5000 == 0) {print(n+1)}
  
}
matches_test <- matches_test %>% mutate(elo_rating_winner=C_scoreW, elo_rating_loser=C_scoreL)
matches_test <- matches_test %>%
  mutate(higher_score_won = elo_rating_winner > elo_rating_loser) %>%
  mutate(higher_elo = elo_rating_winner * (higher_score_won) +
           elo_rating_loser * (1 - higher_score_won)) %>%
  mutate(lower_elo = elo_rating_winner * (1 - higher_score_won) +
           elo_rating_loser * (higher_score_won))

matches_test <- matches_test %>% mutate( diff_test = higher_elo - lower_elo )
matches_test <- matches_test %>% mutate( pred_elo = 1 / (1 + 10^(-diff/400)) )

# Accuracy
mean(ifelse(matches_test$pred_elo > 0.5, 1, 0) == matches_test$higher_score_won)
################################################################################



### Validation of the results ###

# assign a unique id to every match
matches_wide <- matches_wide %>% 
  mutate(matches_no=sort(rep(seq(1:nrow(matches_df)), 2)))
# put the ELO scores back into the wider df
b <- matches_wide %>% 
  pivot_wider(id_cols = matches_no, names_from = result, values_from = elo_rating:elo_FTE)
matches_df <- matches_df %>% arrange(tourney_date) %>% cbind(b)

# new df for elo ratings
df_elo <- matches_df %>% select(tourney_date:best_of,elo_rating_winner:elo_FTE_loser)
# constant K
df_elo <- df_elo %>%
  mutate(higher_C_won = elo_rating_winner > elo_rating_loser) %>%
  mutate(higher_C_elo = elo_rating_winner * (higher_C_won) +
           elo_rating_loser * (1 - higher_C_won)) %>%
  mutate(lower_C_elo = elo_rating_winner * (1 - higher_C_won) +
           elo_rating_loser * (higher_C_won))
# FTE
df_elo <- df_elo %>%
  mutate(higher_score_won = elo_FTE_winner > elo_FTE_loser) %>%
  mutate(higher_elo = elo_FTE_winner * (higher_score_won) +
           elo_FTE_loser * (1 - higher_score_won)) %>%
  mutate(lower_elo = elo_FTE_winner * (1 - higher_score_won) +
           elo_FTE_loser * (higher_score_won))

df_elo <- df_elo %>% 
  mutate(diff_C = higher_C_elo - lower_C_elo) %>% 
  mutate(diff = higher_elo - lower_elo)

# Use only the difference in elo scores to predict the outcome
df_elo <- df_elo %>% mutate( pred_C_elo = 1 / (1 + 10^(-diff_C/400)) )
df_elo <- df_elo %>% mutate( pred_FTE_elo = 1 / (1 + 10^(-diff/400)) )

### split the data ###
# -2018, 2019-
elo_train <- df_elo %>% filter(tourney_date <= "2018-12-31" )
elo_test <- df_elo %>% filter(tourney_date > "2018-12-31")

W_C_elo <- elo_train$higher_C_won
Wt_C_elo <- elo_test$higher_C_won
W_elo <- elo_train$higher_score_won
Wt_elo <- elo_test$higher_score_won

# Accuracy
mean(ifelse(elo_train$pred_C_elo > 0.5, 1, 0) == W_C_elo)
mean(ifelse(elo_train$pred_FTE_elo > 0.5, 1, 0) == W_elo)

# Calibration C
sum(elo_train$pred_C_elo) /sum(W_C_elo)
C_elo_train <- sum(elo_train$pred_FTE_elo) /sum(W_elo)
C_elo_train

# log-loss
ntrain <- nrow(elo_train)
ntest <- nrow(elo_test)

ll_C_train <- -1 / ntrain * sum(W_C_elo * log(elo_train$pred_C_elo) +
                               (1 - W_C_elo) * log(1 - elo_train$pred_C_elo))
ll_train <- -1 / ntrain * sum(W_elo * log(elo_train$pred_FTE_elo) +
                             (1 - W_elo) * log(1 - elo_train$pred_FTE_elo))
ll_C_train
ll_train


# Accuracy
mean(ifelse(elo_test$pred_C_elo > 0.5, 1, 0) == Wt_C_elo)
mean(ifelse(elo_test$pred_FTE_elo > 0.5, 1, 0) == Wt_elo)

# Calibration C
sum(elo_test$pred_C_elo) /sum(Wt_C_elo)
C_elo <- sum(elo_test$pred_FTE_elo) /sum(Wt_elo)
C_elo

# log-loss
ll_C_elo <- -1 / ntest * sum(Wt_C_elo * log(elo_test$pred_C_elo) +
                             (1 - Wt_C_elo) * log(1 - elo_test$pred_C_elo))
ll_elo <- -1 / ntest * sum(Wt_elo * log(elo_test$pred_FTE_elo) +
                          (1 - Wt_elo) * log(1 - elo_test$pred_FTE_elo))
ll_C_elo
ll_elo




############### plot K 538 ############

m <- seq(1, 600, by = 1)
K_FTE1 <- 200 / (m + 10)^0.1
K_FTE2 <- 200 / (m + 100)^0.1
K_FTE3 <- 200 / (m + 10)^0.5
K_FTE4 <- 100 / (m + 10)^0.1
df <- tibble(m = m, K1 = K_FTE1, K2 = K_FTE2, K3 = K_FTE3, K4 = K_FTE4) |>
  pivot_longer(!m, names_to = "model", values_to = "K") |>
  mutate_at("model", as.factor)
ggplot(aes(x = m, y = K, color = model), data = df) +
  geom_line(linewidth=1.2) + 
  scale_color_discrete("(delta, nu, sigma)", labels=c("(200,10,0.1)", "(200,100,0.1)", 
                                     "(200,10,0.5)", "(100,10,0.1)"))












