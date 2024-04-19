

pacman::p_load(lubridate, tidyverse)

############ Glicko ############

g <- prob_table %>% select(-c(9:47)) 

# get all players in a table
player_name1 <- tibble(Name = "Last.F")
for (n in seq(1,nrow(g))) {
  Wname <- g[n,]$Winner
  Lname <- g[n,]$Loser
  if (!Wname %in% player_name1$Name ) {
    player_name1 <- rbind(player_name1, tibble(Name=Wname))
  }
  if (!Lname %in% player_name1$Name ) {
    player_name1 <- rbind(player_name1, tibble(Name=Lname))
  }
}


player_name <- player_name1 %>% filter(Name!="Last.F")
r <- 1500
RD <- 350
q <- 0.0057565
# 350 = sqrt(usual RD^2 + no.of ep becomes unreliable* c^2) solve for c
c <- sqrt((350^2 - 100^2)/(4*5))
player_name[,2] <- r
player_name[,3] <- RD

# split into rating period
# in the rating period, compute r and RD of each player
ep <- months(3)
No_ep <- interval( ymd("2001-01-01"), ymd("2024-02-25")) %/% ep + 1  # "2024-02-25"
start_time <- ymd("2001-01-01")
next_time <- add_with_rollback(start_time, ep)
acc_glicko <- numeric(No_ep)
# r, RD for all players for all rating periods
for (t in seq(1, No_ep)) {
  period_t <- g %>% arrange(Date) %>% filter(Date >= start_time & Date < next_time )
  start_time <- next_time
  next_time <- add_with_rollback(next_time, ep)

  player_name[,2*t+2] <- NA
  player_name[,2*t+3] <- NA
  counter <- 0 
  m <- 0
  for (i in player_name$Name) { # for each player
    player_i <- period_t %>% filter(Winner == i | Loser == i)
    player_name[,2*t+2][player_name["Name"]==i] <- player_name[,2*t][player_name["Name"]==i] 
    player_name[,2*t+3][player_name["Name"]==i] <- min(sqrt((player_name[,2*t+1][player_name["Name"]==i])^2 + c^2), 350)
    
    if (nrow(player_i)>=1) { # if there is a match for this player during this time
      col1 <- ifelse(player_i$Winner==i, 1, 0)
      player_i <- player_i %>% mutate(Winning = col1, rating = NA, deviation = NA)
      own_r <- player_name %>% filter(Name == i) %>% pull(2*t)
      own_RD <- player_name %>% filter(Name == i) %>% pull(2*t+1)
      own_RD <- min(sqrt(own_RD^2 + c^2), 350)
      for (j in seq(1,nrow(player_i)) ) { # get the info of all opponents

        if (player_i[j,]$Winning==1) {
          op <- player_i[j,]$Loser } 
        else {
          op <- player_i[j,]$Winner
        }
        op_r <- player_name %>% filter(Name == op) %>% pull(2*t)
        op_RD <- player_name %>% filter(Name == op) %>% pull(2*t+1)
        player_i[j,]["rating"] <- op_r
        player_i[j,]["deviation"] <- op_RD
      } # for j
      # update rating and RD
      player_i <- player_i %>% mutate(g_func = 1 / sqrt(1+ 3*q^2* deviation^2/ pi^2) )
      player_i <- player_i %>% mutate(Es = 1 / (1+10^( -g_func*(own_r-rating)/400 )) )
      
      d_square <- ( q^2*sum(player_i$g_func^2* player_i$Es* (1-player_i$Es)) )^(-1)
      new_r <- own_r+ 
        ( q/(1/own_RD^2 + 1/d_square) )* sum( player_i$g_func*(player_i$Winning-player_i$Es) )
      new_RD <- max(sqrt((1/own_RD^2 + 1/d_square)^(-1)), 30)
      
      player_name[,2*t+2][player_name["Name"]==i] <- new_r
      player_name[,2*t+3][player_name["Name"]==i] <- new_RD
      
      # compute the winner won prob
      player_i <- player_i %>% mutate(i_rating = own_r) %>% 
        mutate(winner_r  = i_rating *Winning + rating *(1-Winning)) %>% 
        mutate(loser_r = i_rating *(1-Winning) + rating * Winning)
      player_i <- player_i %>% 
        mutate(prob_winner_win = 1 / (1+10^( -g_func*(winner_r-loser_r)/400 )))
      counter <- counter + 1
      m_i <- mean(ifelse(player_i$prob_winner_win >= 0.5, 1, 0) == 1)
      m <- m + m_i
      
    } # if 
 
  } # for i
  acc_glicko[t] <- m/counter
  print(m/counter)
  print(t)
  
}  # for t

mean(acc_glicko, na.rm = TRUE)


player_name %>% view()

# 8:23 start 
# 5 years 50tp 20 minimum, 
# 1month   0.6454589
# 2months  0.6570936
# 3months  0.652778  100tp 0.6534511      
# 4months  0.6579653
# 6months  0.6541248          
# 12months 0.659856




RD <- tibble(month = c(1, 2, 3, 4, 6, 12), 
              Accuracy = c(0.6454589, 0.6570936, 0.652778, 0.6579653, 0.6541248, 0.659856) ) %>% 
  mutate_at("month", as.factor)

ggplot(aes(x = month, y = Accuracy), data = RD) +
  geom_point(size=2)





