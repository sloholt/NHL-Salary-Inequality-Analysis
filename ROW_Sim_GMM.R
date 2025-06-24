#Simulating ROW Data using a Generalized Method of Moments model 

library(dplyr)
library(gmm)

data <- read.csv("CompleteTeamData.csv")

simulate_row <- function(beta0, beta1, beta2, beta3){
  
  data <- data %>% arrange(Team, Year)
  data$ROW_sim <- NA 
  teams <- unique(data$Team)
  
  for (team in teams){ 
    team_data <- data %>% filter(Team == team) #all rows for current team 
    
    for (i in 1:nrow(team_data)){
      year <- team_data$Year[i] 
      
      if (year == min(team_data$Year)){ #keeping actual ROW for first year 
        data$ROW_sim[data$Team == team & data$Year == year] <- team_data$ROW[i]
      }else { 
        #For al other years, simulate the ROW with the previous years simulated ROW 
        prev_sim <- data$ROW_sim[data$Team == team & data$Year == (year - 1)]
        gini <- team_data$RawGini[i]
        gini2 <- team_data$Gini2[i]
        
        mu <- beta0 + beta1*gini + beta2*gini2 + beta3*prev_sim
        data$ROW_sim[data$Team == team & data$Year == year] <- mu
      }
    }
    
  }
  return(data)
}


one_ROW_sim_gmm <- function(data){
  #OLS on original data for starting values
  data <- data %>%
    arrange(Team, Year) %>%
    group_by(Team) %>%
    mutate(
      ROW_prev = lag(ROW),
      Gini2 = RawGini^2
    ) %>%
    ungroup()
  
  gmm_data <- data %>%
    select(ROW, RawGini, Gini2, ROW_prev) %>%
    na.omit()
  
  ols_fit <- lm(
    ROW ~ RawGini + Gini2 + ROW_prev,
    data = gmm_data
  )
  
  beta0 <- coef(ols_fit)[1]
  beta1 <- coef(ols_fit)[2]
  beta2 <- coef(ols_fit)[3]
  beta3 <- coef(ols_fit)[4]
  
  #Simulate new ROW with lagged regressors/instruments
  sim_data <- simulate_row(beta0, beta1, beta2, beta3)
  
  gmm_sim <- sim_data %>%
    arrange(Team, Year) %>%
    group_by(Team) %>%
    mutate(
      ROW_prev_sim = lag(ROW_sim),
      ROW_lag_sim = lag(ROW_sim, 2),
      Gini2 = RawGini^2
    ) %>%
    ungroup() %>%
    select(ROW_sim, RawGini, Gini2, ROW_prev_sim, ROW_lag_sim) %>%
    na.omit()
  
  #Simulated data for GMM setup
  y <- gmm_sim$ROW_sim
  x <- cbind(1, gmm_sim$RawGini, gmm_sim$Gini2, gmm_sim$ROW_prev_sim) #Regressors
  z <- cbind(1, gmm_sim$RawGini, gmm_sim$Gini2, gmm_sim$ROW_lag_sim) #Instruments
  
  gmm_moments <- function(theta, data){
    res <- as.numeric(y - x %*% theta)
    moments <- res * z
    return(moments) 
  }


  theta_start <- rep(0, 4)
  gmm_model<- gmm( g = gmm_moments, x = gmm_sim, t0 = theta_start)
  return(gmm_model)
}

sim_result <- one_ROW_sim_gmm(data)
summary(sim_result)



