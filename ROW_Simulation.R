#Simulating ROW Data using a Poisson Distribution and Log-Link GLM 

library(readxl)
library(dplyr)

data <- read.csv("CompleteTeamData.csv")

#Lagged ROW column 
data <- data %>% 
  arrange(Team, Year) %>%
  group_by(Team) %>%
  mutate(ROW_prev_actual = lag(ROW)) %>%
  ungroup()
#View(data)

##################################################
#UPDATED SIMULATION WITH NEW POISSON MODEL 
#################################################

simulate_row <- function(beta0, beta1, beta2, beta3){
  mean_gini <- mean(data$RawGini, na.rm = TRUE)
  mean_row <- mean(data$ROW_prev_actual, na.rm = TRUE)
  mean_gini2 <- mean(data$RawGini^2, na.rm = TRUE)
  
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
        gini2 <- gini^2
        
        log_mu <- beta0 + 
          beta1 * (gini - mean_gini) + 
          beta2 * (gini2 - mean_gini2) +
          beta3 * (prev_sim - mean_row)
        mu <- exp(log_mu)
        
        sim_row <- rpois(1, lambda = mu)
        data$ROW_sim[data$Team == team & data$Year == year] <- sim_row
      }
    }
    
  }
  return(data)
}


#Our version of one_VAR_sim to simulate one 20yr dataset 
#& estimate the coefficients with GLM
one_ROW_sim <- function(data){
  mean_gini <- mean(data$RawGini, na.rm = TRUE)
  mean_row <- mean(data$ROW_prev_actual, na.rm = TRUE)
  mean_gini2 <- mean(data$RawGini^2, na.rm = TRUE)
  
  #GLM on actual data for starting coeffs
  glm_fit <- glm(
    ROW ~ I(RawGini - mean_gini) +
      I(RawGini^2 - mean_gini2) +
      I(ROW_prev_actual - mean_row),
    family = poisson(link = "log"),
    data = data
  )
  

  beta0 <- coef(glm_fit)[1]
  beta1 <- coef(glm_fit)[2]
  beta2 <- coef(glm_fit)[3]
  beta3 <- coef(glm_fit)[4]
  
  #Sim new ROW values
  sim_data <- simulate_row(beta0, beta1, beta2, beta3)
  
  #GLM on simulated data 
  sim_glm <- glm(
    ROW_sim ~ I(RawGini - mean_gini) + 
      I(Gini2 - mean_gini2) + 
      I(ROW_prev_actual - mean_row), 
    family = poisson(link = "log"),
    data = sim_data
  )
  
  return(coef(sim_glm))
}


#100 Replications
set.seed(2025)
rep_results <- replicate(100, one_ROW_sim(data), 
                         simplify = "matrix")
rep_results_df <- as.data.frame(t(rep_results))
colnames(rep_results_df) <- c("Intercept", "RawGini_Centered", "Gini^2_Centered", "LagROW_Centered")

View(rep_results_df)





