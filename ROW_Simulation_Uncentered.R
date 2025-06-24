#Simulating ROW Data using a Poisson Distribution and Log-Link GLM 

library(readxl)
library(dplyr)
library(ggplot2)
library(ggfortify)

data <- read.csv("CompleteTeamData.csv")

#Lagged ROW column 
data <- data %>% 
  arrange(Team, Year) %>%
  group_by(Team) %>%
  mutate(ROW_prev_actual = lag(ROW)) %>%
  ungroup()

#######################################################
#UPDATED SIMULATION WITH POISSON MODEL & UNCENTERED
######################################################

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
        gini2 <- gini^2
        
        log_mu <- beta0 + 
          beta1 * (gini) + 
          beta2 * (gini2) +
          beta3 * (prev_sim)
        mu <- exp(log_mu)
        
        sim_row <- rpois(1, lambda = mu)
        data$ROW_sim[data$Team == team & data$Year == year] <- sim_row
      }
    }
    
  }
  return(data)
}


#Our version of one_VAR_sim to simulate one 20yr dataset & estimate the coefficients with GLM
one_ROW_sim <- function(data){
  
  #GLM on actual data for starting coeffs
  glm_fit <- glm(
    ROW ~ RawGini +
      Gini2 +
      ROW_prev_actual,
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
    ROW_sim ~ RawGini + 
      Gini2 + 
      ROW_prev_actual, 
    family = poisson(link = "log"),
    data = sim_data
  )
  
  #return(autoplot(glm_fit))
  #return(coef(glm_fit))
  return(sim_glm)
}

#Running one simulation & visualizing 
my_model<- one_ROW_sim(data)
summary(my_model)
autoplot(my_model)
plot(fitted(my_model), my_model$y)

#Curve based on specific coeff to visualize effect of Gini on expected ROW
curve(exp(1.57 + 7.8*x -9.85*x^2+0.01*35), from=c(0.3), to=c(0.6))


#100 Replications of simulation
set.seed(2025)
rep_results <- replicate(100, one_ROW_sim(data), 
                         simplify = "matrix")
rep_results_df_uncentered <- as.data.frame(t(rep_results))
colnames(rep_results_df_uncentered) <- c("Intercept", "RawGini_UnCentered", "Gini2_UnCentered", "LagROW_UnCentered")

View(rep_results_df_uncentered)


ggplot(rep_results_df_uncentered, aes(y=LagROW_Centered)) + geom_boxplot()

ggplot(rep_results_df_uncentered, aes(x = LagROW_Centered)) +
  geom_histogram(binwidth = 0.1)

ggplot(rep_results_df_uncentered, aes(y=RawGini_Centered)) + geom_boxplot()

ggplot(rep_results_df_uncentered, aes(x=RawGini_Centered)) + geom_histogram()

ggplot(rep_results_df_uncentered, aes(y=`Gini2_Centered`)) + geom_boxplot()

ggplot(rep_results_df_uncentered, aes(x=`Gini2_Centered`)) + geom_histogram()

ggplot(rep_results_df_uncentered, aes(y=Intercept)) + geom_boxplot()

ggplot(rep_results_df_uncentered, aes(x=Intercept)) + geom_histogram()





