#Simulating ROW Data using a Poisson Distribution and Log-Link GLM 

library(readxl)
library(dplyr)

complete_data <- read_excel("PerformanceTeamData.xlsx")
gini_data <- read_excel("teamGinis.xlsx")

colnames(complete_data)
colnames(gini_data)

merged_data <- inner_join(complete_data, gini_data, by = c("Team", "Year"))
#head(merged_data)

#View(merged_data)

#Average Raw Gini
mean_gini <- mean(merged_data$RawGini, na.rm = TRUE) 
mean_gini

#Lagged ROW column 
merged_data <- merged_data %>% arrange(Team, Year)
merged_data <- merged_data %>%
  group_by(Team) %>%
  mutate(ROW_prev_actual = lag(ROW)) %>%
  ungroup()
#View(merged_data)

#Average ROW (lagged)
mean_row <- mean(merged_data$ROW_prev_actual, na.rm = TRUE)
mean_row

#Calculating the ROW simulations using u_i 
simulate_row <- function(beta0, beta1, beta2){
  mean_gini <- mean(merged_data$RawGini, na.rm = TRUE)
  mean_row <- mean(merged_data$ROW_prev_actual, na.rm = TRUE)
  
  data <- merged_data %>% arrange(Team, Year)
  data$ROW_sim <- NA 
  
  teams <- unique(merged_data$Team) 
  
  for (team in teams){ 
    team_data <- merged_data %>% filter(Team == team) #all rows for current team 
    
    for (i in 1:nrow(team_data)){
      year <- team_data$Year[i] 
      
      if (year == min(team_data$Year)){ #keeping actual ROW for first year 
        merged_data$ROW_sim[merged_data$Team == team & merged_data$Year == year] <- team_data$ROW[i]
      }else { 
        #For al other years, simulate the ROW with the previous years simulated ROW 
        prev_sim <- merged_data$ROW_sim[merged_data$Team == team & merged_data$Year == (year - 1)]
        gini <- team_data$RawGini[i]
        
        log_mu <- beta0 + beta1 * (gini - mean_gini) + beta2 * (prev_sim - mean_row)
        mu <- exp(log_mu)
        
        sim_row <- rpois(1, lambda = mu)
        merged_data$ROW_sim[merged_data$Team == team & merged_data$Year == year] <- sim_row
        }
      }
    
  }
  return(merged_data)
}

glm_model <- glm(
  ROW ~ I(RawGini - mean(merged_data$RawGini, na.rm = TRUE)) +
    I(ROW_prev_actual - mean(merged_data$ROW_prev_actual, na.rm = TRUE)),
  family = poisson(link = 'log'),
  data = merged_data
)

#summary(glm_model)

beta0 <- coef(glm_model)[1]
beta1 <- coef(glm_model)[2]
beta2 <- coef(glm_model)[3]

simulated_data <- simulate_row(beta0, beta1, beta2)
View(simulated_data) 

write.csv(simulated_data, "simulated_data.csv", row.names = FALSE) 
write.csv(merged_data, "CompleteTeamData.csv", row.names = FALSE)






























