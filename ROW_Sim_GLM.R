#Simulating ROW Data using a Poisson Distribution and Log-Link GLM 

library(readxl)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(tibble)
library(tidyr)


data <- read.csv("CompleteTeamData.csv")

#Lagged ROW column 
data <- data %>% 
  arrange(Team, Year) %>%
  group_by(Team) %>%
  mutate(ROW_prev_actual = lag(ROW)) %>%
  ungroup()
data$ROW_prev_actual

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
    ROW ~ poly(RawGini,2) + ROW_prev_actual,
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
    ROW_sim ~ poly(RawGini,2) + ROW_prev_actual,
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

sd(data$RawGini)

#Curve based on specific coeff to visualize effect of Gini on expected ROW
curve(exp(1.57 + 7.8*x -9.85*x^2+0.01*35), from=c(0.3), to=c(0.6))


#100 Replications of simulation
set.seed(123)
rep_results <- replicate(100, 
                         coef(one_ROW_sim(data)),
                         simplify = "matrix")
rep_results_df_uncentered <- as.data.frame(t(rep_results))
colnames(rep_results_df_uncentered) <- c("Intercept", "RawGini_UnCentered", "Gini2_UnCentered", "LagROW_UnCentered")

View(rep_results_df_uncentered)

dim(rep_results_df_uncentered)

# Boxplots
ggplot(rep_results_df_uncentered, aes(y = Intercept)) + geom_boxplot()
ggplot(rep_results_df_uncentered, aes(y = RawGini_UnCentered)) + geom_boxplot()
ggplot(rep_results_df_uncentered, aes(y = Gini2_UnCentered)) + geom_boxplot()
ggplot(rep_results_df_uncentered, aes(y = LagROW_UnCentered)) + geom_boxplot()

# Histograms
ggplot(rep_results_df_uncentered, aes(x = Intercept)) + geom_histogram(binwidth = 0.5)
ggplot(rep_results_df_uncentered, aes(x = RawGini_UnCentered)) + geom_histogram(binwidth = 1)
ggplot(rep_results_df_uncentered, aes(x = Gini2_UnCentered)) + geom_histogram(binwidth = 1)
ggplot(rep_results_df_uncentered, aes(x = LagROW_UnCentered)) + geom_histogram(binwidth = 0.0005)


summary_stats <- rep_results_df_uncentered %>%
  summarise(across(everything(), list(mean = mean, sd = sd, IQR = IQR)))
summary_stats


#Optimal Gini for Simulation Results 
#Optimal Gini = -B1/2B2
rep_results_df_uncentered$opt_gini <- rep_results_df_uncentered$RawGini_UnCentered /
  (2* rep_results_df_uncentered$Gini2_UnCentered)
summary(rep_results_df_uncentered$opt_gini)
hist(rep_results_df_uncentered$opt_gini, breaks = 20, main = "Optimal Gini", xlab = "Gini")

View(rep_results_df_uncentered)

#COMPARING QUANTILES: 

gini_quantiles <- quantile(data$RawGini, c(0.25, 0.75), na.rm=TRUE)
gini_25 <- gini_quantiles[1]
gini_75 <- gini_quantiles[2]

row_quantiles <- quantile(data$ROW_prev_actual, c(0.25, 0.5, 0.75), na.rm=TRUE)
row_25 <- row_quantiles[1]
row_50 <- row_quantiles[2]
row_75 <- row_quantiles[3]

gini_vals <- c(gini_25, gini_75)
row_vals <- c(row_25, row_50, row_75)
prediction_matrix <- expand.grid(
  RawGini = gini_vals,
  ROW_prev_actual = row_vals
)

#Predictions for the real glm model
real_glm <- glm(
  ROW ~ poly(RawGini, 2) + ROW_prev_actual,
  family = poisson(link = "log"),
  data = data
)
prediction_matrix$real_glm_pred_response <- predict(
  real_glm,
  newdata = prediction_matrix,
  type = "response"
)

sim_glm <- my_model  # already computed from one_ROW_sim()
prediction_matrix$sim_glm_pred_response <- predict(
  sim_glm,
  newdata = prediction_matrix,
  type = "response"
)
prediction_matrix$Scenario <- factor(seq_len(nrow(prediction_matrix)))

#Plotting predicted values: 
#Real GLM predictions:
ggplot(prediction_matrix, aes(x = Scenario, y = real_glm_pred_response)) +
  geom_col(fill = "steelblue") +
  labs(title = "Predicted ROW from Real GLM", y = "Predicted ROW") +
  theme_minimal()

#Simulated GLM predictions:
ggplot(prediction_matrix, aes(x = Scenario, y = sim_glm_pred_response)) +
  geom_col(fill = "darkorange") +
  labs(title = "Predicted ROW from Simulated GLM", y = "Predicted ROW") +
  theme_minimal()


