#Simulating ROW Data using a Poisson Distribution and Log-Link GLM 

library(readxl)
library(dplyr)
library(ggplot2)

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

mean_gini <- mean(data$RawGini, na.rm = TRUE)
print(mean_gini)

simulate_row <- function(beta0, beta1, beta2, beta3){
  mean_gini <- mean(data$RawGini, na.rm = TRUE)
  mean_row <- mean(data$ROW_prev_actual, na.rm = TRUE)
  mean_gini2 <- mean(data$Gini2, na.rm = TRUE)
  
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
#sim_1_row <- as.data.frame(t(simulate_row(3.6272137908, 0.7185139036, -1.8235626858, 0.0009172417)))
#View(sim_1_row)

#Our version of one_VAR_sim to simulate one 20yr dataset 
#& estimate the coefficients with GLM
one_ROW_sim <- function(data){
  mean_gini <- mean(data$RawGini, na.rm = TRUE)
  mean_row <- mean(data$ROW_prev_actual, na.rm = TRUE)
  mean_gini2 <- mean(data$Gini2, na.rm = TRUE)
  
  #GLM on actual data for starting coeffs
  glm_fit <- glm(
    ROW ~ I(RawGini - mean_gini) +
      #I(Gini2 - mean_gini2) +
      I(ROW_prev_actual - mean_row),
    family = poisson(link = "log"),
    data = data
  )
  print(summary(glm_fit))

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
  
  return(coef(glm_fit))
  #return(coef(sim_glm))
}
one_ROW_sim(data)


#100 Replications
set.seed(2025)
rep_results <- replicate(100, one_ROW_sim(data), 
                         simplify = "matrix")
rep_results_df_centered <- as.data.frame(t(rep_results))
colnames(rep_results_df_centered) <- c("Intercept", "RawGini_Centered", "Gini2_Centered", "LagROW_Centered")

View(rep_results_df_centered)


ggplot(rep_results_df_centered, aes(y=LagROW_Centered)) + geom_boxplot()

ggplot(rep_results_df_centered, aes(x=LagROW_Centered)) + geom_histogram()

ggplot(rep_results_df_centered, aes(y=RawGini_Centered)) + geom_boxplot()

ggplot(rep_results_df_centered, aes(x=RawGini_Centered)) + geom_histogram()

ggplot(rep_results_df_centered, aes(y=Gini2_Centered)) + geom_boxplot()

ggplot(rep_results_df_centered, aes(x=Gini2_Centered)) + geom_histogram()

ggplot(rep_results_df_centered, aes(y=Intercept)) + geom_boxplot()

ggplot(rep_results_df_centered, aes(x=Intercept)) + geom_histogram()



##########################
#Comparing GLM Model Coefs 
#########################

#No Gini, No Gini2: 
mean_row <- mean(data$ROW_prev_actual, na.rm = TRUE)
glm_row_only <- glm(
  ROW ~ I(ROW_prev_actual - mean_row),
  family = poisson(link = "log"),
  data = data
)
coef(glm_row_only)

#Gini, No Gini2 (Linear): 
mean_gini <- mean(data$RawGini, na.rm = TRUE)
glm_row_gini <- glm(
  ROW ~ I(RawGini - mean_gini) + I(ROW_prev_actual - mean_row),
  family = poisson(link = "log"),
  data = data
)
coef(glm_row_gini)

############################################################
## Durbin–Wu–Hausman endogeneity test
############################################################
library(AER)    
library(dplyr)
library(lmtest)


# centred regressors 
data <- data |>
  mutate(
    RawGini_c = RawGini - mean(RawGini, na.rm = TRUE),
    Gini2_c   = RawGini^2 - mean(RawGini^2, na.rm = TRUE),
    LagROW_c  = ROW_prev_actual - mean(ROW_prev_actual, na.rm = TRUE)
  )

#season-lag instruments for testing each endogeneity
data <- data |>
  arrange(Team, Year) |>
  group_by(Team) |>
  mutate(
    RawGini_lag2   = lag(RawGini, 2),
    Gini2_lag2     = lag(RawGini^2, 2),
    ROW_prev_lag2  = lag(ROW_prev_actual, 2)
  ) |>
  ungroup()

test_dat <- na.omit(data[, c("ROW", "RawGini_c", "Gini2_c", "LagROW_c",
                             "RawGini_lag2", "Gini2_lag2", "ROW_prev_lag2")])
#OLS 
ols_mod <- lm(ROW ~ RawGini_c + Gini2_c + LagROW_c, data = test_dat)

#Instrumental-Variables regression 
iv_mod  <- ivreg(
  ROW ~ RawGini_c + Gini2_c + LagROW_c |
    RawGini_lag2 + Gini2_lag2 + ROW_prev_lag2,
  data = test_dat
)
summary(iv_mod, diagnostics = TRUE)


#Durbin–Wu–Hausman test
b_ols <- coef(ols_mod)
b_iv <- coef(iv_mod)
common_coef <- intersect(names(b_ols), names(b_iv))
b_diff <- b_iv[common_coef] - b_ols[common_coef]

vcov_ols <- vcov(ols_mod)[common_coef, common_coef]
vcov_iv <- vcov(iv_mod)[common_coef, common_coef]
vcov_diff <- vcov_iv - vcov_ols

H_stat <- t(b_diff) %*% solve(vcov_diff) %*% b_diff
H_val <- as.numeric(H_stat)
df <- length(b_diff)
p_val <- pchisq(H_val, df = df, lower.tail = FALSE)

cat("Hausman Test Statistic:", H_val, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_val, "\n")

if (p_val < 0.05) {
  cat("❌ Reject null: At least one regressor is endogenous.\n")
} else {
  cat("✅ Fail to reject null: No strong evidence of endogeneity.\n")
}


test_dat <- na.omit(data[, c(
  "ROW", 
  "RawGini", "ROW_prev_actual", 
  "RawGini_c", "Gini2_c", "LagROW_c", 
  "RawGini_lag2", "Gini2_lag2", "ROW_prev_lag2"
)])


iv_mod_uncentered <- ivreg(
  ROW ~ RawGini + I(RawGini^2) + ROW_prev_actual |
    RawGini_lag2 + Gini2_lag2 + ROW_prev_lag2,
  data = test_dat
)

summary(iv_mod_uncentered, diagnostics = TRUE)
