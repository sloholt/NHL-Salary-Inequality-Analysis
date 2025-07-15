#ROW Calculations & Residual Diagnosis
#for the GMM Model using real data 
library(dplyr)
library(ggplot2)
library(ggfortify)
library(pdynmc)
library(plm)

data <- read.csv("CompleteTeamData.csv")
 
#Adding lagged vars & instruments
gmm_real_data <- data %>%
  arrange(Team, Year) %>%
  group_by(Team) %>%
  mutate(
    ROW_prev = lag(ROW, 1),
    ROW_lag2 = lag(ROW, 2)
  ) %>%
  ungroup() %>%
  select(Team, Year, ROW, RawGini, Gini2, ROW_prev) %>%  
  na.omit()

valid_teams <- gmm_real_data %>%
  group_by(Team) %>%
  filter(n() >= 8) %>%
  ungroup()

#Checking for persistence
panel_data <- pdata.frame(valid_teams, index = c("Team", "Year"))

# Simple AR(1) panel model
persistence_model <- plm(ROW ~ lag(ROW, 1), data = panel_data, model = "within")
summary(persistence_model)


#GMM with pdynmc
gmm_model <- pdynmc(
  dat = valid_teams, 
  varname.i = "Team", varname.t = "Year", 
  
  #Dep variable 
  include.y = TRUE, 
  varname.y = "ROW", 
  lagTerms.y = 1,
  maxLags.y = 7, 
  
  #Gini terms (endogenous regressors)
  include.x = TRUE, 
  varname.reg.end = c("RawGini", "Gini2"),
  lagTerms.reg.end = c(0,0),
  maxLags.reg.end = c(2,2),
  
  #Moment Conditions
  use.mc.diff = TRUE, #First-differences 
  use.mc.lev = FALSE, #No level-based instruments 
  use.mc.nonlin = FALSE, #No nonlinear moments
  
  #Including year in fixed effects 
  include.dum = FALSE, 
  dum.diff = FALSE,
  dum.lev = FALSE,
  varname.dum = "Year", 
  
  #GMM 
  w.mat = "iid.err", #Weighted matrix 
  std.err = "corrected", #Windmeijer corrected
  estimation = "twostep" #SHOULD THIS BE 1 OR 2? 

)
summary(gmm_model)
