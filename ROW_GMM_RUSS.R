#ROW Calculations & Residual Diagnosis
#for the GMM Model using real data 
library(dplyr)
library(ggplot2)
library(ggfortify)
library(pdynmc)
library(plm)

data <- read.csv("CompleteTeamData.csv")
 
data[,c("RawGini","Gini2")]<-poly(data$RawGini,2)
  
#Adding lagged vars & instruments
gmm_real_data <- data %>%
  arrange(Team, Year) %>%
  ungroup() 

gmm_real_data$ROW_prev = 
  with(gmm_real_data,c(NA,ROW[-length(ROW)]))

gmm_real_data$ROW_lag2 = 
  with(gmm_real_data,c(NA,NA,
                       ROW[-c(length(ROW)-1,
                              length(ROW))]))



gmm_real_data<-gmm_real_data%>%  
  select(Team, Year, ROW, RawGini, Gini2) %>%  
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
  lagTerms.y = 2,
  
  #Gini terms (endogenous regressors)
  include.x = TRUE, 
  varname.reg.end = c("RawGini", "Gini2"),
  lagTerms.reg.end = c(1,1),
  maxLags.reg.end = c(3,3),
  
  # varname.reg.end = c("RawGini"),
  # lagTerms.reg.end = c(1),
  # maxLags.reg.end = c(3),
  
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











