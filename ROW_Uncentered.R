#Residual Diagnosis of GLM model with real data 

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

#REAL GLM MODEL:
real_glm <- glm(
  ROW ~ poly(RawGini,2) + ROW_prev_actual,
  family = poisson(link = "log"),
  data = data
)
summary(real_glm)
autoplot(real_glm)
cov2cor(vcov(real_glm))

update(real_glm, .~ . - poly(RawGini,2))
anova(real_glm)
anova(update(real_glm, .~ROW_prev_actual+poly(RawGini,2)))

#DEVIANCE RESIDUALS: 
res <- residuals(real_glm, type = "deviance")
fitted_vals <- fitted(real_glm)

ggplot(data.frame(Fitted = fitted_vals, Residuals = res), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Deviance Residuals vs Fitted Values")

#VALIDITY CHECKS: 

#Overdispersion: 
summary(real_glm)$deviance / summary(real_glm)$df.residual
#Influence:
influence_measures <- influence.measures(real_glm)
summary(influence_measures)
plot(cooks.distance(real_glm), type = "h", main = "Cook's Distance")






