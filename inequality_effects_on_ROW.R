#Testing the affect of each inequality measure on ROW 

library(readxl)
library(dplyr)

performance <- read_excel("PerformanceTeamData.xlsx")
inequality <- read_excel("Team_Inequality_Measures.xlsx")

colnames(performance)
colnames(inequality)

merged_data <- inner_join(performance, inequality, by = c("Team", "Year"))
write.csv(merged_data, "CompleteTeamData.csv", row.names = FALSE)

#SLR modeling ROW as a function of Raw Gini
gini_model <- lm(ROW ~ RawGini, data = merged_data)
summary(gini_model)

#SLR Modeling ROW as a function of Atkinson 
atk_model <- lm(ROW ~ Atkinson, data = merged_data)
summary(atk_model)

#SLR Modeling ROW as a function of the Ortega Gamma index 
ort_model <- lm(ROW ~ OrtegaGamma, data = merged_data)
summary(ort_model)

#SLR Modeling ROW as a function of all 3 
model <- lm(ROW ~ RawGini + Atkinson + OrtegaGamma, 
            data = merged_data)
summary(model)

#Interaction testing 
model_interact <- lm(ROW ~ RawGini * Atkinson + OrtegaGamma, 
                     data = merged_data)
summary(model_interact) 



