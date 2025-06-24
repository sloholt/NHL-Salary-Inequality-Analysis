#Pasta plots for Ortega Gamma Index 

library(ggplot2)
library(tidyverse)
library(readxl)
library(tidyfun)
library(dplyr)
library(colorspace)
library(ggridges)
library(viridis)

ort_data <- read_excel("Team_Inequality_Measures.xlsx")
ort_data$OrtegaGamma <- as.numeric(ort_data$OrtegaGamma)

ggplot(ort_data, aes(x = OrtegaGamma)) + 
  geom_histogram(binwidth = 0.01, fill = "forestgreen", color = "white") +
  labs(title = "Histogram of Ortega Gamma Coefficients",
       x = "Ortega Gamma Coefficient",
       y = "Frequency") +
  theme_minimal()

# Faceted histogram for all teams per year 
ggplot(ort_data, aes(x = OrtegaGamma)) +
  geom_histogram(binwidth = 0.01, fill = "darkmagenta", color = "white") +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "Histogram of Ortega Gamma Coefficients per Year",
       x = "Ortega Gamma Coefficient", y = "Count") +
  theme_minimal()

# Faceted histogram for all years per team 
ggplot(ort_data, aes(x = OrtegaGamma)) +
  geom_histogram(binwidth = 0.01, fill = "deeppink3", color = "white") +
  facet_wrap(~ Team, scales = "free_y") +
  labs(title = "Histogram of Ortega Gamma Coefficients per Team",
       x = "Ortega Gamma Coefficient", y = "Count") +
  theme_minimal()

# Spaghetti plot of Ortega Gamma per team per year 
ort_df <- ort_data %>%
  rename(
    team = Team,
    year = Year,
    ortega = OrtegaGamma
  ) %>%
  mutate(
    team = factor(team),
    year = as.numeric(year),
    ortega = as.numeric(ortega)
  )

ggplot(ort_df, aes(x = year, y = ortega, group = team, color = team)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  labs(
    title = "Ortega Gamma Coefficient per Team per Year",
    x = "Year",
    y = "Ortega Gamma Coefficient",
    color = "Team"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Lasagna plot (tile heatmap)
ggplot(ort_df, aes(x = year, y = team, fill = ortega)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    name = "Ortega Gamma",
    colors = c("#2c7bb6", "#66c2a5", "#a6d96a")
  ) +
  labs(
    title = "Lasagna Plot of Ortega Gamma by Team and Year",
    x = "Year",
    y = "Team"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

# Dot plot version for readability
ggplot(ort_df, aes(x = year, y = fct_reorder(team, -ortega), color = ortega)) +
  geom_point(size = 4) +
  scale_color_viridis_c(name = "Ortega Gamma") +
  labs(
    title = "Ortega Gamma Coefficient by Team and Year",
    x = "Year",
    y = "Team"
  ) +
  theme_minimal()





