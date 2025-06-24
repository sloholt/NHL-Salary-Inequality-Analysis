#Gini Histograms & Pasta Plots 

library(ggplot2)
library(tidyverse)
library(readxl)
library(tidyfun)
library(dplyr)
library(colorspace)
library(ggridges)
library(viridis)


gini_data <- read_excel("Team_Inequality_Measures.xlsx")
gini_data$RawGini <- as.numeric(gini$`RawGini`)

#Total Histogram for all teams all years
ggplot(gini, aes(x = `RawGini`)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Raw Gini Coefficients",
       x = "Raw Gini Coefficient",
       y = "Frequency") +
  theme_minimal()

#Faceted histogram for all teams per year 
ggplot(gini, aes(x = `RawGini`)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
  facet_wrap(~ `Year`, scales = "free_y") +
  labs(title = "Histogram of Raw Gini Coefficients per Year",
       x = "Raw Gini Coefficient", y = "Count") +
  theme_minimal()

#Faceted histogram for all years per team 
ggplot(gini, aes(x = `RawGini`)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
  facet_wrap(~ `Team`, scales = "free_y") +
  labs(title = "Histogram of Raw Gini Coefficients per Team",
       x = "Raw Gini Coefficient", y = "Count") +
  theme_minimal()

#Spaghetti plot of Gini per team per year 
gini_df <- gini_data %>%
  rename(
    team = Team,
    year = Year,
    gini = `RawGini`
  ) %>%
  mutate(
    team = factor(team),
    year = as.numeric(year),
    gini = as.numeric(gini)
  )
ggplot(gini_df, aes(x = year, y = gini, group = team, color = team)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  labs(
    title = "Raw Gini Coefficient per Team per Year",
    x = "Year",
    y = "Raw Gini Coefficient",
    color = "Team"
  ) +
  theme_minimal() +
  theme(legend.position = "right")



#lasagna plot of gini per team per year 
gini_df <- gini_data %>%
  rename(
    team = Team,
    year = Year,
    gini = RawGini
  ) %>%
  mutate(
    year = as.numeric(year),
    team = factor(team)
  )


ggplot(gini_df, aes(x = year, y = team, fill = gini)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    name = "Raw Gini",
    colors = c("#2c7bb6", "#66c2a5", "#a6d96a") 
  ) +
  labs(
    title = "Lasagna Plot of Raw Gini Coefficient by Team and Year",
    x = "Year",
    y = "Team"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

#Lasagna as a dot plot for easier readability 
ggplot(gini_df, aes(x = year, y = fct_reorder(team, -gini), color = gini)) +
  geom_point(size = 4) +
  scale_color_viridis_c(name = "Gini") +
  labs(
    title = "Gini Coefficient by Team and Year",
    x = "Year",
    y = "Team"
  ) +
  theme_minimal()

#Ridgeline plots

#Per Year
ggplot(gini_df, aes(x = gini, y = factor(year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Gini", option = "C") +
  labs(
    title = "Distribution of Raw Gini Coefficients Across Years",
    x = "Raw Gini Coefficient",
    y = "Year"
  ) +
  theme_minimal()

#Per Team
ggplot(gini_df, aes(x = gini, y = fct_reorder(team, gini, .fun = median), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Gini", option = "D") +
  labs(
    title = "Distribution of Gini Coefficients by Team",
    x = "Raw Gini Coefficient",
    y = "Team"
  ) +
  theme_minimal()







