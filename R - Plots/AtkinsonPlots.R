#Pasta Plots for Atkinson Index 

library(ggplot2)
library(tidyverse)
library(readxl)
library(tidyfun)
library(dplyr)
library(colorspace)
library(ggridges)
library(viridis)

atkinson <- read_excel("Team_Inequality_Measures.xlsx")
atkinson$Atkinson <- as.numeric(atkinson$Atkinson)

# Histogram for all teams, all years
ggplot(atkinson, aes(x = Atkinson)) +
  geom_histogram(binwidth = 0.01, fill = "cadetblue", color = "white") +
  labs(title = "Histogram of Atkinson Index Values",
       x = "Atkinson Index",
       y = "Frequency") +
  theme_minimal()

# Faceted histogram for each year
ggplot(atkinson, aes(x = Atkinson)) +
  geom_histogram(binwidth = 0.01, fill = "blueviolet", color = "white") +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "Histogram of Atkinson Index per Year",
       x = "Atkinson Index", y = "Count") +
  theme_minimal()

# Faceted histogram for each team
ggplot(atkinson, aes(x = Atkinson)) +
  geom_histogram(binwidth = 0.01, fill = "deepskyblue2", color = "white") +
  facet_wrap(~ Team, scales = "free_y") +
  labs(title = "Histogram of Atkinson Index per Team",
       x = "Atkinson Index", y = "Count") +
  theme_minimal()

atk_df <- atkinson %>%
  rename(
    team = Team,
    year = Year,
    atk = Atkinson
  ) %>%
  mutate(
    team = factor(team),
    year = as.numeric(year),
    atk = as.numeric(atk)
  )

# Spaghetti plot
ggplot(atk_df, aes(x = year, y = atk, group = team, color = team)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  labs(
    title = "Atkinson Index per Team per Year",
    x = "Year",
    y = "Atkinson Index",
    color = "Team"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Lasagna plot
ggplot(atk_df, aes(x = year, y = team, fill = atk)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    name = "Atkinson",
    colors = c("#762a83", "#af8dc3", "#e7d4e8")
  ) +
  labs(
    title = "Lasagna Plot of Atkinson Index by Team and Year",
    x = "Year",
    y = "Team"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

# Dot-based lasagna plot for clarity
ggplot(atk_df, aes(x = year, y = fct_reorder(team, -atk), color = atk)) +
  geom_point(size = 4) +
  scale_color_viridis_c(name = "Atkinson Index") +
  labs(
    title = "Atkinson Index by Team and Year",
    x = "Year",
    y = "Team"
  ) +
  theme_minimal()





