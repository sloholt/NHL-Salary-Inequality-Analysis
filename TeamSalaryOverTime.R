library(readxl)
library(dplyr)
library(tsibble)
library(fable)
library(tidyr)
library(ggplot2)
library(stringr)


df <- read_excel("CompleteTeamData.xlsx")
df <- df %>%
  mutate(Season = str_extract(Season, "\\d{4}"),
         Season = as.integer(Season))
df <- df %>% filter(!is.na(Season))

df_ts <- df %>%
  mutate(Season = as.integer(Season)) %>%
  as_tsibble(key = Team, index = Season)

#Average salary over time - team = color
df_ts %>%
  ggplot(aes(x = Season, y = `Ave Salary`, color = Team)) +
  geom_line(size = 1) +
  labs(title = "Average Salary Over Time for All Teams",
       y = "Average Salary",
       x = "Season") +
  theme_minimal()

#Average salary over time - faceted
df_ts %>%
  ggplot(aes(x = Season, y = `Ave Salary`)) +
  geom_line(color = "darkgreen") +
  facet_wrap(~ Team) +
  labs(title = "Average Salary Over Time per Team",
       y = "Average Salary",
       x = "Season")

#Salary variation over time - team = color
df_ts %>%
  ggplot(aes(x = Season, y = `Salary Variation`, color = Team)) +
  geom_line(size = 1) +
  labs(title = "Salary Variation Over Time for All Teams",
       y = "Salary Variation",
       x = "Season") +
  theme_minimal()

#Salary variation over time - faceted
df_ts %>%
  ggplot(aes(x = Season, y = `Salary Variation`)) +
  geom_line(color = "darkgreen") +
  facet_wrap(~ Team) +
  labs(title = "Salary Variation Over Time per Team",
       y = "Salary Variation",
       x = "Season")