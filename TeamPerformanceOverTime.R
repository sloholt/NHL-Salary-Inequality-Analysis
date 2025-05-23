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
unique(df$Season)
df <- df %>% filter(!is.na(Season))

df_ts <- df %>%
  mutate(Season = as.integer(Season)) %>%
  as_tsibble(key = Team, index = Season)

#ROW Team Performance Over Time: 
df_ts %>%
  ggplot(aes(x = Season, y = ROW, color = Team)) +
  geom_line() +
  labs(title = "Team Performance Over Time", y = "ROW (Regulation Wins)")

models <- df_ts %>% model(ARIMA_ROW = ARIMA(ROW))
forecasts <- models %>% forecast(h=1)

# ROW Team Performance Faceted: 
df_ts %>%
  ggplot(aes) +
  geom_line() +
  facet_wrap(~ Team) +
  labs(title = "Team Performance Over Time (Faceted)")

#GF Team Performance: 
df_ts %>%
  ggplot(aes(x = Season, y = GF, color = Team)) +
  geom_line() +
  labs(title = "Team Performance Over Time", y = "GF (Goals For)")

#GF per team over time (faceted)
df_ts %>%
  ggplot(aes(x = Season, y = GF)) +
  geom_line(color = "blue") +
  facet_wrap(~ Team) +
  labs(title = "Goals For (GF) Over Time per Team", y = "Goals For")
