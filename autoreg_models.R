
library(readxl)
library(forecast)
library(ggplot2)
library(patchwork)
library(tidyr)
library(tidyverse)
library(gtools)
library(fs)


df <- read_excel("CompleteTeamData.xlsx")
df <- df %>% mutate(Season_numeric = as.numeric(substr(Season, 1, 4)))

#Trying for 1 team first
team_name <- "CGY"
team_data <- df %>% filter(Team == team_name) %>% arrange(Season)

row_ts <- ts(team_data$ROW, start = min(team_data$Season_numeric), frequency = 1)

#AR(1) Model:
ar1_model <- Arima(row_ts, order = c(1, 0, 0))
summary(ar1_model)

fitted_vals <- fitted(ar1_model)

plot_df <- team_data %>%
  mutate(Fitted_ROW = fitted_vals)

ggplot(plot_df, aes(x = Season_numeric)) +
  geom_line(aes(y = ROW), color = "blue", linewidth = 1.2, linetype = "dashed") +
  geom_line(aes(y = Fitted_ROW), color = "red", linewidth = 1.2) +
  labs(title = paste("AR(1) Model -", team_name),
       x = "Season", y = "ROW",
       caption = "Blue dashed = Actual, Red = Fitted") +
  theme_minimal()

#AR(2) Model: 
ar2_model <- Arima(row_ts, order = c(2, 0, 0))
summary(ar2_model)

fitted_vals_ar2 <- fitted(ar2_model)

plot_df <- team_data %>% mutate(Fitted_ROW_AR2 = fitted_vals_ar2)

ggplot(plot_df, aes(x = Season_numeric)) +
  geom_line(aes(y = ROW), color = "blue", linewidth = 1.2, linetype = "dashed") +
  geom_line(aes(y = Fitted_ROW_AR2), color = "darkgreen", linewidth = 1.2) +
  labs(title = paste("AR(2) Model -", team_name),
       x = "Season", y = "ROW",
       caption = "Blue dashed = Actual, Green = AR(2) Fitted") +
  theme_minimal()

#Forecasting 1 season ahead with AR(2)
forecast_ar2 <- forecast(ar2_model, h=1)
forecast_value <- as.numeric(forecast_ar2$mean)
forecast_year <- max(plot_df$Season_numeric) + 1

forecast_row <- plot_df[1, ]  
forecast_row[] <- NA 
forecast_row$Season_numeric <- forecast_year
forecast_row$Fitted_ROW_AR2 <- forecast_value

# Combine using bind_rows
plot_df_extended <- bind_rows(plot_df, forecast_row)

# Plot with forecast added
ggplot(plot_df_extended, aes(x = Season_numeric)) +
  geom_line(aes(y = ROW), color = "blue", linewidth = 1.2, linetype = "dashed", na.rm = TRUE) +
  geom_line(aes(y = Fitted_ROW_AR2), color = "darkgreen", linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = Fitted_ROW_AR2), data = plot_df_extended %>% filter(Season_numeric == forecast_year),
             color = "darkgreen", size = 3) +
  labs(title = paste("AR(2) Fitted and Forecast -", team_name),
       subtitle = paste("2025 ROW forecast:", round(forecast_value, 1)),
       x = "Season", y = "ROW") +
  theme_minimal()

#Multiple Models for Multiple Teams: 
valid_teams <- df %>%
  group_by(Team) %>%
  summarise(
    n = n(),
    na_row = any(is.na(ROW)) | any(is.na(GF)),
    constant_row = sd(ROW, na.rm = TRUE) == 0,
    constant_gf = sd(GF, na.rm = TRUE) == 0
  ) %>%
  filter(n >= 3, !na_row, !constant_row, !constant_gf) %>%
  pull(Team)

df <- df %>% filter(Team %in% valid_teams)

df <- df %>%
  arrange(Team, Season) %>%
  group_by(Team) %>%
  mutate(Season = factor(Season, levels = unique(Season))) %>%
  ungroup()


#Fit & Forecast AR Models
fit_ar_models <- function(data, variable, order){
  ts_data <- ts(data[[variable]])
  fit <- arima(ts_data, order = c(order, 0, 0))
  fitted_vals <- fitted(fit)
  return(fitted_vals)
}

df <- df %>% filter(Team != "Utah Hockey Club")

teams <- unique(df$Team)

plot_data <- map_dfr(valid_teams, function(team) {
  team_data <- df %>% filter(Team == team)
  
  team_data %>%
    mutate(
      ROW_AR1 = fit_ar_models(team_data, "ROW", 1),
      GF_AR1 = fit_ar_models(team_data, "GF", 1),
      ROW_AR2 = fit_ar_models(team_data, "ROW", 2),
      GF_AR2 = fit_ar_models(team_data, "GF", 2)
    )
})
plot_data <- plot_data %>%
  mutate(Season = factor(Season, levels = mixedsort(unique(Season))))
plot_data <- plot_data %>%
  group_by(Team) %>%
  arrange(Season) %>%
  mutate(Season_Num = row_number()) %>%
  ungroup()


#Plotting 1 team 
plot_team_ar <- function(team_df, team_name){
  df_team <- team_df %>% filter(Team == team_name)
  
  p1 <- ggplot(df_team, aes(x = Season_Num)) +
    geom_line(aes(y = ROW, color = "Actual ROW")) +
    geom_line(aes(y = ROW_AR1, color = "AR(1) ROW")) +
    labs(title = paste(team_name, "- AR(1) ROW"), y = "ROW", x = "Season") +
    theme_minimal()
  
  p2 <- ggplot(df_team, aes(x = Season_Num)) +
    geom_line(aes(y = GF, color = "Actual GF")) +
    geom_line(aes(y = GF_AR1, color = "AR(1) GF")) +
    labs(title = paste(team_name, "- AR(1) GF"), y = "GF", x = "Season") +
    theme_minimal()
  
  p3 <- ggplot(df_team, aes(x = Season_Num)) +
    geom_line(aes(y = ROW, color = "Actual ROW")) +
    geom_line(aes(y = ROW_AR2, color = "AR(2) ROW")) +
    labs(title = paste(team_name, "- AR(2) ROW"), y = "ROW", x = "Season") +
    theme_minimal()
  
  p4 <- ggplot(df_team, aes(x = Season_Num)) +
    geom_line(aes(y = GF, color = "Actual GF")) +
    geom_line(aes(y = GF_AR2, color = "AR(2) GF")) +
    labs(title = paste(team_name, "- AR(2) GF"), y = "GF", x = "Season") +
    theme_minimal()
  
  (p1 | p2) / (p3 | p4)
}

output_dir <- "team_ar_plots"
dir_create(output_dir)

for(team in valid_teams) {
  plot <- plot_team_ar(plot_data, team)
  
  ggsave(
    filename = file.path(output_dir, paste0(team, "_AR_models.png")),
    plot = plot,
    width = 12, height = 10, dpi = 300
  )
}

#Plotting all teams: 
long_plot_data <- plot_data %>%
  pivot_longer(cols = c(ROW_AR1, GF_AR1, ROW_AR2, GF_AR2),
               names_to = "Model_Metric",
               values_to = "Fitted") %>%
  mutate(
    Metric = case_when(str_detect(Model_Metric, "ROW") ~ "ROW", TRUE ~ "GF"),
    Model = case_when(str_detect(Model_Metric, "AR1") ~ "AR(1)", TRUE ~ "AR(2)")
  )

ggplot(long_plot_data, aes(x = Season)) +
  geom_line(aes(y = Fitted, color = "Fitted", group = interaction(Team, Model, Metric))) +
  geom_line(aes(y = ifelse(Metric == "ROW", ROW, GF), color = "Actual", group = interaction(Team, Metric))) +
  facet_grid(Team ~ interaction(Model, Metric), scales = "free_y") +
  labs(title = "AR(1) and AR(2) Fits for ROW and GF by Team",
       y = "Value", x = "Season", color = "Legend") +
  theme_minimal(base_size = 9) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 90, size = 6),
    axis.text.y = element_text(size = 6)
  )



