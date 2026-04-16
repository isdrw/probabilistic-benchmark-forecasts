library(arrow)
library(dplyr)
library(tidyr)


#load AR1 and WEO point forecasts (annual)
df_weo <- load_and_prepare_WEO_data()
df_ar1 <- load_and_prepare_ARIMA1_0_0_data() %>% aggregate_to_annual_input()

#prepare data
ar1_data <- df_ar1 %>%
  filter(horizon %in% c(0.0, 0.5, 1.0, 1.5), target_year >= 2001, target_year <= 2012) %>%
  mutate(error = tv_gdp - pred_gdp,
         source = "AR1")
weo_data <- df_weo %>%
  filter(g7 == 1, horizon %in% c(0.0, 0.5, 1.0, 1.5), target_year >= 2001, target_year <= 2012) %>%
  mutate(error = tv_gdp - pred_gdp,
         source = "WEO")
df_merged <- bind_rows(ar1_data, weo_data)


#Test H0: error = 0
df_results <- df_merged %>%
  group_by(source, horizon, country) %>%
  summarise(p_value = t.test(error)$p.value)


#!!! Generative AI, level = high
#table of test results
df_results %>%
  mutate(
    stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    value = paste0(sprintf("%.3f", p_value), stars)
  ) %>%
  select(source, country, horizon, value) %>%
  pivot_wider(names_from = horizon, values_from = value)
#!!!