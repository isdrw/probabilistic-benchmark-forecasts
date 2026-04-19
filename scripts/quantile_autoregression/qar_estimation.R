#'!!! Generative AI; level = medium --> debugging + 
#'function calls pmap and 
#'other dplyr syntax suggestions

rm(list = ls(all=TRUE))
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(tseries)
library(forecast)
library(quantreg)
library(purrr)

#load functions
source("scripts/utilities/utility_functions.R")
source("scripts/quantile_autoregression/qar_functions.R")
source("scripts/utilities/data_transformation_functions.R")

#load and prepare data from file "data/raw/IMF WEO\oecd_quarterly_data.csv"
df_oecd_q <- load_and_prepare_oecd_data()

#annual
df_oecd <-df_oecd_q %>% aggregate_to_annual_input() 

#load and prepare data from file "data/raw/IMF WEO\WEOforecasts_prefilter.parquet"
df_weo <- load_and_prepare_WEO_data()

df_weo_g7 <- df_weo %>% filter(g7 == 1)

#=================
#prediction on annual data (WEO dataset)
#=================

#create grid 
grid_weo <- crossing(
  country = unique(df_weo_g7$country),
  target = c("gdp", "cpi"),
  horizon = 1.0
)

#predict intervals for all combinations 
pred_weo <- grid_weo %>% 
  mutate(
    results = pmap(
      list(country, target, horizon),
      ~ fit_qar_on_df(df_weo_g7, ..1, ..2, ..3)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#create grid 
grid_oecd <- crossing(
  country = unique(df_oecd$country),
  target = c("gdp", "cpi"),
  horizon = 1.0
)

#predict intervals for all combinations 
pred_oecd <- grid_oecd %>% 
  mutate(
    results = pmap(
      list(country, target, horizon),
      ~ fit_qar_on_df(df_oecd, ..1, ..2, ..3)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


#create grid 
grid_oecd_q <- crossing(
  country = unique(df_oecd_q$country),
  target = c("gdp", "cpi")
)

#predict intervals for all combinations 
pred_oecd_q <- grid_oecd_q %>% 
  mutate(
    results = pmap(
      list(country, target),
      ~ fit_qar_on_df(df_oecd_q, ..1, ..2)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#=============================

#truth value within predicted interval?
pred_weo <- is_covered(pred_weo)

#interval scores
pred_weo <- calc_IS_of_df(pred_weo)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries


#filter prediction dataframe for specific horizon and period
pred_weo_filtered <- pred_weo %>% 
  filter(forecast_year>=2001, forecast_year<=2012)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_weo_eval <- pred_weo_filtered %>% 
    summarise_eval())

pred_weo_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0(
  "results/qar_estimation/qar_prediction_weo_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_weo_eval, paste0(
  "results/qar_estimation/qar_prediction_weo_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#=============================

#truth value within predicted interval?
pred_oecd <- is_covered(pred_oecd)

#interval scores
pred_oecd <- calc_IS_of_df(pred_oecd)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries


#filter prediction dataframe for specific horizon and period
pred_oecd_filtered <- pred_oecd %>% 
  filter(forecast_year>=2001, forecast_year<=2012)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_oecd_eval <- pred_oecd_filtered %>% 
    summarise_eval())

pred_oecd_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_oecd, paste0(
  "results/qar_estimation/qar_prediction_oecd_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_oecd_eval, paste0(
  "results/qar_estimation/qar_prediction_oecd_eval_", 
  timestamp, ".csv"), row.names = FALSE)


#=======================================================================

pred_oecd_q <- pava_correct_df(pred_oecd_q)

#truth value within predicted interval?
pred_oecd_q <- is_covered(pred_oecd_q)

#interval scores
pred_oecd_q <- calc_IS_of_df(pred_oecd_q)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries


#filter prediction dataframe for specific horizon and period
pred_oecd_q_filtered <- pred_oecd_q %>% 
  filter(forecast_year>=2001, forecast_year<=2012)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_oecd_q_eval <- pred_oecd_q_filtered %>% 
    summarise_eval())

pred_oecd_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_oecd_q, paste0(
  "results/qar_estimation/quarterly/qar_prediction_oecd_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_oecd_q_eval, paste0(
  "results/qar_estimation/quarterly/qar_prediction_oecd_q_eval_", 
  timestamp, ".csv"), row.names = FALSE)
