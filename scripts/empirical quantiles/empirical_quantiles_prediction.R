rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(nortest)
library(fitdistrplus)
library(forecast)
library(quantreg)
library(purrr)

#source utility functions
source("scripts/utilities/utility_functions.R")
source("scripts/utilities/data_transformation_functions.R")

#load and prepare WEO data
df_weo <- load_and_prepare_WEO_data()

#load and prepare data from file "data/processed/point_predictions_rw.csv" quarterly data
df_rw <- load_and_prepare_RW_data()

#load and prepare data from file "data/processed/point_predictions_arima_1_0_0.csv" quarterly data
df_ar1 <- load_and_prepare_ARIMA1_0_0_data()

#calc predictions errors
df_weo <- df_weo %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)
#calc predictions errors
df_rw <- df_rw %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)
#calc predictions errors
df_ar1 <- df_ar1 %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)

#filter for G7 countries
df_weo_g7 <- df %>% filter(g7 == 1)

#function to calculate empirical quantiles and prediction interval
fit_emp <- function(df, country, tau, target, h, R = 11){
  
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #filter data by country and horizon and arrange by year and quarter (in case of unsorted data)
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  #loop over each target year
  for(i in R:(nrow(data_by_country)-1)){
    #set of abs prediction errors 
    err_set <- data_by_country[(i-R+1):i,][[paste0(target, "_err")]]
    
    #replacement of NA with median
    err_set[is.na(err_set)] <-  median(err_set)
    
    #forecast_year of i+1
    forecast_year_end <- data_by_country[i+1,][["forecast_year"]]
    
    #target_year of i+1
    target_year_end <- data_by_country[i+1,][["target_year"]]
    
    #prediction of i+1
    last_pred <- data_by_country[i+1,][[paste0("pred_", target)]]
    
    #truth value of i+1
    truth_value <- data_by_country[i+1,][[paste0("tv_", target)]]
    
    #empirical quantiles of abs error set
    quantile <- quantile(err_set, probs=tau, type=7, na.rm=TRUE)
    
    #append to output dataframe
    pred_l <- last_pred - quantile
    pred_u <- last_pred + quantile
    
    #new row
    out_list[[index]] <- new_pred_row(
      country = country,
      forecast_year = forecast_year_end,
      target_year = target_year_end,
      target = target,
      horizon = h,
      tau = tau,
      lower_bound = pred_l,
      upper_bound = pred_u,
      truth_value = truth_value,
      prediction = last_pred
    )
    
    index <- index + 1
  }
  
  predictions <- bind_rows(out_list)
  
  return(predictions)
}

#==============================================================================

#create grid with all combinations
grid_weo <- crossing(
  country = unique(df_weo_g7$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.5, 1.0)
)

#predict intervals for all combinations 
pred_weo <- grid_weo %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ fit_emp(df_weo_g7, ..1, ..2, ..3, ..4)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#prediction on Random Walk data (R=44 due to quarterly data)
grid_rw  <- crossing(
  country = unique(df_rw$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.5, 1.0)
)

pred_rw <- grid_rw %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ fit_emp(df_rw, ..1, ..2, ..3, ..4, R = 44)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#prediction on AR(1) data (R=44 due to quarterly data)
grid_ar1  <- crossing(
  country = unique(df_ar1$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.5, 1.0)
)

pred_ar1 <- grid_ar1 %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ fit_emp(df_ar1, ..1, ..2, ..3, ..4, R = 44)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#==============================================================================
##Evaluation of prediction on dataset WEO (annual)

#PAVA correction 
pred_weo <- pava_correct_df(pred_weo)

#truth value within predicted interval?
pred_weo <- is_covered(pred_weo)

#interval scores
pred_weo <- calc_IS_of_df(pred_weo)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_weo_filtered <- pred_weo %>% 
  filter(forecast_year<2013, horizon==0.5)

#coverage summary
pred_weo_filtered %>% 
  summarise_coverage_of_df()

#Interval score summary
pred_weo_filtered %>% 
  summarise_IS_of_df()

#Weighted interval score summary for 50% and 80% intervals
pred_weo_filtered %>% 
  calc_WIS_of_df(taus = c(0.5, 0.8)) %>%
  as.numeric() %>%
  mean(na.rm=TRUE)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0("results/empirical_quantiles_prediction/empirical_prediction_", timestamp, ".csv"), row.names = FALSE)


#==============================================================================
##Evaluation of prediction on dataset Random Walk (quarterly, generated)

#PAVA correction 
pred_rw <- pava_correct_df(pred_rw)

#truth value within predicted interval?
pred_rw <- is_covered(pred_rw)

#interval scores
pred_rw <- calc_IS_of_df(pred_rw)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_rw_filtered <- pred_rw %>% 
  filter(forecast_year<2013, horizon==0.5)

#coverage summary
pred_rw_filtered %>% 
  summarise_coverage_of_df()

#Interval score summary
pred_rw_filtered %>% 
  summarise_IS_of_df()

#Weighted interval score summary for 50% and 80% intervals
pred_rw_filtered %>% 
  calc_WIS_of_df(taus = c(0.5, 0.8)) %>%
  as.numeric() %>%
  mean(na.rm=TRUE)

#save prediction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_rw, paste0("results/empirical_quantiles_prediction/empirical_prediction_rw_", timestamp, ".csv"), row.names = FALSE)


#==============================================================================
##Evaluation of prediction on dataset ARIMA(1,0,0) (quarterly, generated)

#PAVA correction 
pred_ar1 <- pava_correct_df(pred_ar1)

#truth value within predicted interval?
pred_ar1 <- is_covered(pred_ar1)

#interval scores
pred_ar1 <- calc_IS_of_df(pred_ar1)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_ar1_filtered <- pred_ar1 %>% 
  filter(forecast_year<2013, horizon==0.5)

#coverage summary
pred_ar1_filtered %>% 
  summarise_coverage_of_df()

#Interval score summary
pred_ar1_filtered %>% 
  summarise_IS_of_df()

#Weighted interval score summary for 50% and 80% intervals
pred_ar1_filtered %>% 
  calc_WIS_of_df(taus = c(0.5, 0.8)) %>%
  as.numeric() %>%
  mean(na.rm=TRUE)

#save prediction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_ar1, paste0("results/empirical_quantiles_prediction/empirical_prediction_ar1_", timestamp, ".csv"), row.names = FALSE)

