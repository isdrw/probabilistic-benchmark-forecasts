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
df_rw <- load_and_prepare_RW_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_1_0_0.csv" quarterly data
df_ar1 <- load_and_prepare_ARIMA1_0_0_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_1_1_0.csv" quarterly data
df_arima1_1_0 <- load_and_prepare_ARIMA1_1_0_data() %>% aggregate_to_annual_input()

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
#calc predictions errors
df_arima1_1_0 <- df_arima1_1_0 %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)

#filter for G7 countries
df_weo_g7 <- df_weo %>% filter(g7 == 1)

#function to calculate quantiles of fitted t distribution and prediction interval
fit_t_student <- function(df, country, tau, target, h, R=11, fit_mean=FALSE){
  
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
    
    #forecast_quarter of i+1
    forecast_quarter_end <- data_by_country[i+1,][["forecast_quarter"]]
    
    #target_year of i+1
    target_year_end <- data_by_country[i+1,][["target_year"]]
    
    #target_quarter of i+1 (needed for temporal aggregation to annual data)
    target_quarter_end <- (forecast_quarter_end - 1 + 4*h) %% 4 + 1
    
    #prediction of i+1
    last_pred <- data_by_country[i+1,][[paste0("pred_", target)]]
    
    #truth value of i+1
    truth_value <- data_by_country[i+1,][[paste0("tv_", target)]]
    
    #fit normal distibution
    fit <- fit_t_distribution(err_set)
    
    #default mean
    fitted_mean <- 0
    
    #if mean estimated as well
    if(fit_mean){
      fitted_mean <- fit$mean
    }
    
    #extract standard deviation
    fitted_sd <- fit$sd
    fitted_df <- fit$df
    
    #quantiles of fitted t distribution
    q_l <- fitted_mean + fitted_sd * qt((1-tau)/2, df = fitted_df)
    q_u <- fitted_mean + fitted_sd * qt((1+tau)/2, df = fitted_df)
    
    #append to output dataframe
    pred_l <- last_pred + q_l
    pred_u <- last_pred + q_u
    
    #new row
    out_list[[index]] <- new_pred_row(
      country = as.character(country),
      forecast_year = as.numeric(forecast_year_end),
      target_year = as.numeric(target_year_end),
      target_quarter = as.numeric(target_quarter_end),
      target = as.character(target),
      horizon = as.numeric(h),
      tau = as.numeric(tau),
      lower_bound = as.numeric(pred_l),
      upper_bound = as.numeric(pred_u),
      truth_value = as.numeric(truth_value),
      prediction = as.numeric(last_pred)
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
      ~ fit_t_student(df_weo_g7, ..1, ..2, ..3, ..4, fit_mean = TRUE)
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
      ~ fit_t_student(df_rw, ..1, ..2, ..3, ..4, R = 11, fit_mean = FALSE)
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
      ~ fit_t_student(df_ar1, ..1, ..2, ..3, ..4, R = 11, fit_mean = FALSE)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#prediction on ARIMA(1,1,0) data (R=44 due to quarterly data)
grid_arima1_1_0  <- crossing(
  country = unique(df_arima1_1_0$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.5, 1.0)
)

pred_arima1_1_0 <- grid_arima1_1_0 %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ fit_t_student(df_arima1_1_0, ..1, ..2, ..3, ..4, R = 11, fit_mean = FALSE)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#==============================================================================
##Evaluation of prediction on dataset WEO (annual)

#truth value within predicted interval?
pred_weo <- is_covered(pred_weo)

#interval scores
pred_weo <- calc_IS_of_df(pred_weo)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_weo_filtered <- pred_weo %>% 
  filter(forecast_year<=2012, forecast_year>=2001, horizon==0.5)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_weo_eval <- pred_weo_filtered %>% 
    summarise_eval())

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0(
  "results/t_quantiles_prediction/mean 0 assumption/t_prediction_weo_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_weo_eval, paste0(
  "results/t_quantiles_prediction/mean 0 assumption/t_prediction_weo_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================
##Evaluation of prediction on dataset Random Walk (quarterly, generated)

#truth value within predicted interval?
pred_rw <- is_covered(pred_rw)

#interval scores
pred_rw <- calc_IS_of_df(pred_rw)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_rw_filtered <- pred_rw %>% 
  filter(forecast_year<=2012, forecast_year>=2001, horizon==0.5)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_rw_eval <- pred_rw_filtered %>% 
    summarise_eval())

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_rw, paste0(
  "results/t_quantiles_prediction/mean 0 assumption/t_prediction_rw_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_rw_eval, paste0(
  "results/t_quantiles_prediction/mean 0 assumption/t_prediction_rw_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================
##Evaluation of prediction on dataset ARIMA(1,0,0) (quarterly, generated)

#truth value within predicted interval?
pred_ar1 <- is_covered(pred_ar1)

#interval scores
pred_ar1 <- calc_IS_of_df(pred_ar1)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_ar1_filtered <- pred_ar1 %>% 
  filter(forecast_year<=2012, forecast_year>=2001, horizon==0.5)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_ar1_eval <- pred_ar1_filtered %>% 
    summarise_eval())

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_ar1, paste0(
  "results/t_quantiles_prediction/mean 0 assumption/t_prediction_ar1_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_ar1_eval, paste0(
  "results/t_quantiles_prediction/mean 0 assumption/t_prediction_ar1_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================
##Evaluation of prediction on dataset ARIMA(1,1,0) (quarterly, generated)

#truth value within predicted interval?
pred_arima1_1_0 <- is_covered(pred_arima1_1_0)

#interval scores
pred_arima1_1_0 <- calc_IS_of_df(pred_arima1_1_0)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_arima1_1_0_filtered <- pred_arima1_1_0 %>% 
  filter(forecast_year<=2012, forecast_year>=2001, horizon==0.5)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_arima1_1_0_eval <- pred_arima1_1_0_filtered %>% 
    summarise_eval())


#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_arima1_1_0, paste0(
  "results/t_quantiles_prediction/mean 0 assumption/t_prediction_arima1_1_0_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima1_1_0_eval, paste0(
  "results/t_quantiles_prediction/mean 0 assumption/t_prediction_arima1_1_0_eval_", 
  timestamp, ".csv"), row.names = FALSE)



