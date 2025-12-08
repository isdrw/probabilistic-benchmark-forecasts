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

#filter for G7 countries
df_weo_g7 <- df_weo %>% filter(g7 == 1)

#load and prepare data from file "data/processed/point_predictions_rw.csv" quarterly data
df_rw <- load_and_prepare_RW_data()

#load and prepare data from file "data/processed/point_predictions_arima_1_0_0.csv" quarterly data
df_ar1 <- load_and_prepare_ARIMA1_0_0_data()

fit_lqr <- function(df, country, tau, target, h, R=11){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(R,nrow(data_by_country)-1)){
    #predicted value vector
    data_pred <- data_by_country[(i-R+1):i,][[paste0("pred_", target)]]
    
    #last prediction and truth value of point after rolling window
    last_pred <- as.numeric(data_by_country[i+1,][[paste0("pred_", target)]])
    truth_value <- as.numeric(data_by_country[[paste0("tv_", target)]][i+1])
    
    #truth value vector
    data_tv1 <- data_by_country[(i-R+1):i,][[paste0("tv_", target)]]

    #start date of rolling window
    forecast_year_start <- data_by_country[(i-R+1),"forecast_year"]
    
    #forecast year of point after rolling window for prediction
    forecast_year_end <- data_by_country[i+1,"forecast_year"]
    
    #target year of point after rolling window for prediction
    target_year_end <- data_by_country[i+1,"target_year"]
    
    #skip if only NAs
    if(all(is.na(data_pred)) || is.null(data_pred) || 
       all(is.na(data_tv1)) || is.null(data_tv1)){
      message("no valid data for ", country, " between ", 
              forecast_year_start, " and ", forecast_year_end)
      next
    }
    
    #fit lqr model 
    fit_l <- tryCatch({
      rq(formula = data_tv1 ~ data_pred, tau = (1-tau)/2)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    fit_u <- tryCatch({
      rq(formula = data_tv1 ~ data_pred, tau = (1+tau)/2)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    if(is.null(fit_l)||is.null(fit_u)){
      next
    }
    
    #prediction of quantile based on last point forecast
    new_data <- data.frame(data_pred = last_pred)
    pred_l <- as.numeric(predict(fit_l, newdata = new_data))
    pred_u <- as.numeric(predict(fit_u, newdata = new_data))
    
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
      ~ fit_lqr(df_weo_g7, ..1, ..2, ..3, ..4)
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
      ~ fit_lqr(df_rw, ..1, ..2, ..3, ..4, R = 44)
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
      ~ fit_lqr(df_ar1, ..1, ..2, ..3, ..4, R = 44)
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
write.csv(pred_weo, paste0("results/linear_quantile_regression/ldr_prediction_", timestamp, ".csv"), row.names = FALSE)


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
write.csv(pred_rw, paste0("results/linear_quantile_regression/ldr_prediction_rw_", timestamp, ".csv"), row.names = FALSE)


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
write.csv(pred_ar1, paste0("results/linear_quantile_regression/ldr_prediction_ar1_", timestamp, ".csv"), row.names = FALSE)

