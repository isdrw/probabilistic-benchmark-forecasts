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
library(future)
library(furrr)
library(progressr)
#==================================================================
#settings

#progress bar
handlers(global = TRUE)
handlers("txtprogressbar")

#multi-threading 
future::plan(multisession, workers = 4)

#source utility functions
source("scripts/utilities/utility_functions.R")
source("scripts/utilities/data_transformation_functions.R")

#==================================================================
#load and prepare WEO data
df_weo <- load_and_prepare_WEO_data()

#load and prepare data from file "data/processed/point_predictions_rw.csv" aggregated data
df_rw <- load_and_prepare_RW_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_1_0_0.csv" aggregated data
df_ar1 <- load_and_prepare_ARIMA1_0_0_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_1_1_0.csv" aggregated data
df_arima1_1_0 <- load_and_prepare_ARIMA1_1_0_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_auto.csv" aggregated data
df_arima_auto <- load_and_prepare_ARIMA_auto_data() %>% aggregate_to_annual_input()

#quarterly data
#load and prepare data from file "data/processed/quarterly_horizons/point_predictions_rw.csv" quarterly data
df_rw_q <- load_and_prepare_RW_data(
  "data/processed/point predictions/quarterly_horizons/point_predictions_rw.csv"
)

#load and prepare data from file "data/processed/point_predictions_arima_1_0_0.csv" quarterly data
df_ar1_q <- load_and_prepare_ARIMA1_0_0_data(
  "data/processed/point predictions/quarterly_horizons/point_predictions_arima1_0_0.csv"
)

#load and prepare data from file "data/processed/point_predictions_arima_1_1_0.csv" quarterly data
df_arima1_1_0_q <- load_and_prepare_ARIMA1_1_0_data(
  "data/processed/point predictions/quarterly_horizons/point_predictions_arima1_1_0.csv"
) 

#load and prepare data from file "data/processed/point_predictions_arima_auto.csv" quarterly data
df_arima_auto_q <- load_and_prepare_ARIMA_auto_data(
  "data/processed/point predictions/quarterly_horizons/point_predictions_arima_auto.csv"
)

#==================================================================

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
#calc predictions errors
df_arima_auto <- df_arima_auto %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)

#calc predictions errors
df_rw_q <- df_rw_q %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)
#calc predictions errors
df_ar1_q <- df_ar1_q %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)
#calc predictions errors
df_arima1_1_0_q <- df_arima1_1_0_q %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)
#calc predictions errors
df_arima_auto_q <- df_arima_auto_q %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)

#filter for G7 countries
df_weo_g7 <- df_weo %>% filter(g7 == 1)
#==================================================================


#function to calculate quantiles of fitted normal distribution and prediction interval
fit_gauss <- function(df, country, tau, target, h, R=11, fit_mean=FALSE, unbiased_sd = FALSE){
  
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
    fit_n <- fit_normal_distribution(err_set)
    
    #default mean
    fitted_mean <- 0
    
    #if mean estimated as well
    if(fit_mean){
      fitted_mean <- fit_n$estimate["mean"]  
    }
    
    #extract standard deviation
    fitted_sd <- fit_n$estimate["sd"]
    if(unbiased_sd){
     fitted_sd <- sd(err_set) 
    }
    q_l <- qnorm((1-tau)/2,mean = fitted_mean,sd = fitted_sd)
    q_u <- qnorm((1+tau)/2,mean = fitted_mean,sd = fitted_sd)
    
    #append to output dataframe
    pred_l <- last_pred + q_l
    pred_u <- last_pred + q_u
    
    #new row
    out_list[[index]] <- new_pred_row(
      country = country,
      forecast_year = forecast_year_end,
      target_year = target_year_end,
      target_quarter = target_quarter_end,
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
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_weo))
  
  weo_list <- future_pmap(
    list(grid_weo$country, grid_weo$tau, grid_weo$target, grid_weo$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_weo, country, tau, target, horizon, fit_mean = TRUE, unbiased_sd = TRUE)    
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

#Combine results
pred_weo <- bind_rows(weo_list)

#============

#create grid with all combinations for RW dataset
grid_rw <- crossing(
  country = unique(df_rw$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_rw))
  
  rw_list <- future_pmap(
    list(grid_rw$country, grid_rw$tau, grid_rw$target, grid_rw$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_rw, country, tau, target, horizon, fit_mean = TRUE, unbiased_sd = TRUE)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_rw <- bind_rows(rw_list)

#============

#create grid with all combinations for ar1 dataset
grid_ar1 <- crossing(
  country = unique(df_ar1$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_ar1))
  
  ar1_list <- future_pmap(
    list(grid_ar1$country, grid_ar1$tau, grid_ar1$target, grid_ar1$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_ar1, country, tau, target, horizon, fit_mean = FALSE, unbiased_sd = TRUE)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_ar1 <- bind_rows(ar1_list)

#============

#create grid with all combinations for arima1_1_0 dataset
grid_arima1_1_0 <- crossing(
  country = unique(df_arima1_1_0$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_arima1_1_0))
  
  arima1_1_0_list <- future_pmap(
    list(grid_arima1_1_0$country, grid_arima1_1_0$tau, grid_arima1_1_0$target, grid_arima1_1_0$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_arima1_1_0, country, tau, target, horizon, fit_mean = TRUE, unbiased_sd = FALSE)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_arima1_1_0 <- bind_rows(arima1_1_0_list)

#============

#create grid with all combinations for arima_auto dataset
grid_arima_auto <- crossing(
  country = unique(df_arima_auto$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_arima_auto))
  
  arima_auto_list <- future_pmap(
    list(grid_arima_auto$country, grid_arima_auto$tau, grid_arima_auto$target, grid_arima_auto$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_arima_auto, country, tau, target, horizon, fit_mean = TRUE, unbiased_sd = TRUE)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_arima_auto <- bind_rows(arima_auto_list)

#============================================================

#Quarterly predictions
#create grid with all combinations for RW dataset quarterly
grid_rw_q <- crossing(
  country = unique(df_rw_q$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_rw_q))
  
  rw_q_list <- future_pmap(
    list(grid_rw_q$country, grid_rw_q$tau, grid_rw_q$target, grid_rw_q$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_rw_q, country, tau, target, horizon, R = 44, fit_mean = TRUE, unbiased_sd = TRUE)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_rw_q <- bind_rows(rw_q_list)

#============

#create grid with all combinations for ar1 dataset quarterly
grid_ar1_q <- crossing(
  country = unique(df_ar1_q$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_ar1_q))
  
  ar1_q_list <- future_pmap(
    list(grid_ar1_q$country, grid_ar1_q$tau, grid_ar1_q$target, grid_ar1_q$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_ar1_q, country, tau, target, horizon, R = 44, fit_mean = TRUE, unbiased_sd = TRUE)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_ar1_q <- bind_rows(ar1_q_list)

#============

#create grid with all combinations for arima1_1_0 dataset quarterly
grid_arima1_1_0_q <- crossing(
  country = unique(df_arima1_1_0_q$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_arima1_1_0_q))
  
  arima1_1_0_q_list <- future_pmap(
    list(grid_arima1_1_0_q$country, grid_arima1_1_0_q$tau, 
         grid_arima1_1_0_q$target, grid_arima1_1_0_q$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_arima1_1_0_q, country, tau, target, horizon, R = 44, fit_mean = TRUE, unbiased_sd = TRUE)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_arima1_1_0_q <- bind_rows(arima1_1_0_q_list)

#============

#create grid with all combinations for arima_auto dataset quarterly
grid_arima_auto_q <- crossing(
  country = unique(df_arima_auto_q$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
with_progress({
  p <- progressor(along = 1:nrow(grid_arima_auto_q))
  
  arima_auto_q_list <- future_pmap(
    list(grid_arima_auto_q$country, grid_arima_auto_q$tau, 
         grid_arima_auto_q$target, grid_arima_auto_q$horizon),
    function(country, tau, target, horizon) {
      suppressWarnings({
        res <- fit_gauss(df_arima_auto_q, country, tau, target, horizon, R = 44, fit_mean = TRUE, unbiased_sd = TRUE)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_arima_auto_q <- bind_rows(arima_auto_q_list)
#============================================================
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
  filter(forecast_year<=2012, forecast_year>=2001)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_weo_eval <- pred_weo_filtered %>% 
    summarise_eval())

pred_weo_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()


#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_weo, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/gauss_prediction_weo_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_weo_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/gauss_prediction_weo_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================
##Evaluation of prediction on dataset Random Walk  generated

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
  filter(forecast_year<=2012, forecast_year>=2001)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_rw_eval <- pred_rw_filtered %>% 
    summarise_eval())

pred_rw_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()


#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_rw, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/gauss_prediction_rw_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_rw_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/gauss_prediction_rw_eval_", 
  timestamp, ".csv"), row.names = FALSE)

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
  filter(forecast_year<=2012, forecast_year>=2001)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_ar1_eval <- pred_ar1_filtered %>% 
    summarise_eval())

pred_ar1_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()


#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_ar1, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/mean 0 assumption/gauss_prediction_ar1_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_ar1_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/mean 0 assumption/gauss_prediction_ar1_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================
##Evaluation of prediction on dataset ARIMA(1,1,0) (quarterly, generated)

#PAVA correction 
pred_arima1_1_0 <- pava_correct_df(pred_arima1_1_0)

#truth value within predicted interval?
pred_arima1_1_0 <- is_covered(pred_arima1_1_0)

#interval scores
pred_arima1_1_0 <- calc_IS_of_df(pred_arima1_1_0)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_arima1_1_0_filtered <- pred_arima1_1_0 %>% 
  filter(forecast_year<=2012, forecast_year>=2001)


#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_arima1_1_0_eval <- pred_arima1_1_0_filtered %>% 
  summarise_eval())

pred_arima1_1_0_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()


#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_arima1_1_0, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/gauss_prediction_arima1_1_0_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima1_1_0_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/gauss_prediction_arima1_1_0_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================
##Evaluation of prediction on dataset ARIMA auto fitted (quarterly, generated)

#PAVA correction 
pred_arima_auto <- pava_correct_df(pred_arima_auto)

#truth value within predicted interval?
pred_arima_auto <- is_covered(pred_arima_auto)

#interval scores
pred_arima_auto <- calc_IS_of_df(pred_arima_auto)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_arima_auto_filtered <- pred_arima_auto %>% 
  filter(forecast_year<=2012, forecast_year>=2001)


#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_arima_auto_eval <- pred_arima_auto_filtered %>% 
    summarise_eval())

pred_arima_auto_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()


#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_arima_auto, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/gauss_prediction_arima_auto_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima_auto_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/gauss_prediction_arima_auto_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================

##Evaluation of prediction on dataset Random Walk quarterly

#PAVA correction
pred_rw_q <- pava_correct_df(pred_rw_q)

#truth value within predicted interval?
pred_rw_q <- is_covered(pred_rw_q)

#interval scores
pred_rw_q <- calc_IS_of_df(pred_rw_q)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_rw_q_filtered <- pred_rw_q %>% 
  filter(forecast_year<=2012, forecast_year>=2001)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_rw_q_eval <- pred_rw_q_filtered %>% 
    summarise_eval())

pred_rw_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()


#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_rw_q, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/quarterly/gauss_prediction_rw_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_rw_q_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/quarterly/gauss_prediction_rw_q_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================

##Evaluation of prediction on dataset AR1

#PAVA correction
pred_ar1_q <- pava_correct_df(pred_ar1_q)

#truth value within predicted interval?
pred_ar1_q <- is_covered(pred_ar1_q)

#interval scores
pred_ar1_q <- calc_IS_of_df(pred_ar1_q)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_ar1_q_filtered <- pred_ar1_q %>% 
  filter(forecast_year<=2012, forecast_year>=2001)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_ar1_q_eval <- pred_ar1_q_filtered %>% 
    summarise_eval())

pred_ar1_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_ar1_q, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/quarterly/gauss_prediction_ar1_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_ar1_q_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/quarterly/gauss_prediction_ar1_q_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================

##Evaluation of prediction on dataset ARima(1,0,0)

#PAVA correction
pred_arima1_1_0_q <- pava_correct_df(pred_arima1_1_0_q)

#truth value within predicted interval?
pred_arima1_1_0_q <- is_covered(pred_arima1_1_0_q)

#interval scores
pred_arima1_1_0_q <- calc_IS_of_df(pred_arima1_1_0_q)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_arima1_1_0_q_filtered <- pred_arima1_1_0_q %>% 
  filter(forecast_year<=2012, forecast_year>=2001)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_arima1_1_0_q_eval <- pred_arima1_1_0_q_filtered %>% 
    summarise_eval())

pred_arima1_1_0_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_arima1_1_0_q, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/quarterly/gauss_prediction_arima1_1_0_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima1_1_0_q_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/quarterly/gauss_prediction_arima1_1_0_q_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================

##Evaluation of prediction on dataset ARima_auto

#PAVA correction
pred_arima_auto_q <- pava_correct_df(pred_arima_auto_q)

#truth value within predicted interval?
pred_arima_auto_q <- is_covered(pred_arima_auto_q)

#interval scores
pred_arima_auto_q <- calc_IS_of_df(pred_arima_auto_q)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_arima_auto_q_filtered <- pred_arima_auto_q %>% 
  filter(forecast_year<=2012, forecast_year>=2001)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_arima_auto_q_eval <- pred_arima_auto_q_filtered %>% 
    summarise_eval())

pred_arima_auto_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>% 
  make_latex_block_string()


#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
write.csv(pred_arima_auto_q, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/quarterly/gauss_prediction_arima_auto_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima_auto_q_eval, paste0(
  "results/gauss_quantiles_prediction/unbiased VAR est/fitted_mean/quarterly/gauss_prediction_arima_auto_q_eval_", 
  timestamp, ".csv"), row.names = FALSE)


#====================================================================

#find best rolling window on training dataset for g7 countries to calculate prediction interval
#based on quantiles of fitted normal distribution
#best R between 1 and 20 is R for which average weighted Interval score over all taus 
#found insignificant differences between rolling windows of 11-16 --> 11 for better comparability 
#to other methods

best_R <- 5
best_score <- Inf
score_list <- list()
for(R in seq(5,16,by=1)){
  cat("current rolling window R", R,"\n")
  #set of taus
  taus <- seq(0.1,0.9,by=0.1)
  #prediction list for each tau
  predictions <- vector("list", length(taus))
  names(predictions) <- paste0("tau_",taus)
  
  #loop over taus
  for(i in seq_along(taus)){
    #calculate prediction interval for tau 
    predictions[[i]] <- calc_all_pred(
      df = df_training,
      countries = countries,
      target_variables = target_variables,
      h = c(0.5, 1.0),
      tau = taus[i],
      R = R,
      fit_mean=FALSE
    )
    predictions[[i]] <- calc_interval_score(predictions[[i]])
  }
  #weighted prediction score
  predictions_weighted <- calc_weighted_IS(predictions)
  average_weighted_IS <- mean(predictions_weighted[[1]]$IS_weighted,na.rm=TRUE)
  score_list <- append(score_list,average_weighted_IS)
  #update if better score is achieved
  if(average_weighted_IS <= best_score){
    best_R <- R
    best_score <- average_weighted_IS
  }
  cat("R =", R, "average WIS =", average_weighted_IS, "\n")
}

#found R=16
best_R
best_score

#calculate prediction intervals for all taus (0.1,...,0.9)
##Predictions were made on holdout dataset (>=2013-R)
##For the first R predictions the rolling window was smaller than R
##Predictions from 2013 onwards are with rolling window R
taus <- seq(0.1,0.9,by=0.1)
predictions <- vector("list", length(taus))
names(predictions) <- paste0("tau_",taus)

#loop over taus
for(i in seq_along(taus)){
  predictions[[i]] <- calc_all_pred(
    df = df_holdout,
    countries = countries,
    target_variables = target_variables,
    h = c(0.5, 1.0),
    tau = taus[i],
    R = 11,
    fit_mean = TRUE
  )
  predictions[[i]] <- calc_interval_score(predictions[[i]])
}

predictions_weighted <- calc_weighted_IS(predictions)

#save prediction
for(i in seq_along(taus)){
  tau <- taus[i]
  
  filename <- paste0("prediction_fitted_mean_g7_tau_",tau,".parquet")
  folder <- "results/gauss_quantiles_prediction/fitted_mean"
  path <- file.path(folder,filename)
  write_parquet(predictions_weighted[[i]],path)
}




