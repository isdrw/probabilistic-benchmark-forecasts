rm(list = ls())
gc()

#load libraries
library(arrow)
library(dplyr)
library(tidyr)
library(forecast)
library(purrr)
library(future)
library(furrr)
library(progressr)
library(Matrix)
library(osqp)
library(isodistrreg)
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

#filter for G7 countries
df_weo_g7 <- df_weo %>% filter(g7 == 1)

#load and prepare data from file "data/processed/point_predictions_rw.csv" aggregated data
df_rw <- load_and_prepare_RW_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_1_0_0.csv" aggregated data
df_ar1 <- load_and_prepare_ARIMA1_0_0_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_1_1_0.csv" aggregated data
df_arima1_1_0 <- load_and_prepare_ARIMA1_1_0_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_auto.csv" aggregated data
df_arima_auto <- load_and_prepare_ARIMA_auto_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_rw.csv" quarterly data
df_rw_q <- load_and_prepare_RW_data()

#load and prepare data from file "data/processed/point_predictions_arima_1_0_0.csv" quarterly data
df_ar1_q <- load_and_prepare_ARIMA1_0_0_data()

#load and prepare data from file "data/processed/point_predictions_arima_1_1_0.csv" quarterly data
df_arima1_1_0_q <- load_and_prepare_ARIMA1_1_0_data() 

#load and prepare data from file "data/processed/point_predictions_arima_auto.csv" quarterly data
df_arima_auto_q <- load_and_prepare_ARIMA_auto_data()

#==================================================================


fit_easyUQ_idr <- function(df, country, tau, target, h, R = 11){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(2,nrow(data_by_country)-1)){
    #predicted value vector
    pred_vec <- data_by_country[1:i,][[paste0("pred_", target)]]
    
    #truth value vector
    data_tv1 <- data_by_country[1:i,][[paste0("tv_", target)]]
    
    #last prediction, truth value of point after current window
    last_pred <- as.numeric(data_by_country[[paste0("pred_", target)]][i+1])
    truth_value <- as.numeric(data_by_country[[paste0("tv_", target)]][i+1])
    
    #forecast year of point after rolling window for prediction
    forecast_year_end <- data_by_country[["forecast_year"]][i + 1]
    
    #forecast quarter of point after rolling window for prediction
    forecast_quarter_end <- as.numeric(data_by_country[["forecast_quarter"]][i + 1])
    
    #target year of point after rolling window for prediction
    target_year_end <- data_by_country[["target_year"]][i + 1]
    
    #target quarter of point after rolling window for prediction
    target_quarter_end <- (forecast_quarter_end + 4 * h - 1) %% 4 +1
    
    #skip if only NAs
    if(all(is.na(pred_vec)) || is.null(pred_vec) || 
       all(is.na(data_tv1)) || is.null(data_tv1)){
      message("no valid data for ", country, "\nfor target year: ", target_year_end)
      next
    }
    
    fit <- tryCatch({
      fit_easyUQ(x = pred_vec, y = as.numeric(data_tv1), tau = tau, x_new = last_pred)
    }, error = function(e){
      message("Fit failed for ", country, "\n for target year: ", target_year_end, "\n", e$message)
      NULL
    })
    
    if(is.null(fit)){
      next
    }
    
    pred_l <- tryCatch({
      fit$lower_bound[1]
    }, error = function(e){
      message("lower not computed for, ", country, "\n target year: ", target_year_end, "\n", e$message)
      NA_real_
    })
    
    pred_u <- tryCatch({
      fit$upper_bound[1]
    }, error = function(e){
      message("lower not computed for, ", country, "\n target year: ", target_year_end, "\n", e$message)
      NA_real_
    })
    
    #check for false lengths 
    if (!all(
      length(as.numeric(pred_l)) == 1,
      length(as.numeric(pred_u)) == 1,
      length(last_pred) == 1,
      length(truth_value) == 1
    )) {
      next
    }
    
    #new row
    out_list[[index]] <- new_pred_row(
      country = country,
      forecast_year = as.numeric(forecast_year_end),
      target_year = as.numeric(target_year_end),
      target_quarter = as.numeric(target_quarter_end),
      target = target,
      horizon = h,
      tau = tau,
      lower_bound = as.numeric(pred_l),
      upper_bound = as.numeric(pred_u),
      truth_value = as.numeric(truth_value),
      prediction = as.numeric(last_pred)
    )
    
    index <- index + 1
    
  }
  
  
  if(length(out_list) == 0){
    return(predictions)  
  }
  
  bind_rows(out_list)
}

#=================================

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
        res <- fit_easyUQ_idr(df_weo, country, tau, target, horizon)    
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

#Combine results
pred_weo <- bind_rows(weo_list)


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
        res <- fit_easyUQ_idr(df_rw, country, tau, target, horizon)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_rw <- bind_rows(rw_list)


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
        res <- fit_easyUQ_idr(df_ar1, country, tau, target, horizon)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_ar1 <- bind_rows(ar1_list)


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
        res <- fit_easyUQ_idr(df_arima1_1_0, country, tau, target, horizon)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_arima1_1_0 <- bind_rows(arima1_1_0_list)


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
        res <- fit_easyUQ_idr(df_arima_auto, country, tau, target, horizon)  
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
        res <- fit_easyUQ_idr(df_rw_q, country, tau, target, horizon)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_rw_q <- bind_rows(rw_q_list)


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
        res <- fit_easyUQ_idr(df_ar1_q, country, tau, target, horizon)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_ar1_q <- bind_rows(ar1_q_list)


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
        res <- fit_easyUQ_idr(df_arima1_1_0_q, country, tau, target, horizon)  
      })
      
      #progress bar update
      p()  
      res
    }
  )
})

# Combine results 
pred_arima1_1_0_q <- bind_rows(arima1_1_0_q_list)


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
        res <- fit_easyUQ_idr(df_arima_auto_q, country, tau, target, horizon)  
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
  filter(forecast_year<=2025, forecast_year>=2001)

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
  "results/EasyUQ_idr/easyUQ_prediction_weo_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_weo_eval, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_weo_eval_", 
  timestamp, ".csv"), row.names = FALSE)


#==============================================================================
##Evaluation of prediction on dataset Random Walk

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

pred_rw_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_rw, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_rw_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_rw_eval, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_rw_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================

##Evaluation of prediction on dataset AR1

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

pred_ar1_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_ar1, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_ar1_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_ar1_eval, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_ar1_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================

##Evaluation of prediction on dataset ARima(1,0,0)

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

pred_arima1_1_0_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_arima1_1_0, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_arima1_1_0_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima1_1_0_eval, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_arima1_1_0_eval_", 
  timestamp, ".csv"), row.names = FALSE)

#==============================================================================

##Evaluation of prediction on dataset ARima_auto

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

pred_arima_auto_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_arima_auto, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_arima_auto_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima_auto_eval, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_arima_auto_eval_", 
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

pred_rw_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_rw_q, paste0(
  "results/EasyUQ_idr/quarterly/easyUQ_prediction_rw_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_rw_q_eval, paste0(
  "results/EasyUQ_idr/quarterly/easyUQ_prediction_rw_q_eval_", 
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

pred_ar1_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_ar1_q, paste0(
  "results/EasyUQ_idr/quarterly/easyUQ_prediction_ar1_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_ar1_q_eval, paste0(
  "results/EasyUQ_idr/quarterly/easyUQ_prediction_ar1_q_eval_", 
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

pred_arima1_1_0_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_arima1_1_0_q, paste0(
  "results/EasyUQ_idr/quarterly/easyUQ_prediction_arima1_1_0_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima1_1_0_q_eval, paste0(
  "results/EasyUQ_idr/quarterly/easyUQ_prediction_arima1_1_0_q_eval_", 
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

pred_arima_auto_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save prediction and evaluation dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_arima_auto_q, paste0(
  "results/EasyUQ_idr/quarterly/easyUQ_prediction_arima_auto_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_arima_auto_q_eval, paste0(
  "results/EasyUQ_idr/easyUQ_prediction_arima_auto_q_eval_", 
  timestamp, ".csv"), row.names = FALSE)