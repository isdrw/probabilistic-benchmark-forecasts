rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(forecast)
library(purrr)
library(Matrix)
library(osqp)
library(isodistrreg)

#source utility functions
source("scripts/utilities/utility_functions.R")
source("scripts/utilities/data_transformation_functions.R")

#load and prepare WEO data
df_weo <- load_and_prepare_WEO_data()

#filter for G7 countries
df_weo_g7 <- df_weo %>% filter(g7 == 1)

#load and prepare data from file "data/processed/point_predictions_rw.csv" quarterly data
df_rw <- load_and_prepare_RW_data() %>% aggregate_to_annual_input()
#load and prepare data from file "data/processed/point_predictions_arima_1_0_0.csv" quarterly data
df_ar1 <- load_and_prepare_ARIMA1_0_0_data() %>% aggregate_to_annual_input()

#load and prepare data from file "data/processed/point_predictions_arima_1_1_0.csv" quarterly data
df_arima1_1_0 <- load_and_prepare_ARIMA1_1_0_data() %>% aggregate_to_annual_input()

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
    
    cat(
      paste0(
        "country: ", country, 
        "\nlower bound: ", pred_l, 
        "\nupper bound: ", pred_u,
        "\ntruth value: ", truth_value,
        "\nprediction: ", last_pred,
        "\niteration: ", index,
        "\ntau: ", tau
      )
    )
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
pred_weo <- grid_weo %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ fit_easyUQ_idr(df_weo_g7, ..1, ..2, ..3, ..4, R = 23)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


#create grid with all combinations for RW dataset
grid_rw <- crossing(
  country = unique(df_rw$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = c(0.0, 0.5, 1.0, 1.5)
)

#predict intervals for all combinations 
pred_rw <- grid_rw %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ fit_easyUQ_idr(df_rw, ..1, ..2, ..3, ..4, R = 23)
    )
  ) %>%
  pull(results) %>%
  bind_rows()
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


#find best rolling window size 
best_wis_sum <- Inf
best_R <- 11
for(R in 11:25){
  pred_weo <- grid_weo %>% 
    mutate(
      results = pmap(
        list(country, tau, target, horizon),
        ~ fit_easyUQ_idr(df_weo_g7, ..1, ..2, ..3, ..4, R = R)
      )
    ) %>%
    pull(results) %>%
    bind_rows()
  
  pred_weo <- pred_weo %>% is_covered() %>% calc_IS_of_df() 
  
  wis_sum <- pred_weo %>% 
    filter(forecast_year<=2025, forecast_year>=2001, horizon==0.5) %>% 
    summarise_eval() %>%
    pull(WIS_all) %>% sum()
  
  if(wis_sum <= best_wis_sum){
    best_R <- R
    best_wis_sum <- wis_sum
  }
}

#found best R to be 23 

