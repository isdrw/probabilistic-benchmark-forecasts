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
library(bayesQR)

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

fit_bqr <- function(df, country, tau, target, h){
  
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
    pred_vec <- data_by_country[2:i,][[paste0("pred_", target)]]
    
    #lagged values
    lagged_tv <- data_by_country[1:(i-1), ][[paste0("tv_", target)]]
    
    #truth value vector
    data_tv1 <- data_by_country[2:i,][[paste0("tv_", target)]]
    
    #bind to matrix
    data_bqr <- data.frame(
      y = data_tv1,
      lagged_tv = lagged_tv,
      pred_vec = pred_vec
    )
    
    #last prediction, truth value and last lagged value of point after rolling window
    last_pred <- as.numeric(data_by_country[i+1,][[paste0("pred_", target)]])
    last_tv_lag <- as.numeric(data_by_country[i, ][[paste0("tv_", target)]])
    truth_value <- as.numeric(data_by_country[[paste0("tv_", target)]][i+1])
    
    #start date of rolling window
    forecast_year_start <- data_by_country[2,"forecast_year"]
    
    #forecast year of point after rolling window for prediction
    forecast_year_end <- data_by_country[i+1,"forecast_year"]
    
    #forecast quarter of point after rolling window for prediction
    forecast_quarter_end <- as.numeric(data_by_country[i+1,"forecast_quarter"])
    
    #target year of point after rolling window for prediction
    target_year_end <- data_by_country[i+1,"target_year"]
    
    #target quarter of point after rolling window for prediction
    target_quarter_end <- (forecast_quarter_end + 4 * h - 1) %% 4 +1
  
    #remove NAs
    data_bqr <- na.omit(data_bqr)
    
    cat(
      "\ni:", i,
      " rows:", nrow(data_bqr),
      " anyNA:", anyNA(data_bqr)
    )
    
    #fit lqr model 
    fit_l <- tryCatch({
      bayesQR::bayesQR(
        y ~ lagged_tv + pred_vec, 
        data = data_bqr, 
        quantile = (1 - tau) / 2, 
        ndraw = 5000,
        normal.approx = FALSE
      )
      #bqr(data_bqr$y, data_bqr[,-1], p = (1-tau)/2, seed = 2026, use_minesota = TRUE)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    fit_u <- tryCatch({
      bayesQR::bayesQR(
        y ~ lagged_tv + pred_vec, 
        data = data_bqr, 
        quantile = (1 + tau) / 2, 
        ndraw = 5000,
        normal.approx = FALSE
      )
      #bqr(data_bqr$y, data_bqr[,-1], p = (1+tau)/2, seed = 2026, use_minesota = TRUE)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    if(is.null(fit_l)||is.null(fit_u)){
      next
    }
    
    sum_l <- tryCatch(summary(fit_l, burnin = 500), error = function(e) NULL)
    sum_u <- tryCatch(summary(fit_u, burnin = 500), error = function(e) NULL)
    
    betadraws_l <- sum_l[[1]]$betadraw
    betadraws_u <- sum_u[[1]]$betadraw
    
    if(is.null(betadraws_l) || !is.matrix(betadraws_l) || nrow(betadraws_l) == 0 ||
       is.null(betadraws_u) || !is.matrix(betadraws_u) || nrow(betadraws_u) == 0) {
      message("No beta draws at iteration ", i, ", skipping.")
      next
    }
    
    # Remove rows with NA
    betadraws_l <- betadraws_l[complete.cases(betadraws_l), ]
    betadraws_u <- betadraws_u[complete.cases(betadraws_u), ]
    
    # Skip if all rows removed
    if(nrow(betadraws_l) == 0 || nrow(betadraws_u) == 0) {
      message("All beta draws are NA at iteration ", i)
      next
    }
 
    if(is.null(betadraws_l)||is.null(betadraws_u)||!is.matrix(betadraws_l)||!is.matrix(betadraws_u)){
      message("No beta draws produced")
      next
    }
    
    #extract means of beta draws
    beta_l <- colMeans(betadraws_l, na.rm = TRUE)
    beta_u <- colMeans(betadraws_u, na.rm = TRUE)
    
    #prediction of quantile based on last point forecast
    pred_l <- beta_l[1] + beta_l[2] * last_tv_lag + beta_l[3] * last_pred
    pred_u <- beta_u[1] + beta_u[2] * last_tv_lag + beta_u[3] * last_pred
    
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
  
  predictions <- bind_rows(out_list)
  
  return(predictions)
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
      ~ fit_bqr(df_weo_g7, ..1, ..2, ..3, ..4)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#===========================================================
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

pred_weo_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0(
  "results/bayesian_quantile_regression/bqr_prediction_weo_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_weo_eval, paste0(
  "results/bayesian_quantile_regression/bqr_prediction_weo_eval_", 
  timestamp, ".csv"), row.names = FALSE)


