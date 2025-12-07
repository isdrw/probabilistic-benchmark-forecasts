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
df <- load_and_prepare_WEO_data()

#calc predictions errors
df <- df %>% mutate(
  gdp_err = abs(tv_gdp - pred_gdp),
  cpi_err = abs(tv_cpi - pred_cpi)
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

#create grid with all combinations
grid_weo <- grid_weo <- crossing(
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

#PAVA correction 
pred_weo <- pred_weo %>% 
  group_by(country, target, forecast_year, tau) %>%
  group_modify(~{
    #pava corrections for interval widths over all horizons
    interval_widths <- .x$prediction - .x$lower_bound
    interval_widths <- pava_correction(interval_widths)
    
    #update bounds
    .x$lower_bound <- .x$prediction - interval_widths
    .x$upper_bound <- .x$prediction + interval_widths
    
    .x
  }) %>% ungroup()

#truth value within predicted interval?
pred_weo <- is_covered(pred_weo)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration
pred_weo %>% 
  filter(target=="cpi", forecast_year>=2013, horizon==0.5) %>% 
  summarise_coverage_of_df()

#interval scores
pred_weo <- calc_IS_of_df(pred_weo)

pred_weo %>% 
  filter(target=="cpi", forecast_year>=2013, horizon==0.5) %>% 
  summarise_IS_of_df()


pred_weo %>% 
  filter(target=="cpi", forecast_year>=2013, horizon==0.5) %>%
  calc_WIS_of_df() %>%
  as.numeric() %>%
  mean(na.rm=TRUE)


#check calibration by calculating coverage for 80% prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration
mean(pred_weo %>% filter(tau == 0.5, forecast_year >= 2013, target == "gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)
mean(pred_weo %>% filter(tau == 0.8, forecast_year >= 2013, target == "gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0("results/empirical_quantiles_prediction/empirical_prediction_", timestamp, ".csv"), row.names = FALSE)

