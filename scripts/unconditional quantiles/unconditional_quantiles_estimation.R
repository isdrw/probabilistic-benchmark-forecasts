rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(purrr)

#source utility functions
source("scripts/utilities/utility_functions.R")
source("scripts/utilities/data_transformation_functions.R")

#load and prepare OECD quarterly data from oecd_quarterly_data.csv in folder: "data/raw"
df_oecd_q <- load_and_prepare_RW_data() 

#annual
df_oecd <- df_oecd_q %>% aggregate_to_annual_input()

#load and prepare data from file "data/raw/IMF WEO\WEOforecasts_prefilter.parquet"
df_weo <- load_and_prepare_WEO_data()

df_weo_g7 <- df_weo %>% filter(g7 == 1)


pred_unc_quantiles <- function(df, country, tau, target, h, nlag=1, R=11){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(1,nrow(data_by_country)-1)){
    data <- data_by_country[1:i,][[paste0("tv_", target)]]
    
    #start and end date of rolling window 
    end_year <- as.numeric(data_by_country[i,"forecast_year"])
    start_year <- as.numeric(data_by_country[1,"forecast_year"])
    end_quarter <- as.numeric(data_by_country[i,"forecast_quarter"])
    start_quarter <- as.numeric(data_by_country[1,"forecast_quarter"])
    
    #skip if only NAs
    if(all(is.na(data)) || is.null(data)){
      message("no valid data for ", country, " between ", start_year, " and ", end_year)
      next
    }
    
    #prediction
    preds <- unconditional_quantiles(data, tau, n_ahead = 1)
    preds_l <- preds$preds_l
    preds_u <- preds$preds_u
    
    # truth values
    truth_value <- if (i+1 <= nrow(data_by_country)){
      data_by_country[[paste0("tv_",target)]][i+1] 
    }else{
      NA
    } 
    
    # build all rows at once
    out_list[[index]] <- new_pred_row(
      country = country,
      forecast_year = end_year,
      target_year = NA_real_,
      horizon = h,
      target = target,
      tau = tau,
      lower_bound = preds_l,
      upper_bound = preds_u,
      truth_value = truth_value,
    )
    
    index <- index + 1
    
  }
  predictions <- bind_rows(out_list)
  return(predictions)
}

#=================
#prediction on quarterly data (OECD dataset)
#=================

#create grid 
grid_oecd <- crossing(
  country = unique(df_oecd$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = 1.0
)

#predict intervals for all combinations 
pred_oecd <- grid_oecd %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ pred_unc_quantiles(df_oecd, ..1, ..2, ..3, ..4)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


#truth value within predicted interval?
pred_oecd <- is_covered(pred_oecd)

#interval scores
pred_oecd <- calc_IS_of_df(pred_oecd)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

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

#save prediction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_oecd, paste0(
  "results/unconditional_quantiles/unconditional_quantiles_oecd_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_oecd_eval, paste0(
  "results/unconditional_quantiles/unconditional_quantiles_oecd_eval_", 
  timestamp, ".csv"), row.names = FALSE)



#create grid 
grid_oecd_q <- crossing(
  country = unique(df_oecd_q$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = 1.0
)

#predict intervals for all combinations 
pred_oecd_q <- grid_oecd %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ pred_unc_quantiles(df_oecd_q, ..1, ..2, ..3, ..4)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


#truth value within predicted interval?
pred_oecd_q <- is_covered(pred_oecd_q)

#interval scores
pred_oecd_q <- calc_IS_of_df(pred_oecd_q)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

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

#save prediction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_oecd_q, paste0(
  "results/unconditional_quantiles/quarterly/unconditional_quantiles_oecd_q_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_oecd_q_eval, paste0(
  "results/unconditional_quantiles/quarterly/unconditional_quantiles_oecd_q_eval_", 
  timestamp, ".csv"), row.names = FALSE)
#=================
#prediction on annual data (WEO dataset)
#=================

#create grid 
grid_weo <- crossing(
  country = unique(df_weo_g7$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("gdp", "cpi"),
  horizon = 1.0
)

#predict intervals for all combinations 
pred_weo <- grid_weo %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ pred_unc_quantiles(df_weo_g7, ..1, ..2, ..3, ..4)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


#truth value within predicted interval?
pred_weo <- is_covered(pred_weo)

#interval scores
pred_weo <- calc_IS_of_df(pred_weo)

#check calibration by calculating coverage for all prediction intervals, 
#forecast year 2013 and above, cumulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration

#filter prediction dataframe for specific horizon and period
pred_weo_filtered <- pred_weo %>% 
  filter(forecast_year>=2001, forecast_year<=2012)

#summary of scores
#coverage summary
#Interval score summary
#Weighted interval score summary for 50% and 80% intervals and 10%...90%
(pred_weo_eval <- pred_weo_filtered %>% 
    summarise_eval())

pred_oecd_q_eval %>% filter(tau %in% c(0.5, 0.8)) %>%print(n = Inf)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0(
  "results/unconditional_quantiles/unconditional_quantiles_weo_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_weo_eval, paste0(
  "results/unconditional_quantiles/unconditional_quantiles_weo_eval_", 
  timestamp, ".csv"), row.names = FALSE)
