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
source("scripts/utilities/data_transformation_functions.R")

#load and prepare data from file "data/raw/IMF WEO\oecd_quarterly_data.csv"
df_oecd <- load_and_prepare_oecd_data() %>% 
  aggregate_to_annual_input() 

#load and prepare data from file "data/raw/IMF WEO\WEOforecasts_prefilter.parquet"
df_weo <- load_and_prepare_WEO_data()

df_weo_g7 <- df_weo %>% filter(g7 == 1)

#
fit_qar_on <- function(df, country, target, h, tau = seq(0.1, 0.9, 0.1), nlag=1, R=11, n_ahead=1){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #lower and upper quantile level
  tau <- sort(tau)
  tau_lower <- (1 - tau) / 2
  tau_upper <- (1 + tau) / 2
  tau_all <- sort(unique(c(tau_lower, tau_upper)))
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(2,nrow(data_by_country)-1)){
    data <- data_by_country[1:i,][[paste0("tv_",target)]]
    
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
    
    #lagged values of i+1
    last_lags <- tail(data, nlag)
    
    #fit QAR model
    fits <- tryCatch({
      fit_qar(obs = data, last_obs = last_lags, tau = tau_all, nlag = nlag)
    },error=function(e){
      message("fit failed ", e$message)
      NULL
    })
    
    #null check fits
    if(is.null(fits)){
      next
    }
    

    for(j in seq_along(tau)){
      lower_bound <- fits[length(tau_all) / 2 - j + 1]
      upper_bound <- fits[length(tau_all) / 2 + j]
      
      # truth values
      truth_value <- data_by_country[i+1,][[paste0("tv_",target)]]
      forecast_year_1 <- as.numeric(data_by_country[i+1,"forecast_year"])
      forecast_quarter_1 <- as.numeric(data_by_country[i+1,"forecast_quarter"])
      
      # build all rows at once
      out_list[[index]] <- new_pred_row(
        country = country,
        forecast_year = forecast_year_1,
        forecast_quarter = forecast_quarter_1,
        target_year = NA,
        horizon = h,
        target = target,
        tau = tau[j],
        lower_bound = lower_bound,
        upper_bound = upper_bound,
        truth_value = truth_value,
      )
      
      index <- index + 1
    }
    
  }
  predictions <- bind_rows(out_list)
  return(predictions)
}



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
      ~ fit_qar_on(df_weo_g7, ..1, ..2, ..3)
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

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0(
  "results/qar_estimation/qar_prediction_weo_", 
  timestamp, ".csv"), row.names = FALSE)

write.csv(pred_weo_eval, paste0(
  "results/qar_estimation/qar_prediction_weo_eval_", 
  timestamp, ".csv"), row.names = FALSE)

