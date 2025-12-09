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
df_oecd <- load_and_prepare_oecd_data()

#load and prepare data from file "data/raw/IMF WEO\WEOforecasts_prefilter.parquet"
df_weo <- load_and_prepare_WEO_data()

df_weo_g7 <- df_weo %>% filter(g7 == 1)

pred_quantiles <- function(df, country, tau, target, R = 44, n_ahead = 4){
  
  #initialize prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country) %>%
    arrange(forecast_year, forecast_quarter)
    
  for(i in seq(R,nrow(data_by_country))){
    data <- data_by_country[(i-R+1):i,][[paste0("tv_",target)]]
    #start date of observations 
    end_year <- as.numeric(data_by_country[i,"forecast_year"])
    start_year <- as.numeric(data_by_country[i-R+1,"forecast_year"])
    end_quarter <- as.numeric(data_by_country[i,"forecast_quarter"])
    start_quarter <- as.numeric(data_by_country[i-R+1,"forecast_quarter"])
    
    #skip if only NAs
    if(all(is.na(data)) || is.null(data)){
      message("no valid data for ", country, " between ", start_year, " and ", end_year)
      next
    }

    #prediction
    preds <- unconditional_quantiles(data, tau, n_ahead = n_ahead)
    preds_l <- preds$preds_l
    preds_u <- preds$preds_u

    # horizons (quarterly)
    h_steps <- 1:n_ahead
    horizons <- h_steps / 4
    
    # target quarter and year
    tq <- ((end_quarter - 1 + h_steps) %% 4) + 1
    ty <- end_year + ((end_quarter - 1 + h_steps) %/% 4)
    
    # truth values
    truth_values <- sapply(i + h_steps, function(idx) {
      if (idx <= nrow(data_by_country)){
        data_by_country[[paste0("tv_",target)]][idx] 
      }else{
        NA
      }   
    })
    
    #new row
    out_list[[index]] <- new_pred_row(
      country=country,
      forecast_year = end_year,
      target_year = ty,
      target_quarter = tq,
      target = target,
      horizon = horizons,
      tau = tau,
      lower_bound = preds_l,
      upper_bound = preds_u,
      truth_value = truth_values
    )
    
    index <- index + 1
  }
  
  predictions <- bind_rows(out_list)
      
  return(predictions)
}



pred_quantiles_weo <- function(df, country, tau, target, h, nlag=1, R=11){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(R,nrow(data_by_country))){
    data <- data_by_country[(i-R+1):i,][[paste0("tv_", target)]]
    
    #start and end date of rolling window 
    end_year <- as.numeric(data_by_country[i,"forecast_year"])
    start_year <- as.numeric(data_by_country[i-R+1,"forecast_year"])
    end_quarter <- as.numeric(data_by_country[i,"forecast_quarter"])
    start_quarter <- as.numeric(data_by_country[i-R+1,"forecast_quarter"])
    
    #skip if only NAs
    if(all(is.na(data)) || is.null(data)){
      message("no valid data for ", country, " between ", start_year, " and ", end_year)
      next
    }
    
    #prediction
    preds <- unconditional_quantiles(data, tau, n_ahead = 1)
    preds_l <- preds$preds_l
    preds_u <- preds$preds_u
    
    # target quarter and year vectors based on  forecasts
    tq <- (end_quarter + 4 * (h - floor(h)) - 1) %% 4 + 1
    ty <- as.numeric(data_by_country[i,"target_year"])
    
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
      target_year = ty,
      target_quarter = tq,
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
  target = c("gdp", "cpi")
)

pred_oecd <- grid_oecd %>% 
  mutate(
    results = pmap(
      list(country, tau, target),
      ~ pred_quantiles(df_oecd, ..1, ..2, ..3)
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
  filter(forecast_year>=2001, forecast_year<=2012, horizon==0.5)

#coverage summary
pred_oecd_filtered %>% 
  summarise_coverage_of_df()

#Interval score summary
pred_oecd_filtered %>% 
  summarise_IS_of_df()


pred_oecd_filtered %>% 
  summarise_WIS_of_df()

#save prediction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_oecd, paste0("results/unconditional_quantiles/unconditional_quantiles_oecd_", timestamp, ".csv"), row.names = FALSE)


#=================
#prediction on annual data (WEO dataset)
#=================

#create grid 
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
      ~ pred_quantiles_weo(df_weo_g7, ..1, ..2, ..3, ..4)
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
  filter(forecast_year>=2001, forecast_year<=2012, horizon==0.5)

#coverage summary
pred_weo_filtered %>% 
  summarise_coverage_of_df()

#Interval score summary
pred_weo_filtered %>% 
  summarise_IS_of_df()

#Weighted interval score summary for 50% and 80% intervals
pred_weo_filtered %>% 
  summarise_WIS_of_df()

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0("results/unconditional_quantiles/unconditional_quantiles_annual_", timestamp, ".csv"), row.names = FALSE)
