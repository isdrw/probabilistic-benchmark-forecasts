rm(list = ls(all=TRUE))
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(tseries)
library(forecast)
library(purrr)

#source utility functions
source("scripts/utilities/utility_functions.R")
source("scripts/utilities/data_transformation_functions.R")

#load and prepare OECD quarterly data from oecd_quarterly_data.csv in folder: "data/raw"
df_oecd <- load_and_prepare_oecd_data()


fit_rw <- function(df, country, target, n_ahead = 4){
  
  #prediction dataframe
  predictions <- init_output_df(interval = FALSE)
  
  #output list 
  out_list <- list()
  index <- 1
  
  #time series data for each country and target
  data_by_country <- df %>% 
    filter(country == !!country) %>%
    arrange(forecast_year, forecast_quarter)
  
  n <- nrow(data_by_country)
  for(i in 2:n){
    last_value <- data_by_country[[target]][i-1]
    if(is.na(last_value)){
      next
    }
    
    tv_end <- min(i + n_ahead - 1, n)
    truth_values <- data_by_country[i:tv_end, ][[target]]
    #fill with NA for non-existent truth_values
    if(length(truth_values) < n_ahead){
      truth_values <- c(truth_values, rep(NA, n_ahead - length(truth_values)))
    }
    
    #start/end date of observations 
    end_year <- data_by_country[i,"forecast_year"]
    end_quarter <- data_by_country[i,"forecast_quarter"]
    
    #random walk prediction (last observation)
    pred_rw <- rep(last_value, n_ahead)
    
    # horizons (quarterly)
    h_steps <- 1:n_ahead
    horizons <- h_steps / 4
    
    # target quarter and year vectors based on n_ahead forecasts
    tq <- ((end_quarter - 1 + h_steps) %% 4) + 1
    ty <- end_year + ((end_quarter - 1 + h_steps) %/% 4)
    
    out_list[[index]] <- new_pred_row(
      country = rep(country,n_ahead),
      forecast_year = rep(end_year,n_ahead),
      forecast_quarter = rep(end_quarter,n_ahead),
      target_year = ty,
      target_quarter = tq,
      horizon = horizons,
      target = rep(target,n_ahead),
      truth_value = truth_values,
      prediction = pred_rw,
      interval = FALSE,
    )
    
    index <- index + 1
    
  }
  
  predictions <- bind_rows(out_list)
  
  return(predictions)
}

#create grid 
grid <- crossing(
  country = unique(df_oecd$country),
  target = c("tv_gdp", "tv_cpi")
)

#fit random walk on quarterly OECD data with rolling window
pred_rw <- grid %>% 
  mutate(
    results = pmap(
      list(country, target),
      ~ fit_rw(df_oecd, ..1, ..2)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

write.csv(pred_rw, "data/processed/point predictions/point_predictions_rw.csv", row.names = FALSE)


