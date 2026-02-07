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

#Function to fit ARIMA model either with given order or with automatic
#fitting of best order according to AIC
fit_arima <- function(df, country, target, R = 44, n_ahead = 7, order=c(1,0,0), auto=FALSE){
  
  #prediction
  predictions <- init_output_df(interval = FALSE)
  
  #output list 
  out_list <- list()
  index <- 1
  
  data_by_country <- df %>% 
    filter(country == !!country) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(1 + order[1],(nrow(data_by_country)-n_ahead))){
    data <- data_by_country[1:i,][[target]]
    tv_end <- min(i + n_ahead, nrow(data_by_country))
    truth_values <- data_by_country[(i+1):tv_end, ][[target]]
    #fill with NA for non-existent truth_values
    if(length(truth_values) < n_ahead){
      truth_values <- c(truth_values, rep(NA_real_, n_ahead - length(truth_values)))
    }
    
    #start/end date of observations 
    start_year <- data_by_country[1,"forecast_year"]
    end_year <- data_by_country[i,"forecast_year"]
    start_quarter <- data_by_country[1,"forecast_quarter"]
    end_quarter <- data_by_country[i,"forecast_quarter"]
    start_date <- c(start_year,start_quarter)
    
    #skip if only NAs
    if(all(is.na(data)) || is.null(data)){
      message("no valid data for ", country, " and year: ", end_year)
      next
    }
    
    #convert to time series object
    ts_data <- ts(data, start = start_date,frequency = 4)
    
    #NA handling
    ts_data <- na.omit(ts_data)
    
    #ARIMA model fit with order=order
    fit <- tryCatch({
      if(!auto){
        arima(ts_data, order = order)
      }else{
        #fit model with best BIC 
        auto.arima(ts_data, ic = "bic")
      }
    },error=function(e){
      message("Fit failed for ", country, " (", start_year, "–", end_year, "): ", e$message)
      NULL
    })
    
    
    #predict values for upcoming 1 (4 quarters)
    #first prediction equals 0.0 horizon forecast and 3rd prediction equals 0.5 horizon forecast...
    pred <- tryCatch({
      predict(fit, n.ahead = n_ahead)
    },error=function(e){
      message("Prediction failed for ", country, " (", start_year, "–", end_year, "): ", e$message)
      NULL
    })
    
    pred <- pred$pred
    
    if(is.null(pred)){
      next
    }
    
    # horizons (quarterly)
    h_steps <- 1:n_ahead
    horizons <- h_steps / 4 - 0.25
    
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
      prediction = pred,
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

#fit ARIMA(1,0,0) on quarterly OECD data with rolling window
pred_1_0_0 <- grid %>% 
  mutate(
    results = pmap(
      list(country, target),
      ~ fit_arima(df_oecd, ..1, ..2)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#fit ARIMA(1,1,0) on quarterly OECD data with rolling window --> 1 integration was stationary 
pred_1_1_0 <- grid %>% 
  mutate(
    results = pmap(
      list(country, target),
      ~ fit_arima(df_oecd, ..1, ..2, order = c(1,1,0))
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#fit ARIMA model with auto fitting based on AIC on quarterly OECD data with rolling window
pred_arima_auto <- grid %>% 
  mutate(
    results = pmap(
      list(country, target),
      ~ fit_arima(df_oecd, ..1, ..2, auto = TRUE)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#save predictions
write.csv(pred_1_0_0, "data/processed/point predictions/point_predictions_arima1_0_0.csv", row.names = FALSE)
write.csv(pred_1_1_0, "data/processed/point predictions/point_predictions_arima1_1_0.csv", row.names = FALSE)
write.csv(pred_arima_auto, "data/processed/point predictions/point_predictions_arima_auto.csv", row.names = FALSE)





