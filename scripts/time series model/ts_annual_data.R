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
#fitting of best order according to BIC
fit_arima <- function(df, country, target, R = 44,
                      order = c(1,0,0), auto = FALSE){
  
  out_list <- list()
  index <- 1
  
  data_by_country <- df %>% 
    filter(country == !!country) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(1 + order[1], nrow(data_by_country))){
    
    end_year    <- data_by_country[i, "forecast_year"]
    end_quarter <- data_by_country[i, "forecast_quarter"]
    
    # determine forecast schemes by quarter
    schemes <- switch(
      as.character(end_quarter),
      "3" = list(
        list(n_ahead = 1, horizon = 0.0),
        list(n_ahead = 5, horizon = 1.0)
      ),
      "1" = list(
        list(n_ahead = 3, horizon = 0.5),
        list(n_ahead = 7, horizon = 1.5)
      ),
      NULL
    )
    
    if (is.null(schemes)){
      next
    } 
    
    data <- data_by_country[1:i, ][[target]]
    if (all(is.na(data)) || is.null(data)){
      next
    } 
    
    ts_data <- ts(
      na.omit(data),
      start = c(data_by_country[1,"forecast_year"],
                data_by_country[1,"forecast_quarter"]),
      frequency = 4
    )
    
    fit <- tryCatch({
      if (!auto){
        arima(ts_data, order = order)
      } 
      else{
        auto.arima(ts_data, ic = "bic")
      } 
    }, error = function(e){
      message("fit failed for year, ", end_year, ", country: ", country, ", target: ", target)
      NULL
    })
    
    if (is.null(fit)){
      next
    } 
    
    for (s in schemes) {
      #number of n ahead forecasts and corresponding horizon
      n_ahead <- s$n_ahead
      horizon <- s$horizon
      
      
      pred <- tryCatch({
        predict(fit, n.ahead = n_ahead)$pred
      }, error = function(e) NULL)
      
      if (is.null(pred)) next
      
      origin_index <- 4 * end_year + end_quarter
      h_steps <- 1:n_ahead
      target_index <- origin_index + h_steps
      
      tq <- target_index %% 4
      tq[tq == 0] <- 4
      ty <- floor(target_index / 4)
      ty[tq == 4] <- ty[tq == 4] - 1
      
      tv_end <- min(i + n_ahead, nrow(data_by_country))
      truth_values <- data_by_country[(i+1):tv_end, ][[target]]
      
      if (length(truth_values) < n_ahead) {
        truth_values <- c(
          truth_values,
          rep(NA_real_, n_ahead - length(truth_values))
        )
      }
      
      out_list[[index]] <- new_pred_row(
        country = rep(country, n_ahead),
        forecast_year = rep(end_year, n_ahead),
        forecast_quarter = rep(end_quarter, n_ahead),
        target_year = ty,
        target_quarter = tq,
        horizon = rep(horizon, n_ahead),
        target = rep(target, n_ahead),
        truth_value = truth_values,
        prediction = pred,
        interval = FALSE
      )
      
      index <- index + 1
    }
  }
  
  bind_rows(out_list)
}



#create grid 
grid <- crossing(
  country = unique(df_oecd$country),
  target = c("tv_gdp", "tv_cpi")
)

#fit ARIMA(0,1,0)/RW(1) on quarterly OECD data with rolling window
pred_rw <- grid %>% 
  mutate(
    results = pmap(
      list(country, target),
      ~ fit_arima(df_oecd, ..1, ..2, order = c(0, 1, 0))
    )
  ) %>%
  pull(results) %>%
  bind_rows()

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
write.csv(pred_rw, "data/processed/point predictions/point_predictions_rw.csv", row.names = FALSE)
write.csv(pred_1_0_0, "data/processed/point predictions/point_predictions_arima1_0_0.csv", row.names = FALSE)
write.csv(pred_1_1_0, "data/processed/point predictions/point_predictions_arima1_1_0.csv", row.names = FALSE)
write.csv(pred_arima_auto, "data/processed/point predictions/point_predictions_arima_auto.csv", row.names = FALSE)





