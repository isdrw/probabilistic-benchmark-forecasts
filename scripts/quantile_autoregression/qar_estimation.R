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
df_oecd <- load_and_prepare_oecd_data()

#load and prepare data from file "data/raw/IMF WEO\WEOforecasts_prefilter.parquet"
df_weo <- load_and_prepare_WEO_data()

df_weo_g7 <- df_weo %>% filter(g7 == 1)

#
fit_qar_on_oecd <- function(df, country, tau, target, nlag=1, R=44, n_ahead=4){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(R,nrow(data_by_country))){
    data <- data_by_country[(i-R+1):i,][[target]]
    
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

    #fit QAR model
    fits <- tryCatch({
      fit_qar(data, tau, nlag = !!nlag)
    },error=function(e){
      message("fit failed ", e$message)
      NULL
    })
    
    #null check fits
    if(is.null(fits)){
      next
    }
    
    #extract fitted models for lower bound (l), upper bound (u) and median (m)
    fit_l <- fits$fit_l
    fit_m <- fits$fit_m
    fit_u <- fits$fit_u
    
    #quantile forecast for n_ahead quarters
    last_lags <- tail(data, nlag)
    preds <- tryCatch({
      predict_qar(last_lags, fit_l = fit_l, fit_m = fit_m, fit_u = fit_u, n_ahead = n_ahead)
      },error=function(e){
        message("prediction failed ", e$message)
        NULL
      })
    
    #null check predictions
    if(is.null(preds)){
      next
    }
    
    #extract predicted lower and upper bounds
    preds_l <- preds$pred_l
    preds_u <- preds$pred_u
    
    # horizons (quarterly)
    h_steps <- 1:n_ahead
    horizons <- h_steps / 4
    
    # target quarter and year vectors based on n_ahead forecasts
    tq <- ((end_quarter - 1 + h_steps) %% 4) + 1
    ty <- end_year + ((end_quarter - 1 + h_steps) %/% 4)
    
    # truth values
    truth_values <- sapply(i + h_steps, function(idx) {
      if (idx <= nrow(data_by_country)){
        data_by_country[[target]][idx] 
      }else{
        NA
      }   
    })
    
    # build all rows at once
    out_list[[index]] <- new_pred_row(
      country = rep(country,n_ahead),
      forecast_year = rep(end_year,n_ahead),
      target_year = ty,
      target_quarter = tq,
      horizon = horizons,
      target = rep(target,n_ahead),
      tau = rep(tau,n_ahead),
      lower_bound = preds_l,
      upper_bound = preds_u,
      truth_value = truth_values,
    )
    
    index <- index + 1
    
  }
  predictions <- bind_rows(out_list)
  return(predictions)
}


fit_qar_on_weo <- function(df, country, tau, target, h, nlag=1, R=11){
  
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
    data <- data_by_country[(i-R+1):i,][[target]]
    
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
    
    #fit QAR model
    fits <- tryCatch({
      fit_qar(data, tau, nlag = !!nlag)
    },error=function(e){
      message("fit failed ", e$message)
      NULL
    })
    
    #null check fits
    if(is.null(fits)){
      next
    }
    
    #extract fitted models for lower bound (l), upper bound (u) and median (m)
    fit_l <- fits$fit_l
    fit_m <- fits$fit_m
    fit_u <- fits$fit_u
    
    #quantile forecast for n_ahead quarters
    last_lags <- tail(data, nlag)
    preds <- tryCatch({
      predict_qar(last_lags, fit_l = fit_l, fit_m = fit_m, fit_u = fit_u, n_ahead = 1)
    },error=function(e){
      message("prediction failed ", e$message)
      NULL
    })
    
    #null check predictions
    if(is.null(preds)){
      next
    }
    
    #extract predicted lower and upper bounds
    preds_l <- preds$pred_l
    preds_u <- preds$pred_u
    
    # target quarter and year vectors based on  forecasts
    tq <- (end_quarter + 4 * (h - floor(h)) - 1) %% 4 + 1
    ty <- as.numeric(data_by_country[i,"target_year"])
    
    # truth values
    truth_value <- if (i+1 <= nrow(data_by_country)){
      data_by_country[[target]][i+1] 
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
grid <- crossing(
  country = unique(df_oecd$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("tv_gdp", "tv_cpi")
)

#predict intervals for all combinations 
pred <- grid %>% 
  mutate(
    results = pmap(
      list(country, tau, target),
      ~ fit_qar_on_oecd(df_oecd, ..1, ..2, ..3)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


#truth value within predicted interval?
pred$covered <- pred$truth_value >= pred$lower_bound & pred$truth_value <= pred$upper_bound

#input for calculation of WIS for 50% and 80% prediction intervals
#forecast years 2013 and above and the target gdp cumulated over all g7 countries
lower_bound <- cbind(pred %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(lower_bound),
                     pred %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(lower_bound))
upper_bound <- cbind(pred %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(upper_bound),
                     pred %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(upper_bound))
truth_value <- cbind(pred %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(truth_value),
                     pred %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(truth_value))

#mean Interval scores for 50% and 80% prediction intervals
#forecast years 2013 and above and the target gdp cummulated over all g7 countries
mean(interval_score(truth_value[,1], lower_bound[,1], upper_bound[,1], 0.5), na.rm=TRUE)
mean(interval_score(truth_value[,2], lower_bound[,2], upper_bound[,2], 0.8), na.rm=TRUE)

#mean WIS 
mean(weighted_interval_score(truth_value, lower_bound, upper_bound, c(0.5, 0.8)), na.rm=TRUE)

#check calibration by calculating coverage for 80% prediction intervals, 
#forecast year 2013 and above, cummulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration
mean(pred %>% filter(tau == 0.5, forecast_year >= 2013, target == "tv_gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)
mean(pred %>% filter(tau == 0.8, forecast_year >= 2013, target == "tv_gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)

#save prediction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred, paste0("results/qar_estimation/qar_prediction_", timestamp, ".csv"), row.names = FALSE)


#=================
#prediction on annual data (WEO dataset)
#=================

#create grid 
grid_weo <- crossing(
  country = unique(df_weo_g7$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("tv_gdp", "tv_cpi"),
  horizon = c(0.5, 1.0)
)

#predict intervals for all combinations 
pred_weo <- grid_weo %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ fit_qar_on_weo(df_weo_g7, ..1, ..2, ..3, ..4)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


#truth value within predicted interval?
pred_weo$covered <- pred_weo$truth_value >= pred_weo$lower_bound & pred_weo$truth_value <= pred_weo$upper_bound

#input for calculation of WIS for 50% and 80% prediction intervals
#forecast years 2013 and above and the target gdp cummulated over all g7 countries
lower_bound <- cbind(pred_weo %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(lower_bound),
                     pred_weo %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(lower_bound))
upper_bound <- cbind(pred_weo %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(upper_bound),
                     pred_weo %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(upper_bound))
truth_value <- cbind(pred_weo %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(truth_value),
                     pred_weo %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(truth_value))

#mean Interval scores for 50% and 80% prediction intervals
#forecast years 2013 and above and the target gdp cummulated over all g7 countries
mean(interval_score(truth_value[,1], lower_bound[,1], upper_bound[,1], 0.5), na.rm=TRUE)
mean(interval_score(truth_value[,2], lower_bound[,2], upper_bound[,2], 0.8), na.rm=TRUE)

#mean WIS 
mean(weighted_interval_score(truth_value, lower_bound, upper_bound, c(0.5, 0.8)), na.rm=TRUE)

#check calibration by calculating coverage for 80% prediction intervals, 
#forecast year 2013 and above, cummulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration
mean(pred_weo %>% filter(tau == 0.5, forecast_year >= 2013, target == "tv_gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)
mean(pred_weo %>% filter(tau == 0.8, forecast_year >= 2013, target == "tv_gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0("results/qar_estimation/qar_prediction_annual_", timestamp, ".csv"), row.names = FALSE)


