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

#load and prepare WEO data
df <- load_and_prepare_WEO_data()

#filter for G7 countries
df_weo_g7 <- df %>% filter(g7 == 1)


fit_lqr <- function(df, country, tau, target, h, R=11){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(R,nrow(data_by_country))){
    #predicted value vector
    data_pred <- data_by_country[(i-R+1):i,][[paste0("pred_", target)]]
    #truth value vector
    data_tv1 <- data_by_country[(i-R+1):i,][[paste0("tv_", target)]]

    
    #start and end date of rolling window
    forecast_year_start <- data_by_country[(i-R+1),"forecast_year"]
    forecast_year_end <- data_by_country[i,"forecast_year"]
    
    #date of target year
    target_year_end <- data_by_country[i,"target_year"]
    
    #skip if only NAs
    if(all(is.na(data_pred)) || is.null(data_pred) || 
       all(is.na(data_tv1)) || is.null(data_tv1)){
      message("no valid data for ", country, " between ", 
              forecast_year_start, " and ", forecast_year_end)
      next
    }
    
    #fit lqr model 
    fit_l <- tryCatch({
      rq(formula = data_tv1 ~ data_pred, tau = (1-tau)/2)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    fit_u <- tryCatch({
      rq(formula = data_tv1 ~ data_pred, tau = (1+tau)/2)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    if(is.null(fit_l)||is.null(fit_u)){
      next
    }
    
    #prediction of quantile based on last point forecast
    last_pred <- tail(data_pred, 1)
    new_data <- data.frame(data_pred = last_pred)
    pred_l <- as.numeric(predict(fit_l, newdata = new_data))
    pred_u <- as.numeric(predict(fit_u, newdata = new_data))
    
    # truth values
    truth_value <- if (i+1 <= nrow(data_by_country)){
      data_by_country[[paste0("tv_", target)]][i+1] 
    }else{
      NA
    } 
    
    out_list[[index]] <- data.frame(
      country = country,
      forecast_year = forecast_year_end,
      target_year = target_year_end,
      horizon = h,
      target = target,
      tau = tau,
      lower_bound = pred_l,
      upper_bound = pred_u,
      truth_value = truth_value,
      stringsAsFactors = FALSE
    )
    
    index <- index + 1
  }
  
  predictions <- bind_rows(out_list)
  
  return(predictions)
}


pred <- init_output_df()

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
      ~ fit_lqr(df_weo_g7, ..1, ..2, ..3, ..4)
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
write.csv(pred_weo, paste0("results/unconditional_quantiles/unconditional_quantiles_annual_", timestamp, ".csv"), row.names = FALSE)
