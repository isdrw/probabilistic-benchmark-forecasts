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
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)

#filter for G7 countries
df_weo_g7 <- df %>% filter(g7 == 1)

#function to calculate quantiles of fitted normal distribution and prediction interval
fit_gauss <- function(df, country, tau, target, h, R=11, fit_mean=FALSE){
  
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
    
    #fit normal distibution
    fit_n <- fit_normal_distribution(err_set)
    
    #default mean
    fitted_mean <- 0
    
    #if mean estimated as well
    if(fit_mean){
      fitted_mean <- fit_n$estimate["mean"]  
    }
    
    #extract standard deviation
    fitted_sd <- fit_n$estimate["sd"]
    q_l <- qnorm((1-tau)/2,mean = fitted_mean,sd = fitted_sd)
    q_u <- qnorm((1+tau)/2,mean = fitted_mean,sd = fitted_sd)
    
    #append to output dataframe
    pred_l <- last_pred + q_l
    pred_u <- last_pred + q_u
    
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
      ~ fit_gauss(df_weo_g7, ..1, ..2, ..3, ..4)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

#truth value within predicted interval?
pred_weo$covered <- pred_weo$truth_value >= pred_weo$lower_bound & pred_weo$truth_value <= pred_weo$upper_bound

#input for calculation of WIS for 50% and 80% prediction intervals
#forecast years 2013 and above and the target gdp cummulated over all g7 countries
lower_bound <- cbind(pred_weo %>% filter(tau==0.5, target=="gdp", forecast_year>=2013, horizon==0.5) %>% pull(lower_bound),
                     pred_weo %>% filter(tau==0.8, target=="gdp", forecast_year>=2013, horizon==0.5) %>% pull(lower_bound))
upper_bound <- cbind(pred_weo %>% filter(tau==0.5, target=="gdp", forecast_year>=2013, horizon==0.5) %>% pull(upper_bound),
                     pred_weo %>% filter(tau==0.8, target=="gdp", forecast_year>=2013, horizon==0.5) %>% pull(upper_bound))
truth_value <- cbind(pred_weo %>% filter(tau==0.5, target=="gdp", forecast_year>=2013, horizon==0.5) %>% pull(truth_value),
                     pred_weo %>% filter(tau==0.8, target=="gdp", forecast_year>=2013, horizon==0.5) %>% pull(truth_value))

#mean Interval scores for 50% and 80% prediction intervals
#forecast years 2013 and above and the target gdp cummulated over all g7 countries
mean(interval_score(truth_value[,1], lower_bound[,1], upper_bound[,1], 0.5), na.rm=TRUE)
mean(interval_score(truth_value[,2], lower_bound[,2], upper_bound[,2], 0.8), na.rm=TRUE)

#mean WIS 
mean(weighted_interval_score(truth_value, lower_bound, upper_bound, c(0.5, 0.8)), na.rm=TRUE)

#check calibration by calculating coverage for 80% prediction intervals, 
#forecast year 2013 and above, cummulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration
mean(pred_weo %>% filter(tau == 0.5, forecast_year >= 2013, target == "gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)
mean(pred_weo %>% filter(tau == 0.8, forecast_year >= 2013, target == "gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)

#save pred_weoiction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred_weo, paste0("results/gauss_quantiles_prediction/gauss_prediction_", timestamp, ".csv"), row.names = FALSE)


#find best rolling window on training dataset for g7 countries to calculate prediction interval
#based on quantiles of fitted normal distribution
#best R between 1 and 20 is R for which average weighted Interval score over all taus 
#found insignificant differences between rolling windows of 11-16 --> 11 for better comparability 
#to other methods

best_R <- 5
best_score <- Inf
score_list <- list()
for(R in seq(5,16,by=1)){
  cat("current rolling window R", R,"\n")
  #set of taus
  taus <- seq(0.1,0.9,by=0.1)
  #prediction list for each tau
  predictions <- vector("list", length(taus))
  names(predictions) <- paste0("tau_",taus)
  
  #loop over taus
  for(i in seq_along(taus)){
    #calculate prediction interval for tau 
    predictions[[i]] <- calc_all_pred(
      df = df_training,
      countries = countries,
      target_variables = target_variables,
      h = c(0.5, 1.0),
      tau = taus[i],
      R = R,
      fit_mean=FALSE
    )
    predictions[[i]] <- calc_interval_score(predictions[[i]])
  }
  #weighted prediction score
  predictions_weighted <- calc_weighted_IS(predictions)
  average_weighted_IS <- mean(predictions_weighted[[1]]$IS_weighted,na.rm=TRUE)
  score_list <- append(score_list,average_weighted_IS)
  #update if better score is achieved
  if(average_weighted_IS <= best_score){
    best_R <- R
    best_score <- average_weighted_IS
  }
  cat("R =", R, "average WIS =", average_weighted_IS, "\n")
}

#found R=16
best_R
best_score

#calculate prediction intervals for all taus (0.1,...,0.9)
##Predictions were made on holdout dataset (>=2013-R)
##For the first R predictions the rolling window was smaller than R
##Predictions from 2013 onwards are with rolling window R
taus <- seq(0.1,0.9,by=0.1)
predictions <- vector("list", length(taus))
names(predictions) <- paste0("tau_",taus)

#loop over taus
for(i in seq_along(taus)){
  predictions[[i]] <- calc_all_pred(
    df = df_holdout,
    countries = countries,
    target_variables = target_variables,
    h = c(0.5, 1.0),
    tau = taus[i],
    R = 11,
    fit_mean = TRUE
  )
  predictions[[i]] <- calc_interval_score(predictions[[i]])
}

predictions_weighted <- calc_weighted_IS(predictions)

#save prediction
for(i in seq_along(taus)){
  tau <- taus[i]
  
  filename <- paste0("prediction_fitted_mean_g7_tau_",tau,".parquet")
  folder <- "results/gauss_quantiles_prediction/fitted_mean"
  path <- file.path(folder,filename)
  write_parquet(predictions_weighted[[i]],path)
}




