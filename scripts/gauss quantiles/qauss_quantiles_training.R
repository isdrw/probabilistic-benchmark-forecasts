rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(nortest)
library(fitdistrplus)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)

#prediction errors
df$err <- df$prediction-df$tv_1

#training dataset
df_training <- df[df$forecast_year<=2012&df$g7==1,]

countries <- unique(df_training$country)
target_variables <- unique(df_training$target)
R <- 11

fit_normal_distribution <- function(x,default_mean=0,default_sd=1){
  x_clean <- x[!is.na(x)]
  if(length(x_clean)>1){
    fit_n <- fitdist(x_clean,distr = "norm")  
  }else{
    fit_n <- list(estimate=c(mean=default_mean,sd=default_sd))
  }
  return(fit_n)
}

#filter function for dataframe
filter_df <- function(df, filters) {
  df %>%
    filter(across(all_of(names(filters)), ~ . == filters[[cur_column()]]))
}

#function to calculate quantiles of fitted normal distribution and prediction interval
calc_pred_interval <- function(df, country, target_variable, R, h, tau){
  #filtered df for given h
  df_filtered <- filter_df(
    df,
    list(
      country=country,
      target=target_variable,
      horizon=h
    )
  )
  
  #empty output dataframe
  df_quantiles <- data.frame(
    country = character(),
    target_variable = character(),
    target_year = numeric(),
    forecast_year = numeric(),
    horizon = numeric(),
    tau = numeric(),
    err = numeric(),
    err_quantile_l = numeric(),
    err_quantile_u = numeric(),
    truth_value_1 = numeric(),
    prediction = numeric(),
    lower_point = numeric(),
    upper_point = numeric(),
    tv_in_interval = logical(),
    stringsAsFactors = FALSE
  )
  
  #loop over each target year
  for(i in seq_along(df_filtered$target_year)){
    #rolling window for each row (for first R-1 rows rolling window is smaller than R)   
    rolling_window = min(c(i,R))
    
    #set of abs prediction errors 
    err_set <- df_filtered$err[(i-rolling_window+1):i]
    
    #replacement of NA
    indexes_na <- which(is.na(err_set))
    err_set[indexes_na] <- err_set[R-indexes_na+1]
    
    #quantiles of set
    fit_n <- fit_normal_distribution(err_set)
    fitted_mean <- fit_n$estimate["mean"]
    fitted_sd <- fit_n$estimate["sd"]
    q_l <- qnorm((1-tau)/2,mean=fitted_mean,sd=fitted_sd)
    q_u <- qnorm((1+tau)/2,mean=fitted_mean,sd=fitted_sd)
    
    #append to output dataframe
    lower_point <- df_filtered$prediction[i] + q_l
    upper_point <- df_filtered$prediction[i] + q_u
    new_row <- data.frame(
      country = country,
      target_variable = target_variable,
      target_year = df_filtered$target_year[i],
      forecast_year = df_filtered$forecast_year[i],
      horizon = h,
      tau = tau,
      err = df_filtered$err[i],
      err_quantile_l = q_l,
      err_quantile_u = q_u,
      truth_value_1 = df_filtered$tv_1[i],
      prediction = df_filtered$prediction[i],
      lower_point = lower_point,
      upper_point = upper_point,
      tv_in_interval = (df_filtered$tv_1[i] >= lower_point) & (df_filtered$tv_1[i] <= upper_point),
      stringsAsFactors = FALSE
    )
    df_quantiles <- rbind(df_quantiles, new_row)
  }
  
  return(df_quantiles)
}

calc_all_pred <- function(df, countries, target_variables, h, tau, R){
  #output dataframe
  df_output <- data.frame(
    country = character(),
    target_variable = character(),
    target_year = numeric(),
    forecast_year = numeric(),
    horizon = numeric(),
    tau = numeric(),
    err = numeric(),
    err_quantile_l = numeric(),
    err_quantile_u = numeric(),
    truth_value_1 = numeric(),
    prediction = numeric(),
    lower_point = numeric(),
    upper_point = numeric(),
    tv_in_interval = logical(),
    stringsAsFactors = FALSE
  )
  
  #loop over all combinations of variables
  for(country in countries){
    for (target_variable in target_variables) {
      for(horizon in h){
        new_row <- calc_pred_interval(
          df = df,
          country = country,
          target_variable = target_variable,
          R = R,
          h = horizon,
          tau = tau
        )
        df_output <- rbind(df_output, new_row)
      }
    }
  }
  return(df_output)
}

calc_coverage <- function(df, filters){
  df_filtered <- filter_df(
    df,
    filters
  )
  mean_coverage <- sum(df_filtered$tv_in_interval, na.rm = TRUE) / 
    sum(!is.na(df_filtered$tv_in_interval))
  
  return(mean_coverage)
}

prediction <- calc_all_pred(df_training,countries,target_variables,c(0.5,1.0),0.8,R)
