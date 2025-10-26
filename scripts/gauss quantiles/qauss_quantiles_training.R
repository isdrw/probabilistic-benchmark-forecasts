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
df_holdout <- df[df$forecast_year>=2013-16&df$g7==1,]

countries <- unique(df_holdout$country)
target_variables <- unique(df_holdout$target)


fit_normal_distribution <- function(x,default_mean=0,default_sd=1){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean)<2 || sd(x_clean)==0){
    #return standard normal distribution
    return(list(estimate=c(mean=default_mean,sd=default_sd)))
  }
  
  fit_n <- tryCatch(
    fitdist(x_clean,distr = "norm"),
    error = function(e){
      warning("fitdist failed to fit normal distribution: ", conditionMessage(e))
      #return standard normal distribution
      return(list(estimate=c(mean=default_mean,sd=default_sd)))
    }
  )
  
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

#function to calculate interval score for given prediction
calc_interval_score <- function(df){
  u <- df$upper_point
  l <- df$lower_point
  y <- df$truth_value_1
  tau <- df$tau
  df$IS <- (u-l) + 2/(1-tau)*(l-y)*(y<l) + 2/(1-tau)*(y-u)*(y>u)
  return(df)
}

#function to calculate weighted interval score 
calc_weighted_IS <- function(df_set) {
  k <- length(df_set)
  
  weighted_score <- Reduce(
    `+`,
    lapply(df_set, function(df) {
      tau <- unique(df$tau)
      weight <- (1 - tau) / k
      weight * df$IS
    })
  )
  
  df_set <- lapply(df_set, function(df) {
    df$IS_weighted <- weighted_score
    df
  })
  
  return(df_set)
}

#find best rolling window on training dataset for g7 countries to calculate prediction interval
#based on quantiles of fitted normal distribution
#best R between 1 and 20 is R for which average weighted Interval score over all taus and 
best_R <- 3
best_score <- Inf
score_list <- list()
for(R in seq(3,16,by=1)){
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
      R = R
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
    R = 16
  )
  predictions[[i]] <- calc_interval_score(predictions[[i]])
}

predictions_weighted <- calc_weighted_IS(predictions)

#save prediction
for(i in seq_along(taus)){
  tau <- taus[i]
  
  filename <- paste0("prediction_g7_tau_",tau,".parquet")
  folder <- "results/gauss_quantiles_prediction"
  path <- file.path(folder,filename)
  write_parquet(predictions_weighted[[i]],path)
}




