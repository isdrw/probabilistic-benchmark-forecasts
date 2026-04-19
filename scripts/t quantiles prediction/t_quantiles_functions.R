#==========================================================
#Functions for t_quantiles
#==========================================================

#'!!! Generative AI; level = low --> debugging
#'function fits t distribution on vector x
#'
#'@param x numeric vector of observations
#'@param mean numeric value of default mean (0) to be returned if fit fails
#'@param sd numeric value of default standard deviation (1) to be returned if fit fails
#'@param df numeric value of default degree of freedom (10 for moderate heavy tails)
fit_t_distribution <- function(x,mean = 0, sd = 1, df = 10){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean)<3 || sd(x_clean)==0){
    #return standard t distribution
    return(list(mean = mean, sd = sd, df = df))
  }
  
  #start list for optimizer 
  start_vals <- list(
    m = mean(x_clean),
    s = sd(x_clean),
    df = 10
  )
  
  fit_t <- tryCatch(
    MASS::fitdistr(x_clean, densfun = "t", start = start_vals),
    error = function(e){
      warning("fitdist failed to fit t distribution: ", conditionMessage(e))
      NULL
    }
  )
  
  if(is.null(fit_t)){
    return(list(mean = mean, sd = sd, df = df))
  }
  
  return(list(
    mean = fit_t$estimate["m"],
    sd = fit_t$estimate["s"],
    df = fit_t$estimate["df"]
  )
  )
}


#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = t Quantiles
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon (0.0, 0.5, 1.0, 1.5)
#'@param R = 11 size of rolling window
#'@param fit_mean boolean, TRUE if mean should be fitted to forecast errors
#'
fit_t_student <- function(df, country, tau, target, h, R=11, fit_mean=FALSE){
  
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
    
    #replacement of NA with median (only relevant for years 2025)
    err_set[is.na(err_set)] <-  median(err_set)
    
    #forecast_year of i+1
    forecast_year_end <- data_by_country[i+1,][["forecast_year"]]
    
    #forecast_quarter of i+1
    forecast_quarter_end <- data_by_country[i+1,][["forecast_quarter"]]
    
    #target_year of i+1
    target_year_end <- data_by_country[i+1,][["target_year"]]
    
    #target_quarter of i+1
    target_quarter_end <- (forecast_quarter_end - 1 + 4*h) %% 4 + 1
    
    #prediction of i+1
    last_pred <- data_by_country[i+1,][[paste0("pred_", target)]]
    
    #truth value of i+1
    truth_value <- data_by_country[i+1,][[paste0("tv_", target)]]
    
    #fit normal distibution
    fit <- fit_t_distribution(err_set)
    
    #default mean
    fitted_mean <- 0
    
    #if mean estimated as well
    if(fit_mean){
      fitted_mean <- fit$mean
    }
    
    #according to Harvey, Newbold2003 small df
    #represents kurtosis of forecast errors best
    #--> cap df between 3 and 10
    fitted_df <- fit$df  
    if(fitted_df > 10){
      fitted_df <- 10
    }
    if(fitted_df < 3){
      fitted_df <- 3
    }
    
    #extract standard deviation
    fitted_sd <- fit$sd
    
    #quantiles of fitted t distribution
    q_l <- fitted_mean + fitted_sd * qt((1-tau)/2, df = fitted_df)
    q_u <- fitted_mean + fitted_sd * qt((1+tau)/2, df = fitted_df)
    
    #append to output dataframe
    pred_l <- last_pred + q_l
    pred_u <- last_pred + q_u
    
    #new row
    out_list[[index]] <- new_pred_row(
      country = as.character(country),
      forecast_year = as.numeric(forecast_year_end),
      target_year = as.numeric(target_year_end),
      target_quarter = as.numeric(target_quarter_end),
      target = as.character(target),
      horizon = as.numeric(h),
      tau = as.numeric(tau),
      lower_bound = as.numeric(pred_l),
      upper_bound = as.numeric(pred_u),
      truth_value = as.numeric(truth_value),
      prediction = as.numeric(last_pred)
    )
    
    index <- index + 1
  }
  
  predictions <- bind_rows(out_list)
  
  return(predictions)
}