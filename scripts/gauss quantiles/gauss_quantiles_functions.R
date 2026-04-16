#==========================================================
#Functions for skewed_t_quantiles
#==========================================================

#'function fits normal distribution on vector x
#'
#'@param x numeric vector of observations
#'@param mean numeric value of default mean (0) to be returned if fit fails
#'#'@param sd numeric value of default standard deviation (1) to be returned if fit fails
fit_normal_distribution <- function(x,mean = 0,sd = 1){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean)<2 || sd(x_clean)==0){
    #return standard normal distribution
    return(list(estimate=c(mean = mean, sd = sd)))
  }
  
  fit_n <- tryCatch(
    fitdist(x_clean,distr = "norm"),
    error = function(e){
      warning("fitdist failed to fit normal distribution: ", conditionMessage(e))
      #return standard normal distribution
      return(list(estimate=c(mean = mean, sd = sd)))
    }
  )
  
  return(fit_n)
}


#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = normal Quantiles
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon (0.0, 0.5, 1.0, 1.5)
#'@param R = 11 size of rolling window
#'@param fit_mean boolean, TRUE if mean should be fitted to forecast errors
#'@param unbiased_sd FALSE if ML Variance estimator should be used
#'
fit_gauss <- function(df, country, tau, target, h, R=11, fit_mean=FALSE, unbiased_sd = FALSE){
  
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
    
    #replacement of NA with median (only relevant for year 2025)
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
    fit_n <- fit_normal_distribution(err_set)
    
    #default mean
    fitted_mean <- 0
    
    #if mean estimated as well
    if(fit_mean){
      fitted_mean <- fit_n$estimate["mean"]  
    }
    
    #extract standard deviation
    fitted_sd <- fit_n$estimate["sd"]
    if(unbiased_sd){
      fitted_sd <- sd(err_set) 
    }
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
      target_quarter = target_quarter_end,
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