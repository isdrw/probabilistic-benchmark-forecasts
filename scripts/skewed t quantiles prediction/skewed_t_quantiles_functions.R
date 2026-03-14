#==========================================================
#Functions for skewed_t_quantiles
#==========================================================

#'function fits skewed t distribution on vector x
#'
#'@note Package fGarch required
#'@param x numeric vector of observations
#'@param mean numeric value of default location parameter (0) to be returned if fit fails
#'@param sd numeric value of default scale parameter (1) to be returned if fit fails
#'@param nu numeric value of default shape (10 df) parameter to be returned if fit fails
#'@param xi numeric value of default skewness (1.5)
fit_skewed_t_distribution <- function(x, mean = 0, sd = 1, nu = 10, xi = 1.5){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean) < 5 || sd(x_clean)==0){
    #return default parameters
    return(list(mean = mean, sd = sd, nu = nu, xi = xi))
  }
  
  #Package fGarch required
  fit_skewed_t <- tryCatch(
    fGarch::sstdFit(x_clean),
    error = function(e){
      warning("fitdist failed to fit t distribution: ", conditionMessage(e))
      NULL
    }
  )
  
  if(is.null(fit_skewed_t)){
    return(list(mean = mean, sd = sd, nu = nu, xi = xi))
  }
  
  return(list(
    mean = fit_skewed_t$estimate["mean"],
    sd = fit_skewed_t$estimate["sd"],
    nu = fit_skewed_t$estimate["nu"],
    xi = fit_skewed_t$estimate["xi"]
  )
  )
}


#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = skewed t Quantiles
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
fit_skewed_t <- function(df, country, tau, target, h, R=11, fit_mean=FALSE){
  
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
    fit <- fit_skewed_t_distribution(err_set)
    
    #default mean
    fitted_mean <- 0
    
    #if mean estimated as well
    if(fit_mean){
      fitted_mean <- fit$mean
    }
    
    #extract parameters
    fitted_sd <- fit$sd
    fitted_nu <- fit$nu
    fitted_xi <- fit$xi
    
    #quantiles of fitted skewed t distribution
    q_l <- fGarch::qsstd((1-tau)/2, mean = fitted_mean, sd = fitted_sd, nu = fitted_nu, xi = fitted_xi)
    q_u <- fGarch::qsstd((1+tau)/2, mean = fitted_mean, sd = fitted_sd, nu = fitted_nu, xi = fitted_xi)
    
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