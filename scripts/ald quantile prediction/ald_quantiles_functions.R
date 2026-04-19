#==========================================================
#Functions for Asymmetric Laplace Distribution Quantiles
#==========================================================

#'!!! Generative AI; level = low --> debugging
#'function fits Asymmetric Laplace distribution on vector x
#'
#'@param x numeric vector of observations
fit_ald_distribution <- function(x){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean) < 3 || sd(x_clean)==0){
    #return default parameters
    return(list(mu = 0, sigma = 1, p = 0.5))
  }
  
  fit <- tryCatch(
      ald::mleALD(x_clean),
    error = function(e){
      warning("fitdist failed to fit ALD: ", conditionMessage(e))
      NULL
    }
  )
  
  mu_hat <- fit$par[1]
  sigma_hat <- fit$par[2]
  p <- fit$par[3]
  
  #return estimated parameters
  return(
    list(
      mu = mu_hat,
      sigma = sigma_hat,
      p = p
    )
  )
}

#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = Asymmetric Laplace Distribution quantiles
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon (0.0, 0.5, 1.0, 1.5)
#'@param R = 11 size of rolling window
#'
fit_ald <- function(df, country, tau, target, h, R=11){
  
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
    
    #forecast_quarter of i+1
    forecast_quarter_end <- data_by_country[i+1,][["forecast_quarter"]]
    
    #target_year of i+1
    target_year_end <- data_by_country[i+1,][["target_year"]]
    
    #target_quarter of i+1 (needed for temporal aggregation to annual data)
    target_quarter_end <- (forecast_quarter_end - 1 + 4*h) %% 4 + 1
    
    #prediction of i+1
    last_pred <- data_by_country[i+1,][[paste0("pred_", target)]]
    
    #truth value of i+1
    truth_value <- data_by_country[i+1,][[paste0("tv_", target)]]
    
    #fit normal distribution
    fit <- fit_ald_distribution(err_set)
    
    #extract fitted values
    fitted_mu <- fit$mu
    fitted_sigma <- fit$sigma
    fitted_p <- fit$p
    #cat(paste0("mu_hat: ", fitted_mu, "\nsigma_hat: ", sigma, "\n"))
    #quantiles of fitted ALD distribution
    q_l <- ald::qALD(prob = (1-tau) / 2, mu = fitted_mu, sigma = fitted_sigma, p = fitted_p)
    q_u <- ald::qALD(prob = (1+tau) / 2, mu = fitted_mu, sigma = fitted_sigma, p = fitted_p)
    
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
