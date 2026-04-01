#==========================================================
#Functions for QAR
#==========================================================


#'fit quantile autoregressive model QAR(p) and return fit (observation based)
#'
#'@note 
#'@param obs numeric vector of observations
#'@param last_obs numeric value of last observed value 
#'@param tau numeric vector of quantile levels
#'@param nlag numeric value for nlag=p of QAR(p) model; default p=1 
#'
#' 
fit_qar <- function(obs, last_obs, tau = seq(0.05, 0.95, 0.05)[-10], nlag=1) {
  
  #check for sufficient length of obs
  if (length(obs) < nlag + 2) {
    warning("QAR(",nlag,") Model needs at least ", nlag+2, " observations to fit model")
    return(NULL)
  }
  
  #substitute NAs with median
  obs[is.na(obs)] <- median(obs,na.rm=TRUE)
  
  #lag data
  data_lagged <- lapply(1:nlag, function(L) dplyr::lag(obs, n = L))
  names(data_lagged) <- paste0("lag_",1:nlag)
  
  #dataframe for regression
  data_reg <- data.frame(
    y = obs, 
    do.call(cbind, data_lagged)
  )
  
  #remove NAs
  data_reg <- stats::na.omit(data_reg)
  
  #sort tau 
  tau <- sort(unique(tau))
  
  #check for non empty dataframe
  if (nrow(data_reg) == 0) {
    warning("No valid rows after lagging & removing NA.")
    return(NULL)
  }
  
  fit <- tryCatch({
    quantreg::rq(formula = y ~ ., tau = tau, data = data_reg)
  },error=function(e){
    message("QAR fit failed ", e$message)
    NULL
  })
  
  #==================================================
  #prediction
  
  # convert to numeric lag vector
  last_obs <- as.numeric(last_obs)
  
  if (length(last_obs) != nlag) {
    stop("last_obs must have length equal to nlag used in model fitting.")
  }
  
  #prediction vector
  pred_quantiles <- numeric(length(tau))
  
  # current lag state
  lag_vec <- last_obs
  new_data <- as.data.frame(t(lag_vec))
  names(new_data) <- paste0("lag_", 1:nlag)
  
  pred_quantiles <- as.numeric(predict(fit, newdata = new_data))
  
  #ensure monotonicity by sorting quantiles
  pred_quantiles <- sort(pred_quantiles)
  return(pred_quantiles)
}

#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = unconditional quantiles
#'
#'@note Filter for horizon required to fit dataframe structure
#'(observation-based method --> only one step ahead) 
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon = 1.0
#'@param tau vector of confidence levels 
#'@param nlag lagged values to be used (default = 1)
#'@param n_ahead number of values to be forecast (default = 1) (annual)
#'
fit_qar_on_df <- function(df, country, target, h, tau = seq(0.1, 0.9, 0.1), nlag=1, n_ahead=1){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #lower and upper quantile level
  tau <- sort(tau)
  tau_lower <- (1 - tau) / 2
  tau_upper <- (1 + tau) / 2
  tau_all <- sort(unique(c(tau_lower, tau_upper)))
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(2,nrow(data_by_country)-1)){
    data <- data_by_country[1:i,][[paste0("tv_",target)]]
    
    #start and end date of rolling window 
    end_year <- as.numeric(data_by_country[i,"forecast_year"])
    start_year <- as.numeric(data_by_country[1,"forecast_year"])
    end_quarter <- as.numeric(data_by_country[i,"forecast_quarter"])
    start_quarter <- as.numeric(data_by_country[1,"forecast_quarter"])
    
    #skip if only NAs
    if(all(is.na(data)) || is.null(data)){
      message("no valid data for ", country, " between ", start_year, " and ", end_year)
      next
    }
    
    #lagged values of i+1
    last_lags <- tail(data, nlag)
    
    #fit QAR model
    fits <- tryCatch({
      fit_qar(obs = data, last_obs = last_lags, tau = tau_all, nlag = nlag)
    },error=function(e){
      message("fit failed ", e$message)
      NULL
    })
    
    #null check fits
    if(is.null(fits)){
      next
    }
    
    for(j in seq_along(tau)){
      #extract quantiles to get lower and upper bounds 
      #(fit_qar returns vector of quantiles in order of confidence levels)
      lower_bound <- fits[length(tau_all) / 2 - j + 1]
      upper_bound <- fits[length(tau_all) / 2 + j]
      
      # truth values
      truth_value <- data_by_country[i+1,][[paste0("tv_",target)]]
      forecast_year_1 <- as.numeric(data_by_country[i+1,"forecast_year"])
      forecast_quarter_1 <- as.numeric(data_by_country[i+1,"forecast_quarter"])
      
      # build all rows at once
      out_list[[index]] <- new_pred_row(
        country = country,
        forecast_year = forecast_year_1,
        forecast_quarter = forecast_quarter_1,
        target_year = NA,
        horizon = h,
        target = target,
        tau = tau[j],
        lower_bound = lower_bound,
        upper_bound = upper_bound,
        truth_value = truth_value,
      )
      
      index <- index + 1
    }
    
  }
  predictions <- bind_rows(out_list)
  return(predictions)
}
