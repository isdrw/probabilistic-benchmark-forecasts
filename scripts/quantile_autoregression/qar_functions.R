#==========================================================
#Functions for QAR
#==========================================================

#'!!! Generative AI; level = medium --> debugging + dplyr syntax 
#'fit quantile autoregressive model QAR(p) and return fit (observation based)
#'
#'#'@note Filter for horizon required to fit dataframe structure
#'(observation-based method --> only one step ahead) 
#'
#'@param obs numeric vector of observations
#'@param last_obs numeric value of last observed value 
#'@param tau numeric vector of quantile levels
#'@param nlag numeric value for nlag=p of QAR(p) model; default p=1 
#'
#' 
fit_qar <- function(obs, last_obs, tau = seq(0.05, 0.95, 0.05)[-10], nlag = 1, n_ahead = 1) {
  
  # check for sufficient length
  if (length(obs) < nlag + 2) {
    warning("QAR(", nlag, ") Model needs at least ", nlag + 2, " observations")
    return(NULL)
  }
  
  # lagged data
  data_lagged <- lapply(1:nlag, function(L) dplyr::lag(obs, n = L))
  names(data_lagged) <- paste0("lag_", 1:nlag)
  
  data_reg <- data.frame(
    y = obs,
    do.call(cbind, data_lagged)
  )
  
  data_reg <- stats::na.omit(data_reg)
  
  tau <- sort(unique(tau))
  
  if (nrow(data_reg) == 0) {
    warning("No valid rows after lagging & removing NA.")
    return(NULL)
  }
  
  # fit QAR
  fit <- tryCatch({
    quantreg::rq(formula = y ~ ., tau = tau, data = data_reg)
  }, error = function(e) {
    message("QAR fit failed ", e$message)
    NULL
  })
  
  if (is.null(fit)) return(NULL)
  
  #========================================
  # MULTI-STEP PREDICTION
  
  last_obs <- as.numeric(last_obs)
  
  if (length(last_obs) != nlag) {
    stop("last_obs must have length equal to nlag.")
  }
  
  # rows = horizon, cols = quantiles
  pred_matrix <- matrix(NA, nrow = n_ahead, ncol = length(tau))
  colnames(pred_matrix) <- paste0("tau_", tau)
  
  lag_vec <- last_obs
  
  for (h in 1:n_ahead) {
    
    new_data <- as.data.frame(t(lag_vec))
    names(new_data) <- paste0("lag_", 1:nlag)
    
    preds <- as.numeric(predict(fit, newdata = new_data))
    
    # enforce monotonicity
    preds <- sort(preds)
    
    pred_matrix[h, ] <- preds
    
    # recursive update using median
    median_idx <- ceiling(length(preds) / 2)
    next_value <- preds[median_idx]
    
    lag_vec <- c(tail(lag_vec, nlag - 1), next_value)
  }
  
  return(pred_matrix)
}


#'!!! Generative AI; level = low --> debugging 
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = unconditional quantiles
#'
#'@note up to 7 step ahead forecasts (quarterly). Median prediction used as next 
#'lagged value for prediction of next step
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param tau vector of confidence levels 
#'@param nlag lagged values to be used (default = 1)
#'
fit_qar_on_df <- function(df, country, target, tau = seq(0.1, 0.9, 0.1), nlag = 1, max_h = 7){
  
  out_list <- list()
  index <- 1
  
  # necessary quantile levels
  tau <- sort(tau)
  tau_lower <- (1 - tau) / 2
  tau_upper <- (1 + tau) / 2
  tau_all <- sort(unique(c(tau_lower, tau_upper)))
  
  data_by_country <- df %>% 
    filter(country == !!country) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(2, nrow(data_by_country) - 1)){
    
    data <- data_by_country[1:i, ][[paste0("tv_", target)]]
    
    end_year <- as.numeric(data_by_country[i, "forecast_year"])
    end_quarter <- as.numeric(data_by_country[i, "forecast_quarter"])
    
    if(all(is.na(data)) || is.null(data)){
      next
    }
    
    last_lags <- tail(data, nlag)
    
    # multi-step prediction
    fits <- tryCatch({
      fit_qar(
        obs = data,
        last_obs = last_lags,
        tau = tau_all,
        nlag = nlag,
        n_ahead = max_h
      )
    }, error = function(e) NULL)
    
    if(is.null(fits)){
      next
    }
    
    # loop over horizons
    for(h_step in 1:max_h){
      
      if(i + h_step > nrow(data_by_country)){
        break
      }
      
      # time mapping
      target_quarter <- (end_quarter + h_step - 1) %% 4 + 1
      target_year <- end_year + ((end_quarter + h_step - 1) %/% 4)
      
      truth_value <- data_by_country[[paste0("tv_", target)]][i + h_step]
      
      # loop over tau levels
      for(j in seq_along(tau)){
        
        lower_bound <- fits[h_step, length(tau_all)/2 - j + 1]
        upper_bound <- fits[h_step, length(tau_all)/2 + j]
        
        horizon_val <- (h_step - 1) * 0.25
        
        out_list[[index]] <- new_pred_row(
          country = country,
          forecast_year = end_year,
          forecast_quarter = end_quarter,
          target_year = target_year,
          target_quarter = target_quarter,
          horizon = horizon_val,
          target = target,
          tau = tau[j],
          lower_bound = lower_bound,
          upper_bound = upper_bound,
          truth_value = truth_value
        )
        
        index <- index + 1
      }
    }
  }
  
  predictions <- bind_rows(out_list)
  return(predictions)
}
