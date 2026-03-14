#==========================================================
#Functions for the unconditional quantiles predictions
#==========================================================


#'compute prediction interval for method unconditional quantiles (observation based)
#'@description
#'function computes prediction interval for given set of observations and a level tau by
#'returning the respective unconditional quantiles
#'
#'@param obs numeric vector of observations
#'@param tau numeric value of tau 
#'@param n_ahead numeric value for number of predictions 
#'
#'@return lower and upper bound of predictions interval 
#'\describe{
#'   \item{pred_l}{Lower bound prediction}
#'   \item{pred_u}{Median prediction}
#' }
unconditional_quantiles <- function(obs, tau, n_ahead = 4) {
  #check for valid tau
  if (!is.numeric(tau) || tau <= 0 || tau >= 1) {
    stop("tau must be between 0 and 1")
  }
  
  #replace NAs with median 
  obs[is.na(obs)] <- median(obs,na.rm=TRUE)
  
  #prediction
  pred_l <- quantile(obs, probs = (1-tau)/2, type=7, na.rm=TRUE)
  pred_u <- quantile(obs, probs = (1+tau)/2, type=7, na.rm=TRUE)
  
  #same quantile prediction for all horizons
  preds_l <- rep(pred_l, n_ahead)
  preds_u <- rep(pred_u, n_ahead)
  
  return(list(
    preds_l = preds_l, 
    preds_u = preds_u
  ))
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
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon = 1.0
#'
pred_unc_quantiles <- function(df, country, tau, target, h = 1.0, nlag=1){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(1,nrow(data_by_country)-1)){
    data <- data_by_country[1:i,][[paste0("tv_", target)]]
    
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
    
    #prediction
    preds <- unconditional_quantiles(data, tau, n_ahead = 1)
    preds_l <- preds$preds_l
    preds_u <- preds$preds_u
    
    # truth values
    truth_value <- if (i+1 <= nrow(data_by_country)){
      data_by_country[[paste0("tv_",target)]][i+1] 
    }else{
      NA
    } 
    
    # build all rows at once
    out_list[[index]] <- new_pred_row(
      country = country,
      forecast_year = end_year,
      target_year = NA_real_,
      horizon = h,
      target = target,
      tau = tau,
      lower_bound = preds_l,
      upper_bound = preds_u,
      truth_value = truth_value,
    )
    
    index <- index + 1
    
  }
  predictions <- bind_rows(out_list)
  return(predictions)
}