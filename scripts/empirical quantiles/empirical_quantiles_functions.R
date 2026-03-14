#==========================================================
#Functions for empirical quantiles
#==========================================================


#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = empirical quantiles
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon (0.0, 0.5, 1.0, 1.5)
#'@param R = 11 size of rolling window
#'
fit_emp <- function(df, country, tau, target, h, R = 11){
  
  
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
    
    #target_quarter of i+1 (needed for temporal aggregation to annual data)
    target_quarter_end <- (forecast_quarter_end - 1 + 4*h) %% 4 + 1
    
    #prediction of i+1
    last_pred <- data_by_country[i+1,][[paste0("pred_", target)]]
    
    #truth value of i+1
    truth_value <- data_by_country[i+1,][[paste0("tv_", target)]]
    
    #empirical quantiles of abs error set
    quantile <- quantile(err_set, probs=tau, type=7, na.rm=TRUE)
    
    #append to output dataframe
    pred_l <- last_pred - quantile
    pred_u <- last_pred + quantile
    
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