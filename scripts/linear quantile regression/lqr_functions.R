#==========================================================
#Functions for Linear Quantile Regression
#==========================================================


#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = Linear Quantile Regression (expanding window)
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon (0.0, 0.5, 1.0, 1.5)
#'
fit_lqr <- function(df, country, tau, target, h){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(1,nrow(data_by_country)-1)){
    #predicted value vector
    data_pred <- data_by_country[1:i,][[paste0("pred_", target)]]
    
    #last prediction and truth value of point after expanding window
    last_pred <- as.numeric(data_by_country[i+1,][[paste0("pred_", target)]])
    truth_value <- as.numeric(data_by_country[[paste0("tv_", target)]][i+1])
    
    #truth value vector
    data_tv1 <- data_by_country[1:i,][[paste0("tv_", target)]]
    
    #start date of expanding window
    forecast_year_start <- data_by_country[1,"forecast_year"]
    
    #forecast year of point after expanding window for prediction
    forecast_year_end <- data_by_country[i+1,"forecast_year"]
    
    #forecast quarter of point after expanding window for prediction
    forecast_quarter_end <- as.numeric(data_by_country[i+1,"forecast_quarter"])
    
    #target year of point after expanding window for prediction
    target_year_end <- data_by_country[i+1,"target_year"]
    
    #target quarter of point after expanding window for prediction
    target_quarter_end <- (forecast_quarter_end + 4 * h - 1) %% 4 +1
    
    #skip if only NAs
    if(all(is.na(data_pred)) || is.null(data_pred) || 
       all(is.na(data_tv1)) || is.null(data_tv1)){
      message("no valid data for ", country, " between ", 
              forecast_year_start, " and ", forecast_year_end)
      next
    }
    
    #fit lqr model 
    fit_l <- tryCatch({
      rq(formula = data_tv1 ~ data_pred, tau = (1-tau)/2)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    fit_u <- tryCatch({
      rq(formula = data_tv1 ~ data_pred, tau = (1+tau)/2)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    if(is.null(fit_l)||is.null(fit_u)){
      next
    }
    
    #prediction of quantile based on last point forecast
    new_data <- data.frame(data_pred = last_pred)
    pred_l <- as.numeric(predict(fit_l, newdata = new_data))
    pred_u <- as.numeric(predict(fit_u, newdata = new_data))
    
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


#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = Linear Quantile Regression (expanding window) 
#'Autoregression of Errors
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon (0.0, 0.5, 1.0, 1.5)
#'
fit_lqr_err <- function(df, country, tau, target, h){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(2,nrow(data_by_country)-1)){
    #set of abs prediction errors 
    err_set <- data_by_country[2:i,][[paste0(target, "_err")]]
    
    #lagged errors
    lagged_err_set <- data_by_country[1:(i-1),][[paste0(target, "_err")]]
    
    #last error (i+1)
    last_err <- data_by_country[i,][[paste0(target, "_err")]]
    
    #last prediction and truth value of point after expanding window
    last_pred <- as.numeric(data_by_country[i+1,][[paste0("pred_", target)]])
    truth_value <- as.numeric(data_by_country[[paste0("tv_", target)]][i+1])
    
    #start date of expanding window
    forecast_year_start <- data_by_country[1,"forecast_year"]
    
    #forecast year of point after expanding window for prediction
    forecast_year_end <- data_by_country[i+1,"forecast_year"]
    
    #forecast quarter of point after expanding window for prediction
    forecast_quarter_end <- as.numeric(data_by_country[i+1,"forecast_quarter"])
    
    #target year of point after expanding window for prediction
    target_year_end <- data_by_country[i+1,"target_year"]
    
    #target quarter of point after expanding window for prediction
    target_quarter_end <- (forecast_quarter_end + 4 * h - 1) %% 4 +1
    
    #skip if only NAs
    if(all(is.na(err_set)) || is.null(err_set)){
      message("no valid data for ", country, " between ", 
              forecast_year_start, " and ", forecast_year_end)
      next
    }
    
    #fit lqr model 
    fit_l <- tryCatch({
      rq(formula = err_set ~ lagged_err_set, tau = (1-tau)/2)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    fit_u <- tryCatch({
      rq(formula = err_set ~ lagged_err_set, tau = (1+tau)/2)
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    if(is.null(fit_l)||is.null(fit_u)){
      next
    }
    
    #prediction of quantile based on last point forecast
    new_data <- data.frame(lagged_err_set = last_err)
    pred_l <- as.numeric(predict(fit_l, newdata = new_data))
    pred_u <- as.numeric(predict(fit_u, newdata = new_data))
    
    lower_bound <- last_pred + pred_l
    upper_bound <- last_pred + pred_u
    #new row
    out_list[[index]] <- new_pred_row(
      country = country,
      forecast_year = forecast_year_end,
      target_year = target_year_end,
      target_quarter = target_quarter_end,
      target = target,
      horizon = h,
      tau = tau,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      truth_value = truth_value,
      prediction = last_pred
    )
    
    index <- index + 1
  }
  
  predictions <- bind_rows(out_list)
  
  return(predictions)
}
