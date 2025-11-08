rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(nortest)
library(fitdistrplus)
library(forecast)
library(quantreg)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)

#prediction errors
df$err <- df$prediction-df$tv_1

#training dataset
df_training <- df[df$forecast_year<=2012&df$g7==1,]
df_holdout <- df[df$forecast_year>=2013-16&df$g7==1,]


#Linear Quantile regression for
countries <- unique(df_training$country)
target_variables <- unique(df_training$target)


fit_iqr <- data.frame(
  country=character(),
  target=character(),
  horizon=numeric(),
  tau=numeric(),
  lqr=I(list())
)


fit_iqr <- df_training %>%
  group_by(country,target,horizon) %>%
  group_map(function(.x,.y){
    y_t <- .x$tv_1
    y_t_hat <- .x$prediction
    
    rows <- lapply(taus,function(tau){
      fit <- rq(y_t ~ y_t_hat,tau = tau)
      new_row <- data.frame(
        country=.y$country,
        target=.y$target,
        horizon=.y$horizon,
        tau=tau,
        iqr=I(list(fit))
      )
    })
    bind_rows(rows)
  })



fit_lqr <- function(df, tau, target, R=11){
  
  #prediction dataframe
  predictions <- data.frame(
    country=character(),
    forecast_year=numeric(),
    target_year=numeric(),
    target_quarter=numeric(),
    horizon=numeric(),
    target=character(),
    tau=numeric(),
    pred_quantile=numeric()
  )
  
  for(country in countries){
    for(h in c(0.5,1.0)){
      data_by_country <- df[df$country==country & df$target==target & df$horizon==h,]
      for(i in seq(11,nrow(data_by_country))){
        #predicted value vector
        data_pred <- data_by_country[(i-11+1):i,"prediction"]
        #truth value vector
        data_tv1 <- data_by_country[(i-11+1):i,"tv_1"]
        
        forecast_year_start <- data_by_country[(i-11+1),"forecast_year"]
        forecast_year_end <- data_by_country[i,"forecast_year"]
        
        target_year_start <- data_by_country[(i-11+1),"target_year"]
        target_year_end <- data_by_country[i,"target_year"]
        
        #skip if only NAs
        if(all(is.na(data_pred)) || is.null(data_pred) || 
           all(is.na(data_tv1)) || is.null(data_tv1)){
          message("no valid data for ", country, " between ", 
                  forecast_year_start, " and ", forecast_year_end)
          next
        }
        
        #fit lqr model 
        fit <- tryCatch({
          rq(formula = data_tv1 ~ data_pred, tau = tau)
        },error=function(e){
          message("Fit failed for ", country, " (", 
                  forecast_year_start, "–", forecast_year_end, "): ", e$message)
          NULL
        })
        
        if(is.null(fit)){
          next
        }
        
        #prediction of quantile based on last point forecast
        last_pred <- tail(data_pred, 1)
        new_data <- data.frame(data_pred = last_pred)
        pred_q <- as.numeric(predict(fit, newdata = new_data))
        
        new_row <- data.frame(
          country = country,
          forecast_year = forecast_year_end,
          target_year = target_year_end,
          horizon = h,
          target = target,
          tau = tau,
          pred_quantile = pred_q,
          stringsAsFactors = FALSE
        )
        
        predictions <- rbind(predictions, new_row)
      }
    }
    
  }
  return(predictions)
}

pred <- fit_lqr(df_training,0.6,target="ngdp_rpch")
