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
df_training <- df[df$g7==1,]
df_holdout <- df[df$forecast_year>=2013-11&df$g7==1,]


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
    lower_bound=numeric(),
    upper_bound=numeric(),
    truth_value=numeric()
  )
  countries <- unique(df$country)
  for(country in countries){
    for(h in c(0.5,1.0)){
      data_by_country <- df[df$country==country & df$target==target & df$horizon==h,]
      for(i in seq(R,nrow(data_by_country))){
        #predicted value vector
        data_pred <- data_by_country[(i-R+1):i,"prediction"]
        #truth value vector
        data_tv1 <- data_by_country[(i-R+1):i,"tv_1"]
        #truth value for i+1
        tv_1 <- data_by_country[i+1,"tv_1"]

        forecast_year_start <- data_by_country[(i-R+1),"forecast_year"]
        forecast_year_end <- data_by_country[i,"forecast_year"]
        
        target_year_start <- data_by_country[(i-R+1),"target_year"]
        target_year_end <- data_by_country[i,"target_year"]
        
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
        last_pred <- tail(data_pred, 1)
        new_data <- data.frame(data_pred = last_pred)
        pred_l <- as.numeric(predict(fit_l, newdata = new_data))
        pred_u <- as.numeric(predict(fit_u, newdata = new_data))
        
        new_row <- data.frame(
          country = country,
          forecast_year = forecast_year_end,
          target_year = target_year_end,
          horizon = h,
          target = target,
          tau = tau,
          lower_bound = pred_l,
          upper_bound = pred_u,
          truth_value = tv_1,
          stringsAsFactors = FALSE
        )
        
        predictions <- rbind(predictions, new_row)
      }
    }
    
  }
  return(predictions)
}

interval_score <- function(truth_value, lower_bound, upper_bound, tau){
  u <- upper_bound
  l <- lower_bound
  y <- truth_value
  IS <- (u-l) + 2/(1-tau)*(l-y)*(y<l) + 2/(1-tau)*(y-u)*(y>u)
  return(IS)
}


weighted_interval_score <- function(IS_lower, tau_lower, IS_upper, tau_upper) {
  # remove NAs
  valid <- !(is.na(IS_lower) & is.na(tau_lower) & is.na(IS_upper) & is.na(tau_upper))
  IS_lower <- IS_lower[valid]
  IS_upper <- IS_upper[valid]
  tau_lower <- tau_lower[valid]
  tau_upper <- tau_upper[valid]
  
  WIS <- 1/2 * ((1 - tau_lower) * IS_lower + (1 - tau_upper) * IS_upper)
  
  return(WIS)
}

pred <- data.frame(
  country=character(),
  forecast_year=numeric(),
  target_year=numeric(),
  target_quarter=numeric(),
  horizon=numeric(),
  target=character(),
  tau=numeric(),
  lower_bound=numeric(),
  upper_bound=numeric(),
  truth_value=numeric()
)

for(target in target_variables){
  for(tau in seq(0.1,0.9,0.1)){
    pred <- rbind(pred,fit_lqr(df_training,tau,target=target,R=8))
  }
}

pred$IS <- interval_score(pred$truth_value,pred$lower_bound,pred$upper_bound,pred$tau)
pred$covered <- pred$truth_value >= pred$lower_bound & pred$truth_value <= pred$upper_bound

IS_lower <- pred[pred$tau==0.5,"IS"]
IS_upper <- pred[pred$tau==0.8,"IS"]
tau_lower <- pred[pred$tau==0.5,"tau"]
tau_upper <- pred[pred$tau==0.8,"tau"]

WIS_5_8 <- weighted_interval_score(IS_lower, tau_lower, IS_upper, tau_upper)
pred$WIS_5_8 <- NA
pred[pred$tau==0.5,"WIS_5_8"] <- WIS_5_8
pred[pred$tau==0.8,"WIS_5_8"] <- WIS_5_8

mean(WIS_5_8,na.rm=TRUE)
mean(pred[pred$tau==0.8&pred$target=="pcpi_pch","WIS_5_8"],na.rm = TRUE)



df_training[df_training$country=="Canada" & df_training$target=="ngdp_rpch" & df_training$horizon==1.0,]
