rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)

#prediction errors
df$abs_err <- abs(df$prediction-df$tv_1)

df_training <- df[df$g7==TRUE,]
target_variables <- unique(df_training$target)

fit_MAE_interval <- function(df, target, R=11){
  
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
        #prediction of i+1
        prediction_1 <- data_by_country[i+1,"prediction"]
        #truth value for i+1
        tv_1 <- data_by_country[i+1,"tv_1"]
        #mean absolute error of rolling window
        MAE <- mean(data_by_country[(i-R+1):i,"abs_err"],na.rm=TRUE)
        
        forecast_year_start <- data_by_country[(i-R+1),"forecast_year"]
        forecast_year_end <- data_by_country[i,"forecast_year"]
        
        target_year_start <- data_by_country[(i-R+1),"target_year"]
        target_year_end <- data_by_country[i,"target_year"]
        
        new_row <- data.frame(
          country = country,
          forecast_year = forecast_year_end,
          target_year = target_year_end,
          horizon = h,
          target = target,
          lower_bound = prediction_1 - MAE,
          upper_bound = prediction_1 + MAE,
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
  pred <- rbind(pred,fit_MAE_interval(df_training, target = target))
}

pred$covered <- pred$truth_value >= pred$lower_bound & pred$truth_value <= pred$upper_bound

mean(pred$covered,na.rm=TRUE)
