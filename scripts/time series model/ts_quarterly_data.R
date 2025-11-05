rm(list = ls(all=TRUE))
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(tseries)
library(forecast)
#path
file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"

#read file
df <- read.csv(file_path)

#split year and quarters
df <- df |>
  tidyr::separate(dt,into=c("year","quarter"),sep = " ")

df$year <- as.numeric(df$year)
df$quarter <- as.numeric(gsub("Q","",df$quarter))
countries <- unique(df$ccode)

filter_df <- function(df, filters) {
  df %>%
    filter(across(all_of(names(filters)), ~ . == filters[[cur_column()]]))
}

#Function to fit ARIMA model either with given order or with automatic
#fitting of best order according to AIC
fit_arima <- function(df,target,order=c(1,0,0),auto=FALSE){
  
  #prediction
  predictions <- data.frame(
    country=character(),
    forecast_year=numeric(),
    target_year=numeric(),
    target_quarter=numeric(),
    horizon=numeric(),
    target=character(),
    prediction=numeric()
  )
  
  countries <- unique(df$ccode)
  
  for(country in countries){
    #time series data for each country and target
    data_by_country <- df[df$ccode==country,]
    
    for(i in seq(44,nrow(data_by_country))){
      data <- data_by_country[(i-44+1):i,][[target]]
      #start date of observations 
      start_year <- data_by_country[i-44+1,"year"]
      end_year <- data_by_country[i,"year"]
      start_quarter <- data_by_country[i-44+1,"quarter"]
      end_quarter <- data_by_country[i,"quarter"]
      start_date <- c(start_year,start_quarter)
      
      #skip if only NAs
      if(all(is.na(data)) || is.null(data)){
        message("no valid data for ", country, " between ", start_year, " and ", end_year)
        next
      }
      #replace NAs with mean 
      data[is.na(data)] <- mean(data,na.rm=TRUE)
      
      #convert to time series object
      ts_data <- ts(data,start = start_date,frequency = 4)
      
      #ARIMA model fit with order=order
      fit <- tryCatch({
        if(!auto){
          arima(ts_data,order = order)
        }else{
          #fit model with best AIC 
          auto.arima(ts_data,ic = "aic")
        }
      },error=function(e){
        message("Fit failed for ", country, " (", start_year, "–", end_year, "): ", e$message)
        NULL
      })
      
      
      #predict values for upcoming 1 (4 quarters)
      #second prediction equals 0.5 horizon forecast and 4th prediction equals 1.0 horizon forecast
      pred <- tryCatch({
        predict(fit,n.ahead = 4)
      },error=function(e){
        message("Prediction failed for ", country, " (", start_year, "–", end_year, "): ", e$message)
        NULL
      })

      if(is.null(pred)){
        next
      }
      
      target_quarter_seq <- (end_quarter + 0:3)%%4 + 1
      horizon_seq <- c(0.25,0.5,0.75,1.0)
      target_year_seq <- end_year + floor(end_quarter/4 -0.25 + horizon_seq)
      
      new_row_pred <- data.frame(
        country=rep(country,times=4),
        forecast_year=end_year,
        target_year=target_year_seq,
        target_quarter=target_quarter_seq,
        horizon=horizon_seq,
        target=rep(target,times=4),
        prediction=pred$pred
      )
      
      #append dataframe
      predictions <- bind_rows(predictions, new_row_pred)
      rm(pred,fit,ts_data)
    }
  }
  return(predictions)
}

predictions <- fit_arima(df,target = "gdp")






