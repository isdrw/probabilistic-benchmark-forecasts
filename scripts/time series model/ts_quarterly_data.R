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
fit_arima <- function(df,order=c(1,0,0),auto=FALSE){
  
  #prediction
  predictions <- data.frame(
    country=character(),
    forecast_year=numeric(),
    horizon=numeric(),
    target=character(),
    prediction=numeric()
  )
  
  for(country in countries){
    #time series data for each country and target
    data_by_country <- df[df$ccode==country,]
    
    for(i in seq(44,nrow(data_by_country))){
      data_gdp <- data_by_country[(i-44+1):i,"gdp"]
      data_cpi <- data_by_country[(i-44+1):i,"cpi"]
      #start date of observations 
      start_year <- data_by_country[i-44+1,"year"]
      end_year <- data_by_country[i,"year"]
      start_quarter <- data_by_country[i-44+1,"quarter"]
      start_date <- c(start_year,start_quarter)
      
      #skip if only NAs
      if(all(is.na(data_gdp)) || all(is.na(data_cpi)) || 
         is.null(data_gdp) || is.null(data_cpi)){
        message("no valid data for ", country, " between ", start_year, " and ", end_year)
        next
      }
      #replace NAs with mean 
      data_gdp[is.na(data_gdp)] <- mean(data_gdp,na.rm=TRUE)
      data_cpi[is.na(data_cpi)] <- mean(data_cpi,na.rm=TRUE)
      
      #convert to time series object
      ts_data_gdp <- ts(data_gdp,start = start_date,frequency = 4)
      ts_data_cpi <- ts(data_cpi,start = start_date,frequency = 4)
      
      #ARIMA model fit with order=order
      if(auto==FALSE){
        fit_gdp <- arima(ts_data_gdp,order = order)
        fit_cpi <- arima(ts_data_cpi,order = order)  
      }else{
        #fit model with best AIC 
        fit_gdp <- auto.arima(ts_data_gdp,ic = "aic")
        fit_cpi <- auto.arima(ts_data_cpi,ic = "aic")
      }
      
      #predict values for upcoming 1 (4 quarters)
      #second prediction equals 0.5 horizon forecast and 4th prediction equals 1.0 horizon forecast
      prediction_cpi <- NULL
      prediction_gdp <- NULL
      
      tryCatch(
        prediction_gdp <- predict(fit_gdp,n.ahead = 4),
        error=function(e) NULL
      )
      
      tryCatch(
        prediction_cpi <- predict(fit_cpi,n.ahead = 4),
        error=function(e) NULL
      )
      
      if(is.null(prediction_gdp) || is.null(prediction_cpi)){
        message("no valid data for ", country, " between ", start_year, " and ", end_year)
        next
      }
      
      new_row_pred_gdp <- data.frame(
        country=rep(country,times=4),
        forecast_year=end_year,
        horizon=c(0.25,0.5,0.75,1.0),
        target=rep("gdp",times=4),
        prediction=prediction_gdp$pred
      )
      
      new_row_pred_cpi <- data.frame(
        country=rep(country,times=4),
        forecast_year=end_year,
        horizon=c(0.25,0.5,0.75,1.0),
        target=rep("cpi",times=4),
        prediction=prediction_cpi$pred
      )
      
      #append dataframe
      predictions <- bind_rows(predictions, new_row_pred_gdp, new_row_pred_cpi)
      rm(prediction_cpi,prediction_gdp,fit_gdp,fit_cpi,ts_data_cpi,ts_data_gdp)
    }
  }
  return(predictions)
}

predictions <- fit_arima(df,auto = TRUE)






