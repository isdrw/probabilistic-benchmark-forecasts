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
df_training <- df[df$year<=2012,]
countries <- unique(df_training$ccode)

filter_df <- function(df, filters) {
  df %>%
    filter(across(all_of(names(filters)), ~ . == filters[[cur_column()]]))
}

#Function to fit ARIMA model either with given order or with automatic
#fitting of best order according to AIC
fit_arima <- function(df,order=c(1,0,0),auto=FALSE){
  
  #output dataframe for model and prediction
  output <- data.frame(
    country=character(),
    model_gdp=list(),
    model_cpi=list()
  )
  #prediction
  predictions <- data.frame(
    country=character(),
    year=numeric(),
    quarter=numeric(),
    target=character(),
    prediction=numeric()
  )
  
  for(country in countries){
    #time series data for each country and target
    data_gdp <- df[df$ccode==country,"gdp"]
    data_cpi <- df[df$ccode==country,"cpi"]
    #start date of observations (all start in the second quarter)
    start_date <- c(min(df[df$ccode==country,"year"]),2)
    
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
    
    
    new_row <- data.frame(
      country=country,
      model_gdp=I(list(fit_gdp)),
      model_cpi=I(list(fit_cpi))
    )
    output <- rbind(output,new_row)
    
    #predict values for upcoming 12 years until 2024 starting with q1 2013
    #4Q*12years = 48 predictions
    prediction_gdp <- predict(fit_gdp,n.ahead = 48)
    prediction_cpi <- predict(fit_cpi,n.ahead = 48)
    
    new_row_pred_gdp <- data.frame(
      country=rep(country,times=48),
      year=rep(2013:2024, each = 4),
      quarter=rep(1:4,times=12),
      target=rep("gdp",times=48),
      prediction=prediction_gdp$pred
    )
    
    new_row_pred_cpi <- data.frame(
      country=rep(country,times=48),
      year=rep(2013:2024, each = 4),
      quarter=rep(1:4,times=12),
      target=rep("cpi",times=48),
      prediction=prediction_cpi$pred
    )
    
    #append dataframe
    predictions <- bind_rows(predictions, new_row_pred_gdp, new_row_pred_cpi)
    rm(prediction_cpi,prediction_gdp,fit_gdp,fit_cpi,ts_data_cpi,ts_data_gdp)
  }
  
  return(list(arima_model_fit=output,predictions=predictions))
}

tmp <- fit_arima(df_training,auto = TRUE)
fit_ar<-tmp$arima_model_fit
fit_ar[fit_ar$country=="DEU","model_gdp"]
predictions_12<-tmp$predictions

y_gdp <- df_training$gdp
y_gdp[is.na(y_gdp)] <- mean(y_gdp,na.rm=TRUE)
auto.arima(y_gdp,ic = "aic")
