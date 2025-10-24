rm(list = ls(all=TRUE))
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(tseries)
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

#Fit AR(1) on data for each country and target
fit_ar_1 <- data.frame(
  country=character(),
  ar_1_gdp=numeric(),
  intercept_gdp=numeric(),
  aic_gdp=numeric(),
  ar_1_cpi=numeric(),
  intercept_cpi=numeric(),
  aic_cpi=numeric()
)
predictions <- data.frame(
  country=character(),
  year=numeric(),
  quarter=numeric(),
  target=character(),
  prediction=numeric()
)
for(country in countries){
  #time series data for each country and target
  data_gdp <- df_training[df_training$ccode==country,"gdp"]
  data_cpi <- df_training[df_training$ccode==country,"cpi"]
  #start date of observations (all start in the second quarter)
  start_date <- c(min(df_training[df_training$ccode==country,"year"]),2)
  
  #replace NAs with mean 
  data_gdp[is.na(data_gdp)] <- mean(data_gdp,na.rm=TRUE)
  data_cpi[is.na(data_cpi)] <- mean(data_cpi,na.rm=TRUE)
  
  #convert to time series object
  ts_data_gdp <- ts(data_gdp,start = start_date,frequency = 4)
  ts_data_cpi <- ts(data_cpi,start = start_date,frequency = 4)
  
  #simple AR(1) model fit
  fit_gdp <- arima(ts_data_gdp,order = c(1,0,0))
  fit_cpi <- arima(ts_data_cpi,order = c(1,0,0))
  
  new_row <- data.frame(
    country=country,
    ar_1_gdp=coef(fit_gdp)[1],
    intercept_gdp=coef(fit_gdp)[2],
    aic_gdp=fit_gdp$aic,
    ar_1_cpi=coef(fit_cpi)[1],
    intercept_cpi=coef(fit_cpi)[2],
    aic_cpi=fit_cpi$aic
  )
  fit_ar_1 <- rbind(fit_ar_1,new_row)
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


