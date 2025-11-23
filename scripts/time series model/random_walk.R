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


fit_rw <- function(df,target){
  
  #prediction
  predictions <- data.frame(
    country=character(),
    forecast_year=numeric(),
    target_year=numeric(),
    target_quarter=numeric(),
    horizon=numeric(),
    target=character(),
    truth_value=numeric(),
    prediction=numeric()
  )
  
  countries <- unique(df$ccode)
  
  for(country in countries){
    #time series data for each country and target
    data_by_country <- df[df$ccode==country,]
    
    n <- nrow(data_by_country)
    for(i in 2:n){
      last_value <- data_by_country[[target]][i-1]
      if(is.na(last_value)){
        next
      }
      
      tv_end <- min(i + 3, n)
      truth_values <- data_by_country[(i):tv_end, ][[target]]
      #fill with NA for non-existent truth_values
      if(length(truth_values) < 4){
        truth_values <- c(truth_values, rep(NA, 4 - length(truth_values)))
      }
      
      #start/end date of observations 
      start_year <- data_by_country[i-1,"year"]
      start_quarter <- data_by_country[i-1,"quarter"]
      
      #random walk prediction (last observation)
      rw_pred <- rep(last_value,4)
      
      #target dates
      target_quarter_seq <- (start_quarter + 0:3)%%4 + 1
      horizon_seq <- c(0.25,0.5,0.75,1.0)
      target_year_seq <- start_year + floor(start_quarter/4 -0.25 + horizon_seq)
      
      new_row_pred <- data.frame(
        country=rep(country,times=4),
        forecast_year=start_year,
        target_year=target_year_seq,
        target_quarter=target_quarter_seq,
        horizon=horizon_seq,
        target=rep(target,times=4),
        truth_value=truth_values,
        prediction=rw_pred
      )
      
      #append dataframe
      predictions <- dplyr::bind_rows(predictions, new_row_pred)
    }
  }
  return(predictions)
}

predictions <- fit_rw(df,"gdp")
predictions <- rbind(predictions, fit_rw(df,target="cpi"))

write.csv(predictions,"data/processed/point predictions/point_predictions_rw1.csv")
