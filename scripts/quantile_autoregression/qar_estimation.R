rm(list = ls(all=TRUE))
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(tseries)
library(forecast)
library(quantreg)
#path
file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"

#read file
df <- read.csv(file_path)

#split year and quarters
df <- df |>
  tidyr::separate(dt,into=c("year","quarter"),sep = " ")

df$year <- as.numeric(df$year)
df$quarter <- as.numeric(gsub("Q","",df$quarter))
#training set
df_training <- df[df$year<=2012,]
countries <- unique(df_training$ccode)

filter_df <- function(df, filters) {
  df %>%
    filter(across(all_of(names(filters)), ~ . == filters[[cur_column()]]))
}

fit_qar <- function(df, tau, nlag=1){
  
  #output dataframe for model and prediction
  output <- data.frame(
    country=character(),
    model_gdp=I(list()),
    model_cpi=I(list())
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
    
    #lag data
    data_gdp_lagged <- lapply(1:nlag,function(L) dplyr::lag(data_gdp,n = L))
    names(data_gdp_lagged) <- paste0("lag_",1:nlag)
    
    data_cpi_lagged <- lapply(1:nlag,function(L) dplyr::lag(data_cpi,n = L))
    names(data_cpi_lagged) <- paste0("lag_",1:nlag)
    
    df_gdp <- data.frame(data_gdp,do.call(cbind,data_gdp_lagged))
    names(df_gdp) <- c("y",paste0("lag_",1:nlag))
    df_cpi <- data.frame(data_cpi,do.call(cbind,data_cpi_lagged))
    names(df_cpi) <- c("y",paste0("lag_",1:nlag))
    
    #remoce NAs
    df_gdp <- na.omit(df_gdp)
    df_cpi <- na.omit(df_cpi)
    
    #QAR(p) model with p=nlag
    fit_gdp <- rq(formula = y ~ ., tau = tau, data = df_gdp)
    fit_cpi <- rq(formula = y ~ ., tau = tau, data = df_cpi)
    
    new_row <- data.frame(
      country=country,
      model_gdp=I(list(fit_gdp)),
      model_cpi=I(list(fit_cpi))
    )
    output <- rbind(output,new_row)
  }
  
  return(qar_model_fit=output)
}






