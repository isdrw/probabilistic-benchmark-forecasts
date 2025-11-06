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

countries <- unique(df$ccode)

filter_df <- function(df, filters) {
  df %>%
    filter(across(all_of(names(filters)), ~ . == filters[[cur_column()]]))
}



fit_qar <- function(df, tau, target, nlag=1, R=44){
  
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
    data_by_country <- df[df$ccode==country,]
    
    for(i in seq(44,nrow(data_by_country))){
      data <- data_by_country[(i-44+1):i,][[target]]
      #start date of observations 
      end_year <- data_by_country[i,"year"]
      start_year <- data_by_country[i-44+1,"year"]
      end_quarter <- data_by_country[i,"quarter"]
      start_quarter <- data_by_country[i-44+1,"quarter"]
      
      #skip if only NAs
      if(all(is.na(data)) || is.null(data)){
        message("no valid data for ", country, " between ", start_year, " and ", end_year)
        next
      }
      #replace NAs with mean 
      data[is.na(data)] <- mean(data,na.rm=TRUE)
      
      #lag data
      data_lagged <- lapply(1:nlag,function(L) dplyr::lag(data,n = L))
      names(data_lagged) <- paste0("lag_",1:nlag)
      
      #dataframe for regression
      data_reg <- data.frame(data,do.call(cbind,data_lagged))
      names(data_reg) <- c("y",paste0("lag_",1:nlag))
      
      #remove NAs
      data_reg <- na.omit(data_reg)
      
      #fit qar(p) model with p=nlag
      fit <- tryCatch({
        rq(formula = y ~ ., tau = tau, data = data_reg)
      },error=function(e){
        message("Fit failed for ", country, " (", start_year, "–", end_year, "): ", e$message)
        NULL
      })
      
      if(is.null(fit)){
        next
      }
      
      #last lagged values of current rolling window
      last_lags <- as.numeric(tail(data_reg[, -1, drop=FALSE], 1))
      
      #quantile forecast for 4 quarters
      preds <- numeric(4)
      for(h in 1:4){
        new_data <- as.data.frame(t(last_lags[1:nlag]))
        names(new_data) <- paste0("lag_", 1:nlag)
        pred <- as.numeric(predict(fit, newdata=new_data))
        preds[h] <- pred
        
        # update lag vector for next iteration
        last_lags <- c(pred, head(last_lags, nlag-1))
      }
      
      #append predictions dataframe with new prediction of quantiles
      for(h in 1:4){
        fq <- end_quarter + h
        fy <- end_year + (fq - 1) %/% 4
        fq <- ((fq - 1) %% 4) + 1
        
        new_row <- data.frame(
          country = country,
          forecast_year = end_year,
          target_year = fy,
          target_quarter = fq,
          horizon = h,
          target = target,
          tau = tau,
          pred_quantile = preds[h],
          stringsAsFactors = FALSE
        )
        predictions <- rbind(predictions, new_row)
      }
    }
  }
  return(predictions)
}


pred <- fit_qar(df,0.9,target="gdp")


