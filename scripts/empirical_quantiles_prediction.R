#load library 
library(arrow)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)

#parameters
##rolling window
R <- 11
##quantiles
tau <-c(0.5,0.8)

#data preparation
##calculating absolute forecast errors
df$abs_err <- abs(df$prediction-df$tv_1)
##countries
countries <- unique(df$country)
##target variables
target_variables <- unique(df$target)


#data spliting
df_training <- df[df$forecast_year <=2012,]
df_holdout <- df[df$forecast_year >= 2013,]

#filter function for dataframe ("ngdp_rpch" for GDP or "pcpi_pch" for Inflation) 
filter_df <- function(df, country, target_variable, h){
  return(df[df$country == country & df$target == target_variable & df$horizon == h,])
}
filter_df(df, countries[1], "ngdp_rpch", 1)


#function to calculate empirical quantiles and prediction interval
calc_pred_interval <- function(df, country, target_variable, R, h, tau){
  #filtered df for given h
  df_filtered <- filter_df(df=df, country=country, target_variable=target_variable,h=h)
  
  #empty output dataframe
  df_quantiles <- data.frame(
    country = character(),
    target_variable = character(),
    target_year = numeric(),
    forecast_year = numeric(),
    horizon = numeric(),
    tau = numeric(),
    abs_err = numeric(),
    abs_err_quantile = numeric(),
    prediction = numeric(),
    lower_point = numeric(),
    upper_point = numeric(),
    stringsAsFactors = FALSE
  )
  
  #loop over each target year
  for(i in seq_along(df_filtered$target_year)){
    #rolling window for each row (for first R-1 rows rolling window is smaller than R)   
    rolling_window = min(c(i,R))
    
    #set of abs prediction errors 
    abs_err_set <- df_filtered$abs_err[(i-rolling_window+1):i]
    
    #replacement of NA
    indexes_na <- which(is.na(abs_err_set))
    abs_err_set[indexes_na] <- abs_err_set[11-indexes_na+1]
    
    #quantiles of set
    quantile <- quantile(abs_err_set, probs=tau, type=7, na.rm=TRUE)
    
    #append to output dataframe
    new_row <- data.frame(
      country = country,
      target_variable = target_variable,
      target_year = df_filtered$target_year[i],
      forecast_year = df_filtered$forecast_year[i],
      horizon = h,
      tau = tau,
      abs_err = df_filtered$abs_err[i],
      abs_err_quantile = quantile,
      prediction = df_filtered$prediction[i],
      lower_point = df_filtered$prediction[i] - quantile,
      upper_point = df_filtered$prediction[i] + quantile,
      stringsAsFactors = FALSE
    )
    df_quantiles <- rbind(df_quantiles, new_row)
  }
  
  return(df_quantiles)
}

#loop function to calculate all intervals for given sets of countries, target_variables, h, tau
calc_all_pred <- function(df, countries, target_variables, h, tau, R){
  #output dataframe
  df_output <- data.frame(
    country = character(),
    target_variable = character(),
    target_year = numeric(),
    forecast_year = numeric(),
    horizon = numeric(),
    tau = numeric(),
    abs_err = numeric(),
    abs_err_quantile = numeric(),
    prediction = numeric(),
    lower_point = numeric(),
    upper_point = numeric(),
    stringsAsFactors = FALSE
  )
  
#TODO:PAVA algorithm function
pava_correction <- function(x, tolerance=1e-12){
  n <- length(x)
  
  repeat{
    violations_found <- FALSE
    for(i in seq(n-1)){
      if(x[i] - x[i+1] > tolerance){
        x[c(i,i+1)] <- mean(c(x[i],x[i+1]))
        violations_found <- TRUE
      }
    }
    if(violations_found == FALSE){
      break
    }
  }
  return(x)
}


  #loop over all combinations of variables
  for(country in countries){
    for (target_variable in target_variables) {
      for(horizon in h){
        new_row <- calc_pred_interval(
          df = df,
          country = country,
          target_variable = target_variable,
          R = R,
          h = horizon,
          tau = tau
        )
        df_output <- rbind(df_output, new_row)
      }
    }
  }
  
  return(df_output)
}



calc_all_pred(
  df = df_training,
  countries = countries,
  target_variables = target_variables,
  h = c(0.5, 1.0),
  tau = tau,
  R = R
)



