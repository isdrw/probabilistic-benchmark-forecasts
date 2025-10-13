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
tau <-0.5

#data preparation
##calculating absolute forecast errors
df$abs_err <- abs(df$prediction-df$tv_1)
##countries
countries <- unique(df$country)
##target variables
target_variables <- unique(df$target)


#data spliting
df_training <- df[df$forecast_year <=2012,]
df_holdout <- df[df$forecast_year >= 2013-R,]

#filter function for dataframe ("ngdp_rpch" for GDP or "pcpi_pch" for Inflation) 
filter_df <- function(df, country, target_variable, h){
  return(df[df$country == country & df$target == target_variable & df$horizon == h,])
}


#function to filter df for given values such as 
#filter <- list(country=countries[1], target="ngdp_rpch")
make_mask <- function(df, filters) {
  mask <- rep(TRUE, nrow(df))
  for (v in names(filters)) {
    mask <- mask & (df[[v]] == filters[[v]])
  }
  return(mask)
}


#PAVA algorithm function
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

#function to apply PAVA algorithm on dataframe to correct abs error violations
apply_pava_on_errors <- function(df){
  for(country in countries){
    for(target_year in df$target_year){
      for(target_variable in df$target){
        for(tau in df$tau){
          mask <- make_mask(
            df,
            list(country=country,
                 target_year=target_year,
                 target=target_variable,
                 tau=tau)
            )
          df[mask,"abs_err_quantile"] <- pava_correction(df[mask,"abs_err_quantile"])
          
        }
      }
    }
  }
  return(df)
}

#function to calculate empirical quantiles and prediction interval
calc_pred_interval <- function(df, country, target_variable, R, h, tau){
  #filtered df for given h
  mask <- make_mask(
    df,
    list(
      country=country,
      target=target_variable,
      horizon=h
    )
  )
  df_filtered <- df[mask,]
  
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
    truth_value_1 = numeric(),
    prediction = numeric(),
    lower_point = numeric(),
    upper_point = numeric(),
    tv_in_interval = logical(),
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
    lower_point <- df_filtered$prediction[i] - quantile
    upper_point <- df_filtered$prediction[i] + quantile
    new_row <- data.frame(
      country = country,
      target_variable = target_variable,
      target_year = df_filtered$target_year[i],
      forecast_year = df_filtered$forecast_year[i],
      horizon = h,
      tau = tau,
      abs_err = df_filtered$abs_err[i],
      abs_err_quantile = quantile,
      truth_value_1 = df_filtered$tv_1[i],
      prediction = df_filtered$prediction[i],
      lower_point = lower_point,
      upper_point = upper_point,
      tv_in_interval = (df_filtered$tv_1[i] >= lower_point) & (df_filtered$tv_1[i] <= upper_point),
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
    truth_value_1 = numeric(),
    prediction = numeric(),
    lower_point = numeric(),
    upper_point = numeric(),
    tv_in_interval = logical(),
    stringsAsFactors = FALSE
  )

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
  #pava correction of abs_err_quantiles
  df_output <- apply_pava_on_errors(df_output)
  #update intervals after PAVA
  df_output$lower_point <- df_output$prediction - df_output$abs_err_quantile
  df_output$upper_point <- df_output$prediction + df_output$abs_err_quantile
  
  df_output$tv_in_interval <- (df_output$truth_value_1 >= df_output$lower_point) & (df_output$truth_value_1 <= df_output$upper_point)
  
  return(df_output)
}


#TODO eval scores implementation CRPS, log Score, Interval Score

calc_coverage <- function(x){
  return(x/length(x))
}

pred_median <- calc_all_pred(
  df = df_holdout,
  countries = countries,
  target_variables = target_variables,
  h = c(0.5, 1.0),
  tau = tau,
  R = R
)

#save prediction
write_parquet(pred_median, r"(data/processed/pred_median.parquet)")



