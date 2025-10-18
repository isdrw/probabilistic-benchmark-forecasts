#load library 
library(arrow)
library(dplyr)
library(tidyr)
#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)

#parameters
##rolling window
R <- 11
##quantiles
tau <-0.8

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


filter_df <- function(df, filters) {
  df %>%
    filter(across(all_of(names(filters)), ~ . == filters[[cur_column()]]))
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
pava_correction <- function(x, y, increasing=TRUE, tolerance=1e-12){
  na_idx <- is.na(x) | is.na(y)
  x_clean <- x[!na_idx]
  y_clean <- y[!na_idx]
  n <- length(x_clean)
  
  if(n <= 1){
    return(data.frame(x=x,y=y))
  }
  
  repeat{
    violations_found <- FALSE
    for(i in seq_len(n-1)){
      #increasing order
      if(increasing && (x_clean[i] - x_clean[i+1] > tolerance)){
        x_clean[c(i,i+1)] <- mean(c(x_clean[i],x_clean[i+1]))
        y_clean[c(i,i+1)] <- mean(c(y_clean[i],y_clean[i+1]))
        violations_found <- TRUE
      }  
      #decreasing order 
      if(!increasing && (x_clean[i+1] - x_clean[i] > tolerance)){
        x_clean[c(i,i+1)] <- mean(c(x_clean[i],x_clean[i+1]))
        y_clean[c(i,i+1)] <- mean(c(y_clean[i],y_clean[i+1]))
        violations_found <- TRUE
      }
      
    }
    if(violations_found == FALSE){
      break
    }
  }
  x_out <- x
  y_out <- y
  x_out[!na_idx] <- x_clean
  y_out[!na_idx] <- y_clean
  
  return(data.frame(x=x_out,y=y_out))
}

#function to apply PAVA algorithm on dataframe to correct upper point violations and change
#corresponding lower points as well
apply_pava_on_df <- function(df) {
  df %>%
    group_by(country, target_year, target_variable, tau) %>%
    group_modify(~ {
      corrected <- pava_correction(.x$upper_point, .x$lower_point)
      .x$upper_point <- corrected$x
      .x$lower_point <- corrected$y
      .x
    }) %>%
    ungroup()
}

#function to calculate empirical quantiles and prediction interval
calc_pred_interval <- function(df, country, target_variable, R, h, tau){
  #filtered df for given h
  df_filtered <- filter_df(
    df,
    list(
      country=country,
      target=target_variable,
      horizon=h
    )
  )
  
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
  #pava correction of upper and lower interval points
  df_output <- apply_pava_on_df(df_output)
  
  df_output$tv_in_interval <- (df_output$truth_value_1 >= df_output$lower_point) &
    (df_output$truth_value_1 <= df_output$upper_point)
  
  return(df_output)
}


#eval scores implementation Interval Score, Coverage

calc_coverage <- function(df, filters){
  df_filtered <- filter_df(
    df,
    filters
  )
  mean_coverage <- sum(df_filtered$tv_in_interval, na.rm = TRUE) / 
    sum(!is.na(df_filtered$tv_in_interval))
  
  return(mean_coverage)
}

calc_interval_score <- function(df){
  u <- df$upper_point
  l <- df$lower_point
  y <- df$truth_value_1
  tau <- df$tau
  df$IS <- (u-l) + 2/(1-tau)*(l-y)*(y<l) + 2/(1-tau)*(y-u)*(y>u)
  return(df)
}

calc_weighted_IS <- function(df_set) {
  k <- length(df_set)
  
  weighted_score <- Reduce(
    `+`,
    lapply(df_set, function(df) {
      tau <- unique(df$tau)
      weight <- (1 - tau) / k
      weight * df$IS
    })
  )
  
  df_set <- lapply(df_set, function(df) {
    df$IS_weighted <- weighted_score
    df
  })
  
  return(df_set)
}


pred_median <- calc_all_pred(
  df = df_holdout,
  countries = countries,
  target_variables = target_variables,
  h = c(0.5, 1.0),
  tau = 0.5,
  R = R
)

pred_0.8 <- calc_all_pred(
  df = df_holdout,
  countries = countries,
  target_variables = target_variables,
  h = c(0.5, 1.0),
  tau = 0.8,
  R = R
)


calc_coverage(
  pred_0.8[pred_0.8$forecast_year>=2013,],
  list(horizon=0.5)
  )

pred_median <- calc_interval_score(pred_median)
pred_0.8 <- calc_interval_score(pred_0.8)

df_set <- calc_weighted_IS(list(pred_median,pred_0.8))
pred_median <- df_set[[1]]
pred_0.8 <- df_set[[2]]


#save prediction
write_parquet(pred_median,r"(data/processed/pred_median.parquet)")
write_parquet(pred_0.8, r"(data/processed/pred_80%.parquet)")
