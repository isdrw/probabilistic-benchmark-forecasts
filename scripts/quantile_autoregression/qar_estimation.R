rm(list = ls(all=TRUE))
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(tseries)
library(forecast)
library(quantreg)

#load functions
source("scripts/utilities/utility_functions.R")

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



fit_qar_on_df <- function(df, tau, target, nlag=1, R=44, n_ahead=4){
  
  #prediction dataframe
  predictions <- init_output_df()
  
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

      #fit QAR model
      fits <- tryCatch({
        fit_qar(data, tau, nlag = nlag)
      },error=function(e){
        message("fit failed ", e$message)
        NULL
      })
      
      #null check fits
      if(is.null(fits)){
        next
      }
      
      fit_l <- fits$fit_l
      fit_m <- fits$fit_m
      fit_u <- fits$fit_u
      
      #quantile forecast for 4 quarters
      last_lags <- tail(data, nlag)
      preds <- tryCatch({
        predict_qar(last_lags, fit_l = fit_l, fit_m = fit_m, fit_u = fit_u)
        },error=function(e){
          message("prediction failed ", e$message)
          NULL
        })
      
      if(is.null(preds)){
        next
      }
      
      preds_l <- preds$pred_l
      preds_u <- preds$pred_u
      
      # horizons (quarterly)
      h_steps <- 1:n_ahead
      horizons <- h_steps / 4
      
      # target quarter and year
      tq <- ((end_quarter - 1 + h_steps) %% 4) + 1
      ty <- end_year + ((end_quarter - 1 + h_steps) %/% 4)
      
      # truth values
      truth_values <- sapply(i + h_steps, function(idx) {
        if (idx <= nrow(data_by_country)) data_by_country[[target]][idx] else NA
      })
      
      # build all rows at once
      new_rows <- new_pred_row(
        country = rep(country,n_ahead),
        forecast_year = rep(end_year,n_ahead),
        target_year = ty,
        target_quarter = tq,
        horizon = horizons,
        target = rep(target,n_ahead),
        tau = rep(tau,n_ahead),
        lower_bound = preds_l,
        upper_bound = preds_u,
        truth_value = truth_values,
      )
      
      # append
      predictions <- rbind(predictions, new_rows)
      
    }
  }
  return(predictions)
}


pred <- init_output_df()


for(tau in seq(0.1,0.9,0.1)){
  pred <- rbind(pred,fit_qar_on_df(df,tau = tau,target = "gdp"))
  pred <- rbind(pred,fit_qar_on_df(df,tau = tau,target = "cpi"))
}

pred$covered <- pred$truth_value >= pred$lower_bound & pred$truth_value <= pred$upper_bound

lower_bound <- cbind(pred %>% filter(tau==0.5, target=="gdp", forecast_year>=2013) %>% pull(lower_bound),
                     pred %>% filter(tau==0.8, target=="gdp", forecast_year>=2013) %>% pull(lower_bound))
upper_bound <- cbind(pred %>% filter(tau==0.5, target=="gdp", forecast_year>=2013) %>% pull(upper_bound),
                     pred %>% filter(tau==0.8, target=="gdp", forecast_year>=2013) %>% pull(upper_bound))
truth_value <- cbind(pred %>% filter(tau==0.5, target=="gdp", forecast_year>=2013) %>% pull(truth_value),
                     pred %>% filter(tau==0.8, target=="gdp", forecast_year>=2013) %>% pull(truth_value))

mean(weighted_interval_score(truth_value, lower_bound, upper_bound, c(0.5, 0.8)), na.rm=TRUE)

mean(pred[pred$tau=="0.9"&pred$target=="gdp"&pred$horizon==1.0&pred$forecast_year>=2013,"covered"],na.rm = TRUE)
write.csv(pred,"results/qar_estimation/qar_prediction.csv")
