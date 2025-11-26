rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)

#source utility functions
source("scripts/utilities/utility_functions.R")

file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"

#read file
df <- read.csv(file_path)

#split year and quarters
df <- df |>
  tidyr::separate(dt,into=c("year","quarter"),sep = " ")

df$year <- as.numeric(df$year)
df$quarter <- as.numeric(gsub("Q","",df$quarter))
countries <- unique(df$ccode)

pred_quantiles <- function(df, tau, target, R = 44, n_ahead = 4){
  
  #initialize prediction dataframe
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
 
      #prediction
      preds <- unconditional_quantiles(data, tau, n_ahead = 4)
      preds_l <- preds$preds_l
      preds_u <- preds$preds_u

      # horizons (quarterly)
      h_steps <- 1:n_ahead
      horizons <- h_steps / 4
      
      # target quarter and year
      tq <- ((end_quarter - 1 + h_steps) %% 4) + 1
      ty <- end_year + ((end_quarter - 1 + h_steps) %/% 4)
      
      # truth values
      truth_values <- sapply(i + h_steps, function(idx) {
        if (idx <= nrow(data_by_country)){
          data_by_country[[target]][idx] 
        }else{
          NA
        }   
      })
      
      new_row <- new_pred_row(
        country=country,
        forecast_year = end_year,
        target_year = ty,
        target_quarter = tq,
        target = target,
        horizon = horizons,
        tau = tau,
        lower_bound = preds_l,
        upper_bound = preds_u,
        truth_value = truth_values
      )
      predictions <- rbind(predictions, new_row)
    }
  }
      
  return(predictions)
}

pred <- init_output_df()

for(tau in seq(0.1,0.9,0.1)){
  for(target in c("gdp","cpi")){
    pred <-  rbind(pred,pred_quantiles(df,tau,target))
  }
}

pred$IS <- interval_score(pred$truth_value,pred$lower_bound,pred$upper_bound,pred$tau)
pred$covered <- pred$truth_value >= pred$lower_bound & pred$truth_value <= pred$upper_bound
lower_bound <- matrix(c(pred[pred$tau==0.5&pred$target=="gdp"&pred$forecast_year>=2013,"lower_bound"],
                        pred[pred$tau==0.8&pred$target=="gdp"&pred$forecast_year>=2013,"lower_bound"]), 
                      ncol = 2)
upper_bound <- matrix(c(pred[pred$tau==0.5&pred$target=="gdp"&pred$forecast_year>=2013,"upper_bound"], 
                        pred[pred$tau==0.8&pred$target=="gdp"&pred$forecast_year>=2013,"upper_bound"]), 
                      ncol = 2)
truth_value <- matrix(c(pred[pred$tau==0.5&pred$target=="gdp"&pred$forecast_year>=2013,"truth_value"], 
                        pred[pred$tau==0.8&pred$target=="gdp"&pred$forecast_year>=2013,"truth_value"]), 
                      ncol = 2)

mean(weighted_interval_score(truth_value, lower_bound, upper_bound, c(0.5, 0.8)), na.rm=TRUE)

mean(pred[pred$tau=="0.6"&pred$target=="gdp"&pred$horizon==0.25&pred$forecast_year>=2013, "covered"], na.rm=TRUE)


