rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(purrr)

#source utility functions
source("scripts/utilities/utility_functions.R")
source("scripts/utilities/data_transformation_functions.R")

#load and prepare OECD quarterly data from oecd_quarterly_data.csv in folder: "data/raw"
df <- load_and_prepare_oecd_data()



pred_quantiles <- function(df, country, tau, target, R = 44, n_ahead = 4){
  
  #initialize prediction dataframe
  predictions <- init_output_df()
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country) %>%
    arrange(forecast_year, forecast_quarter)
    
  for(i in seq(44,nrow(data_by_country))){
    data <- data_by_country[(i-44+1):i,][[target]]
    #start date of observations 
    end_year <- data_by_country[i,"forecast_year"]
    start_year <- data_by_country[i-44+1,"forecast_year"]
    end_quarter <- data_by_country[i,"forecast_quarter"]
    start_quarter <- data_by_country[i-44+1,"forecast_quarter"]
    
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

      
  return(predictions)
}


grid <- crossing(
  country = unique(df$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("tv_gdp", "tv_cpi")
)

pred_test <- grid %>% 
  mutate(
    results = pmap(
      list(country, tau, target),
      ~ pred_quantiles(df, ..1, ..2, ..3)
    )
  ) %>%
  pull(results) %>%
  bind_rows()

for(tau in seq(0.1,0.9,0.1)){
  for(target in c("gdp","cpi")){
    for(country in unique(df$country)){
      pred <-  rbind(pred,pred_quantiles(df,country,tau,target))  
    }
    
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


