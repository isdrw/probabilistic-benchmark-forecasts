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
df_oecd <- load_and_prepare_oecd_data()

pred_quantiles <- function(df, country, tau, target, R = 44, n_ahead = 4){
  
  #initialize prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  #group data by country
  data_by_country <- df %>% 
    filter(country == !!country) %>%
    arrange(forecast_year, forecast_quarter)
    
  for(i in seq(R,nrow(data_by_country))){
    data <- data_by_country[(i-R+1):i,][[target]]
    #start date of observations 
    end_year <- data_by_country[i,"forecast_year"]
    start_year <- data_by_country[i-R+1,"forecast_year"]
    end_quarter <- data_by_country[i,"forecast_quarter"]
    start_quarter <- data_by_country[i-R+1,"forecast_quarter"]
    
    #skip if only NAs
    if(all(is.na(data)) || is.null(data)){
      message("no valid data for ", country, " between ", start_year, " and ", end_year)
      next
    }

    #prediction
    preds <- unconditional_quantiles(data, tau, n_ahead = n_ahead)
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
    
    #new row
    out_list[[index]] <- new_pred_row(
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
    
    index <- index + 1
  }
  
  predictions <- bind_rows(out_list)
      
  return(predictions)
}


#=================
#prediction on quarterly data (OECD dataset)
#=================

#create grid 
grid <- crossing(
  country = unique(df_oecd$country),
  tau = seq(0.1, 0.9, 0.1),
  target = c("tv_gdp", "tv_cpi")
)

pred <- grid %>% 
  mutate(
    results = pmap(
      list(country, tau, target),
      ~ pred_quantiles(df_oecd, ..1, ..2, ..3)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


#truth value within predicted interval?
pred$covered <- pred$truth_value >= pred$lower_bound & pred$truth_value <= pred$upper_bound

#input for calculation of WIS for 50% and 80% prediction intervals
#forecast years 2013 and above and the target gdp cummulated over all g7 countries
lower_bound <- cbind(pred %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(lower_bound),
                     pred %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(lower_bound))
upper_bound <- cbind(pred %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(upper_bound),
                     pred %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(upper_bound))
truth_value <- cbind(pred %>% filter(tau==0.5, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(truth_value),
                     pred %>% filter(tau==0.8, target=="tv_gdp", forecast_year>=2013, horizon==0.5) %>% pull(truth_value))

#mean Interval scores for 50% and 80% prediction intervals
#forecast years 2013 and above and the target gdp cummulated over all g7 countries
mean(interval_score(truth_value[,1], lower_bound[,1], upper_bound[,1], 0.5), na.rm=TRUE)
mean(interval_score(truth_value[,2], lower_bound[,2], upper_bound[,2], 0.8), na.rm=TRUE)

#mean WIS 
mean(weighted_interval_score(truth_value, lower_bound, upper_bound, c(0.5, 0.8)), na.rm=TRUE)

#check calibration by calculating coverage for 80% prediction intervals, 
#forecast year 2013 and above, cummulated over all g7 countries
#TODO Mincer Zarnowitz regression for better evaluation of calibration
mean(pred %>% filter(tau == 0.5, forecast_year >= 2013, target == "tv_gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)
mean(pred %>% filter(tau == 0.8, forecast_year >= 2013, target == "tv_gdp", horizon == 0.5) %>% pull(covered),na.rm = TRUE)

#save prediction dataframe
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
write.csv(pred, paste0("results/unconditional_quantiles/unconditional_quantiles_", timestamp, ".csv"), row.names = FALSE)
