rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)

file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"

#read file
df <- read.csv(file_path)

#split year and quarters
df <- df |>
  tidyr::separate(dt,into=c("year","quarter"),sep = " ")

df$year <- as.numeric(df$year)
df$quarter <- as.numeric(gsub("Q","",df$quarter))
countries <- unique(df$ccode)

pred_quantiles <- function(df, tau, target, R=44){
  
  #prediction dataframe
  predictions <- data.frame(
    country=character(),
    forecast_year=numeric(),
    target_year=numeric(),
    target_quarter=numeric(),
    target=character(),
    tau=numeric(),
    lower_bound=numeric(),
    upper_bound=numeric(),
    truth_value=numeric()
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
      
      pred_l <- quantile(data, probs=(1-tau)/2, type=7, na.rm=TRUE)
      pred_u <- quantile(data, probs=(1+tau)/2, type=7, na.rm=TRUE)
      
      tv <- if((i+1) <= nrow(data_by_country)) {
        data_by_country[[target]][i+1]
      } else {
        NA
      }
      
      fq <- end_quarter + 1
      fy <- end_year + (fq - 1) %/% 4
      fq <- ((fq - 1) %% 4) + 1
      
      new_row <- data.frame(
        country = country,
        forecast_year = end_year,
        target_year = fy,
        target_quarter = fq,
        target = target,
        tau = tau,
        lower_bound = pred_l,
        upper_bound = pred_u,
        truth_value = tv,
        stringsAsFactors = FALSE
      )
      predictions <- rbind(predictions, new_row)
    }
  }
      
  return(predictions)
}


interval_score <- function(truth_value, lower_bound, upper_bound, tau){
  u <- upper_bound
  l <- lower_bound
  y <- truth_value
  IS <- (u-l) + 2/(1-tau)*(l-y)*(y<l) + 2/(1-tau)*(y-u)*(y>u)
  return(IS)
}


weighted_interval_score <- function(IS_lower, tau_lower, IS_upper, tau_upper) {
  # remove NAs
  valid <- !(is.na(IS_lower) & is.na(tau_lower) & is.na(IS_upper) & is.na(tau_upper))
  IS_lower <- IS_lower[valid]
  IS_upper <- IS_upper[valid]
  tau_lower <- tau_lower[valid]
  tau_upper <- tau_upper[valid]
  
  WIS <- 1/2 * ((1 - tau_lower) * IS_lower + (1 - tau_upper) * IS_upper)
  
  return(WIS)
}

pred <- data.frame(
  country=character(),
  forecast_year=numeric(),
  target_year=numeric(),
  target_quarter=numeric(),
  target=character(),
  tau=numeric(),
  lower_bound=numeric(),
  upper_bound=numeric(),
  truth_value=numeric()
)

for(tau in seq(0.1,0.9,0.1)){
  for(target in c("gdp","cpi")){
    pred <-  rbind(pred,pred_quantiles(df,tau,target))
  }
}

pred$IS <- interval_score(pred$truth_value,pred$lower_bound,pred$upper_bound,pred$tau)
pred$covered <- pred$truth_value >= pred$lower_bound & pred$truth_value <= pred$upper_bound

IS_lower <- pred[pred$tau==0.5,"IS"]
IS_upper <- pred[pred$tau==0.8,"IS"]
tau_lower <- pred[pred$tau==0.5,"tau"]
tau_upper <- pred[pred$tau==0.8,"tau"]

WIS_5_8 <- weighted_interval_score(IS_lower, tau_lower, IS_upper, tau_upper)
pred$WIS_5_8 <- NA
pred[pred$tau==0.5,"WIS_5_8"] <- WIS_5_8
pred[pred$tau==0.8,"WIS_5_8"] <- WIS_5_8


mean(pred[pred$tau==0.8&pred$target=="gdp","WIS_5_8"],na.rm = TRUE)
mean(pred[pred$tau=="0.9"&pred$target=="gdp"&pred$target_year>=2013,"covered"],na.rm = TRUE)
