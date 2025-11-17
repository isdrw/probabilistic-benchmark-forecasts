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
      
      #lag data
      data_lagged <- lapply(1:nlag,function(L) dplyr::lag(data,n = L))
      names(data_lagged) <- paste0("lag_",1:nlag)
      
      #dataframe for regression
      data_reg <- data.frame(data,do.call(cbind,data_lagged))
      names(data_reg) <- c("y",paste0("lag_",1:nlag))
      
      #remove NAs
      data_reg <- na.omit(data_reg)
      
      #fit qar(p) model with p=nlag
      #lower quantile
      fit_l <- tryCatch({
        rq(formula = y ~ ., tau = (1-tau)/2, data = data_reg)
      },error=function(e){
        message("Fit failed for ", country, " (", start_year, "–", end_year, "): ", e$message)
        NULL
      })
      
      #median for prediction 
      fit_m <- tryCatch({
        rq(formula = y ~ ., tau = 0.5, data = data_reg)
      },error=function(e){
        message("Fit failed for ", country, " (", start_year, "–", end_year, "): ", e$message)
        NULL
      })
      
      fit_u <- tryCatch({
        rq(formula = y ~ ., tau = (1+tau)/2, data = data_reg)
      },error=function(e){
        message("Fit failed for ", country, " (", start_year, "–", end_year, "): ", e$message)
        NULL
      })
      
      if(is.null(fit_l)||is.null(fit_u)){
        next
      }
      
      #last lagged values of current rolling window
      last_lags <- as.numeric(tail(data_reg[, -1, drop=FALSE], 1))
      
      #quantile forecast for 4 quarters
      preds_l <- numeric(4)
      preds_u <- numeric(4)
      for(h in 1:4){
        new_data <- as.data.frame(t(last_lags[1:nlag]))
        names(new_data) <- paste0("lag_", 1:nlag)
        pred_l <- as.numeric(predict(fit_l, newdata=new_data))
        pred_u <- as.numeric(predict(fit_u, newdata=new_data))
        preds_l[h] <- pred_l
        preds_u[h] <- pred_u
        
        # update lag vector
        pred_m <- as.numeric(predict(fit_m, newdata=new_data))
        last_lags <- c(pred_m, head(last_lags, nlag-1))
      }
      
      #append predictions dataframe with new prediction of quantiles
      for(h in 1:4){
        fq <- end_quarter + h
        fy <- end_year + (fq - 1) %/% 4
        fq <- ((fq - 1) %% 4) + 1
        #truth value
        tv <- if((i + h) <= nrow(data_by_country)) {
          data_by_country[[target]][i + h]
        } else {
          NA
        }
        
        new_row <- data.frame(
          country = country,
          forecast_year = end_year,
          target_year = fy,
          target_quarter = fq,
          horizon = h/4,
          target = target,
          tau = tau,
          lower_bound = preds_l[h],
          upper_bound = preds_u[h],
          truth_value = tv,
          stringsAsFactors = FALSE
        )
        predictions <- rbind(predictions, new_row)
      }
    }
  }
  return(predictions)
}


pred <- data.frame(
  country=character(),
  forecast_year=numeric(),
  target_year=numeric(),
  target_quarter=numeric(),
  horizon=numeric(),
  target=character(),
  tau=numeric(),
  lower_bound=numeric(),
  upper_bound=numeric()
)


for(tau in seq(0.1,0.9,0.1)){
  pred <- rbind(pred,fit_qar(df,tau = tau,target = "gdp"))
  pred <- rbind(pred,fit_qar(df,tau = tau,target = "cpi"))
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

mean(pred[pred$tau=="0.9"&pred$target=="gdp"&pred$horizon==0.25&pred$target_year>=2013,"covered"],na.rm = TRUE)
write.csv(pred,"results/qar_estimation/qar_prediction.csv")
