rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(nortest)
library(fitdistrplus)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)

#prediction errors
df$err <- df$prediction-df$tv_1

#training dataset
df_training <- df[df$forecast_year<=2012&df$g7==1,]
df_holdout <- df[df$forecast_year>=2013-16&df$g7==1,]

unique(df_training$horizon)
#Linear Quantile regression for
countries <- unique(df_holdout$country)
target_variables <- unique(df_holdout$target)
horizon <- c(0.5,1)
taus <- seq(0.1,0.9,0.1)

fit_iqr <- data.frame(
  country=character(),
  target=character(),
  horizon=numeric(),
  tau=numeric(),
  lqr=I(list())
)


fit_iqr <- df_training %>%
  group_by(country,target,horizon) %>%
  group_map(function(.x,.y){
    y_t <- .x$tv_1
    y_t_hat <- .x$prediction
    
    rows <- lapply(taus,function(tau){
      fit <- rq(y_t ~ y_t_hat,tau = tau)
      new_row <- data.frame(
        country=.y$country,
        target=.y$target,
        horizon=.y$horizon,
        tau=tau,
        iqr=I(list(fit))
      )
    })
    bind_rows(rows)
  })

