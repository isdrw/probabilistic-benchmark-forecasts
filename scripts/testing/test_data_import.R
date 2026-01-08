#load library 
library(arrow)
library(dplyr)
library(tidyr)





gauss_eval_arima110 <- read.csv("results/gauss_quantiles_prediction/mean 0 assumption/gauss_prediction_arima1_1_0_2026-01-02_11-34-48.csv") %>%
  filter(forecast_year<=2012, forecast_year>=2001, horizon==0.5) %>%
  summarise_eval() 

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

write.csv(gauss_eval_arima110, paste0(
  "results/gauss_quantiles_prediction/mean 0 assumption/gauss_prediction_arima1_1_0_eval_", 
  timestamp, ".csv"), row.names = FALSE)



y <- rnorm(1000, mean = 2, sd = 1)

x <- y[-1000]

norm_data <- data.frame(
  y = y[-1],
  x = x
)

out_list <- list()
index <- 1
for(i in 100:998){
  y_r <- as.numeric(norm_data$y[(i-100+1):i])
  x_r <- as.matrix(norm_data$x[(i-100+1):i])
  last_x <- as.numeric(norm_data$x[i+1])
  last_y <- as.numeric(norm_data$y[i+1])
  
  fit_l <- tryCatch({
    bqr(y_r, x_r, tau = 0.1, n_iter = 3000, burn = 500)
  }, error=function(e){
    message("Fit failed, message: ", e$message)
  })
  fit_u <- tryCatch({
    bqr(y_r, x_r, tau = 0.9, n_iter = 3000, burn = 500)
  }, error=function(e){
    message("Fit failed, message: ", e$message)
  })
  
  pred_l <- fit_l$beta_mean[1] + fit_l$beta_mean[2] * last_x
  pred_u <- fit_u$beta_mean[1] + fit_u$beta_mean[2] * last_x
  
  out_list[[index]] <- data.frame(
    pred_l = pred_l,
    pred_u = pred_u,
    truth_value = last_y,
    prediction = last_x
  )
  index <- index + 1
  
  cat(paste0("iteration: ", i))
}

out <- bind_rows(out_list)


mean(out$pred_l)
mean(out$pred_u)
