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



x <- rnorm(500, 0, 1)
z <- rexp(500, 1)
eps <- rnorm(500, 0, 1)
p <- 0.5
theta <- (1 - 2 * p) / (p * (1 - p)) 
tau2 <- 2 / (p * (1 - p))
sigma <- 0.1
beta0 <- 0.34
beta1 <- 1.25
y <- beta0 + beta1 * x + theta * z + sqrt(tau2 * sigma * z) * eps



out_list <- list()
index <- 1
for(i in 100:499){
  y_r <- y[(i-100+1):i]
  x_r <- as.matrix(x[(i-100+1):i],ncol = 1)
  last_x <- x[i+1]
  last_y <- y[i+1]
  
  fit_m <- tryCatch({
    bqr(y_r, x_r, p = 0.5, n_iter = 3000, burn = 500)
  }, error=function(e){
    message("Fit failed, message: ", e$message)
  })
  
  sim_y <- predict_bqr(fit_m, cbind(1, last_x))
  
  pred_l <- quantile(sim_y, probs = 0.1, type = 7)
  pred_u <- quantile(sim_y, probs = 0.9, type = 7)
  
  out_list[[index]] <- data.frame(
    pred_l = pred_l,
    pred_u = pred_u,
    truth_value = last_y
  )
  index <- index + 1
  
  cat(paste0("iteration: ", i))
}

out <- bind_rows(out_list)
mean(out$truth_value >= out$pred_l & out$truth_value <= out$pred_u)


library(ald)
aaa <- fGarch::rsstd(500)

fit_aaa <- fGarch::sstdFit(aaa)
fit_aaa$estimate["m"] 

fit_aaa <- ald::mleALD(aaa)
fit_aaa$par


