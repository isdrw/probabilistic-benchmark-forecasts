#load library 
library(arrow)
library(dplyr)
library(tidyr)




x <- rnorm(500, 0, 1)
z <- rexp(500, 1)
eps <- rnorm(500, 0, 1)
p <- 0.8
theta <- (1 - 2 * p) / (p * (1 - p)) 
tau2 <- 2 / (p * (1 - p))
sigma <- 0.01
beta0 <- 0.34
beta1 <- 2.25
y <- beta0 + beta1 * x + theta * z + sqrt(tau2 * sigma * z) * eps


fit <- tryCatch({
  bqr(y = y, X = as.matrix(x), p = 0.8, n_iter = 10000, burn = 2000, use_minesota = FALSE)
}, error=function(e){
  message("Fit failed, message: ", e$message)
})

betas <- fit$beta_draws
fit$beta_mean
plot(x = 1:nrow(betas), y = betas[,2])

fit <- bayesQR::bayesQR(y ~ x, ndraw = 10000, quantile = 0.8)
plot(1:nrow(fit[[1]]$betadraw),fit[[1]]$betadraw[,2])

#=============================================================================

grid_weo <- crossing(
  country = c("Germany"),
  tau = c(0.7, 0.8),
  target = c("gdp", "cpi"),
  horizon = c(0.5)
)

#predict intervals for all combinations 
pred_weo <- grid_weo %>% 
  mutate(
    results = pmap(
      list(country, tau, target, horizon),
      ~ fit_bqr(df_weo_g7, ..1, ..2, ..3, ..4)
    )
  ) %>%
  pull(results) %>%
  bind_rows()


pred_weo <- pred_weo %>% is_covered() %>% calc_IS_of_df()

pred_weo %>% filter(tau == 0.8) %>% pull(covered) %>% mean(na.rm = TRUE)
pred_weo %>% filter(tau == 0.8) %>% pull(IS) %>% mean(na.rm = TRUE)


tmp <- read.csv("results/empirical_quantiles_prediction/empirical_prediction_weo_2025-12-09_16-57-01.csv")

tmp %>% filter(forecast_year<=2012, forecast_year>=2001, horizon==0.5) %>% summarise_eval()
