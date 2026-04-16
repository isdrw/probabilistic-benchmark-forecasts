#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(bayesQR)



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

fit_y <- bayesQR::bayesQR(y~x, ndraw = 5000)
beta_mean0 <- mean(fit_y[[1]]$betadraw[1000:5000,1])
beta_mean1<- mean(fit_y[[1]]$betadraw[1000:5000,2])

plot(x, y)
lines(x, beta_mean0 + beta_mean1 * x)
lines(x, rep(quantile(y, probs = p), length(x)))


plot(density(y))

hist(y)

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


test_fit <- fitdistrplus::fitdist(rlnorm(100), distr = "lnorm")
as.numeric(test_fit$estimate["sdlog"])


required_index <- (4*(2000-1)+2):(4*2000+4)

tmp<-df_ar1 %>% group_by(country) %>% group_modify(~{
  pred_df <- .x %>%
    filter(
      forecast_year == 1999,
      horizon == 1.0
    ) %>%
    arrange(tq_index)
})

df_rw <- load_and_prepare_RW_data() %>% aggregate_to_annual_input()

df_rw %>% mutate(
  sqr_err_cpi = (pred_cpi-tv_cpi)**2,
  sqr_err_gdp = (pred_gdp-tv_gdp)**2
) %>% group_by(
  horizon  
) %>% summarise(
  MSFE_cpi = mean(sqr_err_cpi, na.rm = TRUE),
  MSFE_gdp = mean(sqr_err_gdp, na.rm = TRUE),
  .groups = "drop"
)




set.seed(123)

n <- 3000

# simulate point forecasts
x <- rnorm(n)

# heteroskedastic noise (variance depends on x)
sigma <- 0.5 + 0.5 * abs(x)

# observed outcomes
y <- x + rnorm(n, sd = sigma)

# run both methods
res_osqp  <- easyUQ_idr(x, y)
res_gpava <- easyUQ_idr_pav(x, y)

# compare solutions
max_diff <- max(abs(res_osqp$F_hat - res_gpava$F_hat))

max_diff

j <- 20

plot(res_osqp$x, res_osqp$F_hat[j,], type="l", col="red", lwd=2)
lines(res_gpava$x, res_gpava$F_hat[j,], col="blue", lty=2)


library(microbenchmark)

bench <- microbenchmark(
  OSQP  = easyUQ_idr(x, y),
  GPAVA = easyUQ_idr_pav(x, y),
  times = 10
)

print(bench)


df_rw <- load_and_prepare_RW_data() 
first_diff <- df_rw %>% 
  filter(country == "CAN", horizon == 0.0) %>%
  mutate(first_diff = tv_cpi - pred_cpi) %>% pull(first_diff)
hist(first_diff, breaks = 30)
ad.test(first_diff)


