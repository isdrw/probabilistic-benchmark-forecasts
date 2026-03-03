library(quantreg)
library(ggplot2)

set.seed(11)

# -------------------------
# 1. Simulate heteroskedastic AR(1)
# -------------------------
n <- 500
phi <- 0.5

y <- numeric(n)
eps <- rnorm(n)

for (t in 2:n) {
  sigma_t <- 0.3 + 0.4 * abs(y[t-1])   # state-dependent variance
  y[t] <- phi * y[t-1] + sigma_t * eps[t]
}

df <- data.frame(
  y = y[-1],
  y_lag = y[-n]
)

# -------------------------
# 2. OLS
# -------------------------
ols_model <- lm(y ~ y_lag, data = df)

# -------------------------
# 3. QAR(1) for multiple taus
# -------------------------
tau_vals <- c(0.1, 0.9)

qar_models <- lapply(tau_vals, function(tau) {
  rq(y ~ y_lag, tau = tau, data = df)
})

# -------------------------
# 4. Prediction grid
# -------------------------
x_grid <- seq(min(df$y_lag), max(df$y_lag), length.out = 200)
pred_df <- data.frame(y_lag = x_grid)

pred_df$OLS <- predict(ols_model, newdata = pred_df)

for (i in seq_along(tau_vals)) {
  pred_df[[paste0("QAR_", tau_vals[i])]] <-
    predict(qar_models[[i]], newdata = pred_df)
}

# -------------------------
# 5. Plot
# -------------------------
# Get rightmost x-position
x_pos <- max(pred_df$y_lag)

# Get corresponding y-values at right edge
y_ols  <- tail(pred_df$OLS, 1)
y_q01  <- tail(pred_df$QAR_0.1, 1)
y_q09  <- tail(pred_df$QAR_0.9, 1)

ggplot(df, aes(x = y_lag, y = y)) +
  geom_point(alpha = 0.5, color = "black", size = 0.75) +
  
  geom_line(data = pred_df, aes(y = OLS), color = "blue", size = 1.2) +
  geom_line(data = pred_df, aes(y = QAR_0.1), color = "red", size = 1.2) +
  geom_line(data = pred_df, aes(y = QAR_0.9), color = "green", size = 1.2) +
  
  # Add labels next to lines
  annotate("text", x = x_pos, y = y_ols,
           label = "OLS",
           color = "blue", hjust = -0.1, size = 4) +
  
  annotate("text", x = x_pos, y = y_q01,
           label = expression(tau == 0.1),
           color = "red", hjust = -0.1, size = 4) +
  
  annotate("text", x = x_pos, y = y_q09,
           label = expression(tau == 0.9),
           color = "green", hjust = -0.1, size = 4) +
  
  labs(
    title = "Quantile Autoregression vs OLS Regression",
    x = expression(y[t-1]),
    y = expression(y[t])
  ) +
  theme_minimal() +
  
  # Add a bit of extra space on the right for labels
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))


