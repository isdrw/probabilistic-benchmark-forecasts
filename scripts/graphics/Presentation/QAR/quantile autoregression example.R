library(quantreg)
library(ggplot2)
library(RColorBrewer)

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

#unconditional quantiles
q01_uncond <- quantile(df$y, 0.1)
q09_uncond <- quantile(df$y, 0.9)


set2 <- brewer.pal(8, "Set2")

ggplot(df, aes(x = y_lag, y = y)) +
  geom_point(alpha = 0.5, color = "black", size = 0.75) +
  
  geom_line(data = pred_df, aes(y = OLS), color = set2[1], size = 1.2) +
  geom_line(data = pred_df, aes(y = QAR_0.1), color = set2[2], size = 1.2) +
  geom_line(data = pred_df, aes(y = QAR_0.9), color = set2[3], size = 1.2) +
  
  # Unconditional quantiles
  geom_hline(yintercept = q01_uncond,
             linetype = "dashed",
             color = set2[2],
             alpha = 0.7,
             size = 1) +
  
  geom_hline(yintercept = q09_uncond,
             linetype = "dashed",
             color = set2[3],
             alpha = 0.7,
             size = 1) +
  
  # Labels for conditional lines
  annotate("text", x = x_pos, y = y_ols,
           label = "OLS",
           color = set2[1], hjust = -0.1, size = 4) +
  
  annotate("text", x = x_pos, y = y_q01,
           label = expression(tau == 0.1),
           color = set2[2], hjust = -0.1, size = 4) +
  
  annotate("text", x = x_pos, y = y_q09,
           label = expression(tau == 0.9),
           color = set2[3], hjust = -0.1, size = 4) +
  
  # Labels for unconditional quantiles
  annotate("text",
           x = x_pos,
           y = q01_uncond,
           label = expression("Uncond." ~ tau == 0.1),
           color = set2[2],
           hjust = 1.1,
           vjust = -0.5,
           size = 4) +
  
  annotate("text", x = min(pred_df$y_lag),
           y = q09_uncond,
           label = expression("Uncond." ~ tau == 0.9),
           color = set2[3], hjust = 0, vjust = -0.5, size = 4) +
  
  labs(
    title = "Quantile Autoregression vs OLS Regression",
    x = expression(y[t-1]),
    y = expression(y[t])
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))


