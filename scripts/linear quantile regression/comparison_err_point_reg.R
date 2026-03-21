library(dplyr)
library(ggplot2)
library(arrow)
library(patchwork)
library(RColorBrewer)
library(quantreg)

# -------------------------
# Simulate  AR(1)
# -------------------------

selected_country = "Japan"
selected_horizon = 1.5
df_weo <- load_and_prepare_WEO_data() %>% filter(country == selected_country, horizon == selected_horizon)

y <- df_weo %>% pull(tv_gdp)
x <- df_weo %>% pull(pred_gdp)

set.seed(11)

n <- 1000
phi <- 0.75

# heteroskedasticity
het <- TRUE
sigma_t <- 1


y <- numeric(n)
eps <- rnorm(n)

# AR(1) simulated data
for(i in 2:n){
  if(het){
    # introduce heteroskedasticity
    sigma_t <- 0.3 + 0.25 * abs(y[i-1])  
  }
  
  
  y[i] <- phi*y[i-1] + sigma_t*eps[i]
}

# simulate point forecasts
x <- numeric(n)
x[1:2] <- y[1:2]
for(i in 3:n){
  fit_y <- arima(y[1:i], order = c(1,0,0))
  x[i] <- as.numeric(predict(fit_y)$pred[1])
}

# add some noise to forecasts to make them "less perfect"
x <- x + rnorm(n, sd = 0.075)

# forecast errors
err <- y - x

# -------------------------
# Fit quantile regressions individually
# -------------------------

#confidence levels
tau1 <- 0.1
tau2 <- 0.9

# linear quantile regression of forecast errors on point forecasts
fit1 <- rq(err ~ x, tau = tau1)
fit2 <- rq(err ~ x, tau = tau2)

# empirical quantiles (unconditional)
emp_quant1 <- quantile(err, tau1, na.rm = TRUE)
emp_quant2 <- quantile(err, tau2, na.rm = TRUE)


df <- data.frame(x = x, err = err)
# -------------------------
# Prepare data for plotting
# -------------------------
set2 <- brewer.pal(8, "Set2")

# sort data for line plotting
df <- df[order(df$x), ]

# prediction lines
pred_df <- data.frame(
  x = df$x,
  q01 = coef(fit1)[1] + coef(fit1)[2] * df$x,
  q09 = coef(fit2)[1] + coef(fit2)[2] * df$x
)

# label positions
x_pos <- max(df$x)
y_q01 <- tail(pred_df$q01, 1)
y_q09 <- tail(pred_df$q09, 1)

# -------------------------
# Plot
# -------------------------

ggplot(df, aes(x = x, y = err)) +
  geom_point(alpha = 0.5, color = "black", size = 1) +
  
  # conditional quantile lines
  geom_line(data = pred_df, aes(y = q01), color = set2[2], linewidth = 1.2) +
  geom_line(data = pred_df, aes(y = q09), color = set2[3], linewidth = 1.2) +
  
  # unconditional quantiles
  geom_hline(yintercept = emp_quant1,
             linetype = "dashed",
             color = set2[2],
             alpha = 0.7,
             linewidth = 1) +
  
  geom_hline(yintercept = emp_quant2,
             linetype = "dashed",
             color = set2[3],
             alpha = 0.7,
             linewidth = 1) +
  
  # labels for conditional lines
  annotate("text", x = x_pos, y = y_q01,
           label = expression(tau == 0.1),
           color = set2[2], hjust = -0.1, size = 4) +
  
  annotate("text", x = x_pos, y = y_q09,
           label = expression(tau == 0.9),
           color = set2[3], hjust = -0.1, size = 4) +
  
  # labels for unconditional quantiles
  annotate("text",
           x = x_pos,
           y = emp_quant1,
           label = expression("Empirical" ~ tau == 0.1),
           color = set2[2],
           hjust = 1.1,
           vjust = -0.5,
           size = 4) +
  
  annotate("text",
           x = min(df$x),
           y = emp_quant2,
           label = expression("Empirical" ~ tau == 0.9),
           color = set2[3],
           hjust = 0,
           vjust = -0.5,
           size = 4) +
  
  labs(
    title = "Linear Quantile Regression vs. Empirical Quantiles",
    x = expression(x[t]),
    y = expression(epsilon[t])
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

