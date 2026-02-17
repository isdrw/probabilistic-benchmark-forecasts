library(ggplot2)
library(tibble)

set.seed(12)
T <- 30
R <- 5
tau <- 0.8

sigma <- 0.4
process <- "AR"

y <- numeric(T)

#true data generating process
if(process ==  "MA"){
  # MA(1) parameters
  theta <- 0.4
  
  # innovations
  u <- rnorm(T + 1, sd = sigma)
  
  # MA(1) process
  
  for (t in 1:T) {
    y[t] <- u[t + 1] + theta * u[t]
  }
}else{
  #AR(1) parameters
  phi <- 0.4
  
  y[1] <- rnorm(1, sd = sigma / sqrt(1 - phi^2))
  
  for (t in 2:T) {
    y[t] <- phi * y[t - 1] + rnorm(1, sd = sigma)
  }
}

#point predictions unbiased with noise
x <- y + rnorm(T, sd = sigma)

#absolute forecast errors
abs_errors <- abs(y-x)

#rolling window quantiles function
rolling_quantile <- function(errors, t, R, tau){
  if(t <= R){
    return(NA)
  }
  quantile(errors[(t-R):(t-1)], probs = tau, type= 7)
}

q_tau <- sapply(seq_len(T), function(t){
  rolling_quantile(abs_errors, t, R = R, tau = tau)
})

lower_bound <- as.numeric(x - q_tau)
upper_bound <- as.numeric(x + q_tau)

coverage <- mean(y >= lower_bound & y <= upper_bound, na.rm = TRUE)

plot_df <- tibble(
  t = 1:T,
  y = y,
  x = x,
  lower = lower_bound,
  upper = upper_bound
)


#plot intervals Boxplot Style
ggplot(plot_df, aes(x = t)) +
  
  # prediction intervals (boxplot-style)
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.4,
    color = "steelblue",
    alpha = 0.8
  ) +
  
  # point forecasts
  geom_point(
    aes(y = x),
    color = "blue",
    size = 2,
    alpha = 0.8
  ) +
  
  # observations
  geom_point(
    aes(y = y),
    color = "black",
    size = 2
  ) +
  
  #connect observations
  geom_line(
    aes(y = y),
    color = "black",
    linewidth = 0.6,
    alpha = 0.6
  ) +
  
  # rolling window start
  geom_vline(
    xintercept = R,
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  
  labs(
    title = "Quantile-Based Prediction Intervals Using Absolute Forecast Errors",
    subtitle = paste0(
      process,"(1) data, rolling window R = ", R,
      ", nominal coverage τ = ", tau, ", actual coverage: ", coverage * 100, "%"
    ),
    x = "Time",
    y = "Value"
  ) +
  
  annotate(
    "text",
    x = R,
    y = -Inf,
    label = "R",
    size = 4,
    vjust = 1
  ) + 
  
  coord_cartesian(clip = "off") +
  
  theme_minimal()
