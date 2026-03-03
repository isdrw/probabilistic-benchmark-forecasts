# Load required library
library(ggplot2)

# Parameters
mean_forecast <- 2
sd_low_uncertainty <- 0.3
sd_high_uncertainty <- 1.2

# Create x grid
x <- seq(-3, 7, length.out = 1000)

# Create density data
df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean_forecast, sd_high_uncertainty),
    dnorm(x, mean_forecast, sd_low_uncertainty)
  ),
  Scenario = rep(
    c("Volatile Environment (High Uncertainty)",
      "Stable Environment (Low Uncertainty)"),
    each = length(x)
  )
)

# Make Scenario a factor and reorder levels
df$Scenario <- factor(
  df$Scenario,
  levels = c(
    "Volatile Environment (High Uncertainty)",
    "Stable Environment (Low Uncertainty)"
  )
)

ggplot(df, aes(x = x, y = density, color = Scenario)) +
  geom_line(linewidth = 1.2) +
  # Vertical line
  annotate(
    "segment",
    x = mean_forecast,
    xend = mean_forecast,
    y = 0,
    yend = max(df$density)*1.05,
    linetype = "solid",
    color = "black",
    size = 0.8
  ) +
  # Label below x-axis
  annotate(
    "text",
    x = mean_forecast,
    y = -0.12,           # slightly further down
    label = "hat(mu)",
    parse = TRUE,
    vjust = 1,
    size = 5
  ) +
  labs(
    x = "Inflation Forecast (%)",
    y = "Density"
  ) +
  coord_cartesian(ylim = c(-0.25, max(df$density)*1.05)) +  # extend y-axis further
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

