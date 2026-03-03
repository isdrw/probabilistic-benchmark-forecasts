library(ggplot2)
library(Matrix)
library(tidyr)

set.seed(66)

#DGP 
n <- 5002
x <- runif(n, -2, 2)
y <- x + rnorm(n)
#===============================================
#Fit EasyUQ model

x <- x[3:n]
y <- y[3:n]
x_order <- order(x)
x <- x[x_order]
y <- y[x_order]

fit <- easyUQ_idr(x, y)
nx <- length(fit$x)

#choose observation columns 
rows <- c(
  round(nx * 0.25),
  round(nx * 0.50),
  round(nx * 0.75)
)

#data frame with three CDF curves
df <- data.frame(
  y   = fit$y_grid,
  F25 = fit$F_hat[rows[1],],
  F50 = fit$F_hat[rows[2],],
  F75 = fit$F_hat[rows[3],]
)

#long format

df_long <- pivot_longer(df, cols = c(F25, F50, F75),
                        names_to = "curve",
                        values_to = "F")

#map curve names to actual x values
label_map <- c(
  F25 = paste0("x = ", round(fit$x[rows[1]], 3)),
  F50 = paste0("x = ", round(fit$x[rows[2]], 3)),
  F75 = paste0("x = ", round(fit$x[rows[3]], 3))
)

#plot cdfs
ggplot(df_long, aes(x = y, y = F, color = curve)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set2", labels = label_map) +
  labs(
    title = "EasyUQ IDR",
    x = "y",
    y = expression(hat(F)(y ~ "|" ~ x)),
    color = "Forecast",
    caption = expression(
      "Data generating process:  " *
        x %~% U(-2,2) * "," ~
        y == x + epsilon * "," ~
        epsilon %~% N(0,1)
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.caption = element_text(size = 12, hjust = 0)
  )

which(df$F25<df$F50)


