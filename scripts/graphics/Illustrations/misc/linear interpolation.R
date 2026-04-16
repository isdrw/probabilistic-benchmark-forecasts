library(ggplot2)

set.seed(13)
#sample size
n1 <- 15
n2 <- 60

#standard normal samples
x1 <- rnorm(n1, 0, 1)
x2 <- rnorm(n2, 0, 1)
x1 <- sort(x1)
x2 <- sort(x2)

#sequence of probabilities at threshold values of samples
p1_seq <- (1:n1-1)/(n1-1)
p2_seq <- (1:n2-1)/(n2-1)

#empirical quantile function via quantile(type=7) (linear interpolation) function
tau_grid <- seq(0, 1, 0.02)
q1 <- quantile(x1,probs = seq(0, 1, 0.02), type = 7)
q2 <- quantile(x2,probs = seq(0, 1, 0.02), type = 7)

# Build data.frames for points
df_points <- rbind(
  data.frame(probability = p1_seq, quantile = x1, sample = paste0("n = ", n1)),
  data.frame(probability = p2_seq, quantile = x2, sample = paste0("n = ", n2))
)

# Build data.frames for empirical quantile lines
df_lines <- rbind(
  data.frame(probability = tau_grid, quantile = q1, sample = paste0("n = ", n1)),
  data.frame(probability = tau_grid, quantile = q2, sample = paste0("n = ", n2))
)

# Build data.frame for true quantile line
df_true <- rbind(
  data.frame(probability = tau_grid, quantile = qnorm(tau_grid), sample = paste0("n = ", n1)),
  data.frame(probability = tau_grid, quantile = qnorm(tau_grid), sample = paste0("n = ", n2))
)

# Force the facet order so n1 is always left, n2 right
facet_order <- c(paste0("n = ", n1), paste0("n = ", n2))
df_points$sample <- factor(df_points$sample, levels = facet_order)
df_lines$sample <- factor(df_lines$sample, levels = facet_order)
df_true$sample <- factor(df_true$sample, levels = facet_order)

# Plot
ggplot() +
  geom_point(data = df_points, aes(x = probability, y = quantile), color = "black", shape = 20) +
  geom_line(data = df_lines, aes(x = probability, y = quantile), color = "blue", size = 0.5) +
  geom_line(data = df_true, aes(x = probability, y = quantile), color = "darkgreen", linetype = "dashed", size = 0.7) +
  facet_wrap(~sample, nrow = 1) +
  labs(
    x = "Probability",
    y = "Quantile",
    title = "Empirical quantile functions with linear interpolation (type = 7)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12)
  )