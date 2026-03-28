# Install if needed
# install.packages("ALD")

library(ald)
library(ggplot2)
library(RColorBrewer)

# Parameters
alpha <- 0.8
mu <- 0
sigma <- 1

# Grid
u <- seq(-3, 3, length.out = 500)

# ALD density from package
ald_density <- dALD(u, mu = mu, sigma = sigma, p = alpha)

# Check loss
check_loss <- u * (alpha - (u < 0))

# Scale check loss for comparability
check_loss_scaled <- check_loss / max(check_loss) * max(ald_density)

# Data frame
df <- data.frame(
  u = u,
  ald_density = ald_density,
  check_loss = check_loss_scaled
)

# Colors
cols <- brewer.pal(3, "Set2")

# Plot
ggplot(df, aes(x = u)) +
  geom_line(aes(y = ald_density, color = "ALD density"), linewidth = 1.2) +
  geom_line(aes(y = check_loss, color = "Check loss"), linewidth = 1.2) +
  
  geom_vline(xintercept = mu, linetype = "dashed", linewidth = 1) +
  
  scale_color_manual(values = c(cols[2], cols[3])) +
  
  labs(
    x = "Residual (y - Xθ)",
    y = "Density / Scaled Loss",
    color = "",
    title = bquote("ALD Density and Check Loss (" * alpha == .(alpha) * ")")
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 12)
  )