library(dplyr)
library(ggplot2)
library(arrow)
library(patchwork)
library(RColorBrewer)
library(quantreg)

# -------------------------
# 1. Simulate heteroskedastic AR(1)
# -------------------------

set.seed(NULL)

n <- 500
phi <- 0.8

y <- numeric(n)
eps <- rnorm(n)

for(i in 2:n){
  sigma_t <- 0.3 + 0.2 * abs(y[i-1])
  y[i] <- phi*y[i-1] + sigma_t*eps[i]
}

x <- y + rnorm(n,0,1)
err <- y - x

# -------------------------
# 2. Fit quantile regressions individually
# -------------------------

tau1 <- 0.1
tau2 <- 0.9

fit1 <- rq(err ~ x, tau = tau1)
fit2 <- rq(err ~ x, tau = tau2)

emp_quant1 <- quantile(err, tau1, na.rm = TRUE)
emp_quant2 <- quantile(err, tau2, na.rm = TRUE)

# -------------------------
# 3. Prepare data for plotting
# -------------------------

df <- data.frame(x = x, err = err)

# Color palette
colors <- brewer.pal(3, "Set2")

# -------------------------
# 4. Create ggplot with both quantiles
# -------------------------

ggplot(df, aes(x = x, y = err)) +
  geom_point(aes(color = "Data"), alpha = 0.6) +
  # Regression lines
  geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2],
              color = colors[1], size = 1.2, linetype = "solid") +  # 0.1 quantile
  geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2],
              color = colors[2], size = 1.2) +  # 0.8 quantile
  # Empirical quantile lines
  geom_hline(yintercept = emp_quant1, color = colors[1], linetype = "dashed", size = 1) +
  geom_hline(yintercept = emp_quant2, color = colors[2], linetype = "dashed", size = 1) +
  scale_color_manual(values = colors[3]) +
  labs(title = "Quantile Regression (tau = 0.1 & 0.8) of err ~ x",
       x = "x",
       y = "err",
       color = "") +
  theme_minimal(base_size = 14)

