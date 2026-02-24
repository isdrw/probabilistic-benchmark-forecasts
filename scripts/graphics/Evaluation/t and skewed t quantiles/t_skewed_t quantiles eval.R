
library(arrow)
library(ggplot2)
library(dplyr)
library(forcats)


eval_df <- read.csv("data/Evaluation results/evaluation_table.CSV", sep = ";")


#======================================================
#WIS plot eval of variations of t quantiles
#=====================================================

selected_dataset <- "WEO"

data_filtered <- eval_df %>%
  filter(dataset == selected_dataset,
         method == "t_quantiles_prediction") %>%
  mutate(
    Horizon = factor(horizon, levels = c(0.0, 0.5, 1.0, 1.5)),  # convert numeric to factor
    Horizon = fct_recode(Horizon,
                         "Fall, current"   = "0",
                         "Spring, current" = "0.5",
                         "Fall, next"      = "1",
                         "Spring, next"    = "1.5"),
    variation = factor(variation, levels = c("fitted_mean", "mean0"))
  ) %>%
  droplevels()

# Normalize WIS within each horizon AND target
data_normalized <- data_filtered %>%
  group_by(target, Horizon) %>%
  mutate(WIS_norm = WIS_all / mean(WIS_all, na.rm = TRUE)) %>%
  ungroup()

ggplot(data_normalized,
       aes(x = Horizon, y = WIS_norm,
           color = variation,
           shape = variation,
           linetype = variation,
           group = variation)) +
  
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "black",
             linewidth = 0.7,
             alpha = 0.7) +
  
  geom_line(linewidth = 1.2,
            position = position_dodge(width = 0.3)) +
  
  geom_point(size = 3.5,
             position = position_dodge(width = 0.3)) +
  
  facet_wrap(~target, nrow = 1) +
  
  labs(
    x = "Horizon",
    y = "Normalized WIS (1 = mean of both variations per horizon & target)",
    title = paste("t-quantiles WIS Comparison for", selected_dataset, "Dataset"),
    color = "Variation",
    shape = "Variation",
    linetype = "Variation"
  ) +
  
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )



#=================================================================
# average WIS score over all horizons and datasets (non-normalized)
#=================================================================


eval_df %>% 
  filter(method == "t_quantiles_prediction", tau == 0.8) %>%
  group_by(variation, target) %>%
  summarise(
    WIS_mean = mean(WIS_all),
    .groups = "drop"
  ) 

#=================================================================
# average coverage over all horizons and datasets 
#=================================================================


eval_df %>% 
  filter(method == "skewed_t_quantiles_prediction", tau %in% c(0.5,0.8)) %>%
  group_by(variation, target, tau) %>%
  summarise(
    WIS_mean = mean(coverage),
    .groups = "drop"
  ) 
