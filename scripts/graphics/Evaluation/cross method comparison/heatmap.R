library(arrow)
library(ggplot2)
library(dplyr)
library(forcats)


eval_df <- read.csv("data/Evaluation results/evaluation_table.CSV", sep = ";")

#===============================================================================
#WIS Heatmap of Dataset x Methods for Combination of Target x Horizon x Frequency
#===============================================================================

heatmap_data <- eval_df %>%
  mutate(
    Dataset = factor(dataset),
    Method  = factor(method)
  ) %>%
  filter(frequency == "annually", dataset != "OECD", 
         horizon == 0.0, method != "bayesian_quantile_regression",
         method != "EasyUQ_idr",
         target == "cpi") %>%
  group_by(Dataset, Method) %>%
  summarise(mean_WIS_norm = mean(WIS_all_group_norm, na.rm = TRUE), .groups = "drop")

# Heatmap
ggplot(heatmap_data, aes(x = Method, y = Dataset, fill = mean_WIS_norm)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "green", high = "red", name = "Normalized WIS") +
  
  labs(
    title = "Heatmap of Normalized WIS (WIS_all_group_norm) per Dataset & Method",
    x = "Method",
    y = "Dataset"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14)
  )
