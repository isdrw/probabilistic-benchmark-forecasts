#load library 
library(arrow)
library(dplyr)
library(tidyr)
#path
file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"

#read data file
df <- read.csv(file_path)

(unique(df$ccode))

p <- read.csv("results/unconditional_quantiles/unconditional_quantiles_oecd_2025-12-09_19-06-32.csv")

p %>% filter(country=="CAN", target == "gdp", tau==0.8, horizon==0.5, target_year == 2000 | target_year == 2001) 

d <- load_WEO_data()

(0.25 * log1p(-0.1266/100) + 0.5 * log1p(-0.1266/100) + 0.75 * log1p(-0.1266/100) + 
  1 * log1p(-0.1266/100) + 0.75 * log1p(-0.0456/100) + 0.5 * log1p(-0.0456/100) + 0.25 * log1p(-0.0456/100))*100
