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

df_rw %>% filter(country=="CAN", horizon==0.5, target_year == 2000 | target_year == 2001) 

a3 <- df_rw %>% aggregate_to_annual_input()

weo <- df_weo %>% filter(g7==1)

a2 <- p %>% select(-covered, -IS) %>%
  aggregate_to_annual()

a2 <- is_covered(a2)

a2 <- calc_IS_of_df(a2)

a2 %>% filter(forecast_year<=2012, forecast_year>=2001, horizon==0.5) %>% summarise_eval()

d <- load_WEO_data()

(0.25 * log1p(-0.1266/100) + 0.5 * log1p(-0.1266/100) + 0.75 * log1p(-0.1266/100) + 
  1 * log1p(-0.1266/100) + 0.75 * log1p(-0.0456/100) + 0.5 * log1p(-0.0456/100) + 0.25 * log1p(-0.0456/100))*100

a1 <- load_and_prepare_ARIMA1_1_0_data()
