rm(list = ls(all=TRUE))
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(tseries)
library(forecast)
library(quantreg)
#path
file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"

#read file
df <- read.csv(file_path)

#split year and quarters
df <- df |>
  tidyr::separate(dt,into=c("year","quarter"),sep = " ")

df$year <- as.numeric(df$year)
df$quarter <- as.numeric(gsub("Q","",df$quarter))
countries <- unique(df$ccode)

filter_df <- function(df, filters) {
  df %>%
    filter(across(all_of(names(filters)), ~ . == filters[[cur_column()]]))
}

y <- df$gdp
y <- na.remove(y)

y_lag1 <- y[-1]
y <- y[-length(y)]
head(y_lag1)
length(y)
length(y_lag1)

quantile_fit <- rq(formula = y ~ y_lag1, tau=0.8)
quantile_fit_2 <- rq(formula = y ~ y_lag1, tau=0.2)

