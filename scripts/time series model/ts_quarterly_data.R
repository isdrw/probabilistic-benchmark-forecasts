rm(list = ls(all=TRUE))
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
#path
file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"

#read file
df <- read.csv(file_path)

#split year and quarters
df <- df |>
  tidyr::separate(dt,into=c("year","quarter"),sep = " ")

df$year <- as.numeric(df$year)
df$quarter <- as.numeric(gsub("Q","",df$quarter))
df_training <- df[df$year>=2012,]
countries <- unique(df_training$ccode)

filter_df <- function(df, filters) {
  df %>%
    filter(across(all_of(names(filters)), ~ . == filters[[cur_column()]]))
}


fit_ar <- data.frame(
  ccode = character(),
  
  stringsAsFactors = FALSE
)
for(country in countries){
  
}
data <- df$gdp
data[is.na(data)] <- mean(data,na.rm=TRUE)
ts_data <- ts(data)

acf_values <- acf(ts_data,lag.max = 30,plot = TRUE)
pacf_values <- pacf(ts_data,lag.max = 20, plot = TRUE)

fit <- arima(ts_data,order=c(1,0,0))

Box.test(residuals(fit),lag = 4,type = "Ljung-Box")
