#load library 
library(arrow)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)


#parameters
#rolling window
R <- 11
tau <- 0.9
#quantiles


#calculating absolute forecast errors
df$abs_err <- abs(df$prediction-df$tv_1)


#function to calculate empirical quantiles
emp_q <- function(df, R, h, tau){
  #filtered df for given h
  df_filtered <- df[df$horizon==h,]
  
}

