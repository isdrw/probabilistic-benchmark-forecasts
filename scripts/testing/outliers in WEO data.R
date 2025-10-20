#load library 
library(arrow)
library(dplyr)
library(tidyr)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)

#prediction errors
df$err <- df$prediction-df$tv_1

#testing assumption: prediction errors are normally distributed
##qq plot of errors

qqnorm(df$err)
qqline(df$err, col="blue")


df[which(df$err>1000),]
