#load library 
library(arrow)
library(dplyr)
library(tidyr)
#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)
