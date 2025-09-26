#load library 
library(arrow)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)
df[100:110,]
(unique(df$target))
