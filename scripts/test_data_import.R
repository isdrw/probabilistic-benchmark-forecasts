#load library 
library(arrow)
library(dplyr)
library(tidyr)
#path
file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"

#read data file
df <- read.csv(file_path)

(unique(df$ccode))
