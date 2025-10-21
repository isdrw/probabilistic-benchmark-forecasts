rm(list = ls())
gc()

#load library 
library(arrow)
library(dplyr)
library(tidyr)
library(nortest)
library(fitdistrplus)

#path
file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"

#read data file
df <- read_parquet(file_path)


#prediction errors
df$err <- df$prediction-df$tv_1

#testing assumption: prediction errors are normally distributed
##qq plot of errors 


##Subset errors for both targets
errors_pcpi <- df[df$target == "pcpi_pch"&df$g7==1&df$horizon==0.5&df$forecast_year>=2011&df$forecast_year<=2019, "err"]
errors_ngdp <- df[df$target == "ngdp_rpch"&df$g7==1&df$horizon==0.5&df$forecast_year>=2011&df$forecast_year<=2019, "err"]

errors_pcpi <- errors_pcpi[!is.na(errors_pcpi)]
errors_ngdp <- errors_ngdp[!is.na(errors_ngdp)]

##remove outliers using IQR Method (Tukey)
remove_outliers <- function(x){
  q1 <- quantile(x, 0.25, na.rm=TRUE)
  q3 <- quantile(x, 0.75, na.rm=TRUE)
  iqr_err <- IQR(x,na.rm = TRUE)
  
  lower_bound <- q1-iqr_err*1.5
  upper_bound <- q3+iqr_err*1.5
  
  x_filtered <- x[x>=lower_bound&x<=upper_bound]
  return(x_filtered)
}

##function to plot qq plots of both PCI and GDP
plot_qq <- function(x_1, x_2){
  par(mfrow = c(1, 2), oma=c(0,0,3,0))
  
  # QQ plot 1: pcpi_pch
  qqnorm(x_1, main = "QQ Plot: Inflation", col = "black")
  qqline(x_1, col = "blue", lwd = 2)
  
  # QQ plot 2: ngdp_rpch
  qqnorm(x_2, main = "QQ Plot: GDP", col = "black")
  qqline(x_2, col = "red", lwd = 2)
  
  mtext("QQ Plots of Prediction Errors",
        outer = TRUE, cex = 1.4, font = 2)
}

#qq-plots on all datapoints
plot_qq(errors_pcpi,errors_ngdp)
par(mfrow = c(1, 1))

##Anderson-Darling test for normality
ad.test(errors_pcpi)
ad.test(errors_ngdp)


#removing outliers
errors_pcpi <- remove_outliers(errors_pcpi)
errors_ngdp <- remove_outliers(errors_ngdp)

#qq plot after outliers have been removed
plot_qq(errors_pcpi,errors_ngdp)
par(mfrow = c(1, 1))

##Anderson-Darling test for normality
ad.test(errors_pcpi)
ad.test(errors_ngdp)

#shapiro test for small sample size
shapiro.test(errors_pcpi)
shapiro.test(errors_ngdp)



