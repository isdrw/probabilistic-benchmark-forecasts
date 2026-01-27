rm(list = ls())
gc()

library(arrow)
library(dplyr)
library(tidyr)
library(purrr)
library(nortest)
library(fitdistrplus)
library(ald)

df_weo <- load_and_prepare_WEO_data()

test_distr <- function(x, R = 11, distr = "norm", alpha = 0.05){
  
  out <- numeric()
  
  for(i in seq(R, len(x))){
    #rolling window
    x_r <- x[(i-R+1):i]
    
    test_result <- NA
    
    test_result <- switch (distr,
     "norm" = {
       #Anderson-Darling normality test
        test_result <- ad.test(x_r)$p.value > alpha
     },
     
     "t" = {
       #fit t distribution on data and perform goodness of fit test
       fit <- try(fitdistrplus::fitdist(x_r, distr = "t", start = list(df = 5)), silent = TRUE)
       
       if(inherits(fit, "try-error")){
         FALSE
       }else{
         ks.test(x_r, "pt", df = fit$estimate["df"])$p.value > alpha
       }
     },
     
     "skewt" = {
       
     },
     
     "ald" = {
       #fit t distribution on data and perform goodness of fit test
       fit <- try(ald::mleALD(x_r), silent = TRUE)
       
       if(inherits(fit, "try-error")){
         FALSE
       }else{
         ks.test(x_r, "pald", location = fit$mu, scale = fit$sigma, p = fit$p)$p.value > alpha
       }
     },
     
     stop("Unknown distribution.\nChoose from: \"norm\", \"t\", \"skewt\", \"ald\"")
   )
    
  out[index] <- test_result
  index <- index + 1
  } 
}


x <- rnorm(100)
test <- ad.test(x)
test$p.value
