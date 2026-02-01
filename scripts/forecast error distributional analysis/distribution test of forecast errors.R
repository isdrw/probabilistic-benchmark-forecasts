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

df_weo <- df_weo %>% mutate(
  gdp_err = tv_gdp - pred_gdp,
  cpi_err = tv_cpi - pred_cpi
)

df_weo_g7 <- df_weo %>% filter(g7 == 1)

test_distr <- function(x, R = 11, distr = "norm", alpha = 0.05){
  
  out <- numeric()
  index <- 1
  
  for(i in seq(R, length(x))){
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
       x_clean <- x_r[!is.na(x_r)]
       
       if (length(x_clean) < 5) {
         FALSE
       } else {
         fit <- try(ald::mleALD(x_clean), silent = TRUE)
         
         if (inherits(fit, "try-error")) {
           FALSE
         } else {
           par <- as.numeric(fit$par)
           
           # must be exactly (mu, sigma, p)
           if (length(par) != 3 || any(!is.finite(par))) {
             FALSE
           } else {
             mu    <- par[1]
             sigma <- par[2]
             p     <- par[3]
             
             if (sigma <= 0 || p <= 0 || p >= 1) {
               FALSE
             } else {
               ks.test(
                 x_clean,
                 ald::pALD,
                 mu    = mu,
                 sigma = sigma,
                 p     = p
               )$p.value > alpha
             }
           }
         }
       }
     },
     
     
     stop("Unknown distribution.\nChoose from: \"norm\", \"t\", \"skewt\", \"ald\"")
   )
    
  out[index] <- test_result
  index <- index + 1
  }
  
  return(out)
}

df_weo_g7 %>% filter(country == "Germany", horizon == 0.5) %>% 
  pull(gdp_err) %>% na.exclude() %>% test_distr(distr = "ald", alpha = 0.5) %>% mean(na.rm = TRUE)


