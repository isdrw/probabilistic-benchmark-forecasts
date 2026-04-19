#==========================================================
#Functions for Bayesian Quantile Regression
#==========================================================

#'!!! Generative AI; level = medium --> debugging + bayesQR function call
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = Bayesian Quantile Regression (expanding window)
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon (0.0, 0.5, 1.0, 1.5)
#'
fit_bqr <- function(df, country, tau, target, h){
  
  #prediction dataframe
  predictions <- init_output_df()
  
  #output list 
  out_list <- list()
  index <- 1
  
  data_by_country <- df %>% 
    filter(country == !!country, horizon == h) %>%
    arrange(forecast_year, forecast_quarter)
  
  for(i in seq(2,nrow(data_by_country)-1)){
    #predicted value vector
    pred_vec <- data_by_country[2:i,][[paste0("pred_", target)]]
    
    #lagged values
    lagged_tv <- data_by_country[1:(i-1), ][[paste0("tv_", target)]]
    
    #truth value vector
    data_tv1 <- data_by_country[2:i,][[paste0("tv_", target)]]
    
    #bind to matrix
    data_bqr <- data.frame(
      y = data_tv1,
      lagged_tv = lagged_tv,
      pred_vec = pred_vec
    )
    
    #last prediction, truth value and last lagged value of point after rolling window
    last_pred <- as.numeric(data_by_country[i+1,][[paste0("pred_", target)]])
    last_tv_lag <- as.numeric(data_by_country[i, ][[paste0("tv_", target)]])
    truth_value <- as.numeric(data_by_country[[paste0("tv_", target)]][i+1])
    
    #start date of rolling window
    forecast_year_start <- data_by_country[2,"forecast_year"]
    
    #forecast year of point after rolling window for prediction
    forecast_year_end <- data_by_country[i+1,"forecast_year"]
    
    #forecast quarter of point after rolling window for prediction
    forecast_quarter_end <- as.numeric(data_by_country[i+1,"forecast_quarter"])
    
    #target year of point after rolling window for prediction
    target_year_end <- data_by_country[i+1,"target_year"]
    
    #target quarter of point after rolling window for prediction
    target_quarter_end <- (forecast_quarter_end + 4 * h - 1) %% 4 +1
    
    #remove NAs
    data_bqr <- na.omit(data_bqr)
    
    cat(
      "\ni:", i,
      " rows:", nrow(data_bqr),
      " anyNA:", anyNA(data_bqr)
    )
    
    #fit lqr model 
    fit_l <- tryCatch({
      bayesQR::bayesQR(
        y ~ lagged_tv + pred_vec, 
        data = data_bqr, 
        quantile = (1 - tau) / 2, 
        ndraw = 5000,
        normal.approx = FALSE
      )
      
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    fit_u <- tryCatch({
      bayesQR::bayesQR(
        y ~ lagged_tv + pred_vec, 
        data = data_bqr, 
        quantile = (1 + tau) / 2, 
        ndraw = 5000,
        normal.approx = FALSE
      )
      
    },error=function(e){
      message("Fit failed for ", country, " (", 
              forecast_year_start, "–", forecast_year_end, "): ", e$message)
      NULL
    })
    
    if(is.null(fit_l)||is.null(fit_u)){
      next
    }
    
    sum_l <- tryCatch(summary(fit_l, burnin = 1000), error = function(e) NULL)
    sum_u <- tryCatch(summary(fit_u, burnin = 1000), error = function(e) NULL)
    
    betadraws_l <- sum_l[[1]]$betadraw
    betadraws_u <- sum_u[[1]]$betadraw
    
    if(is.null(betadraws_l) || !is.matrix(betadraws_l) || nrow(betadraws_l) == 0 ||
       is.null(betadraws_u) || !is.matrix(betadraws_u) || nrow(betadraws_u) == 0) {
      message("No beta draws at iteration ", i, ", skipping.")
      next
    }
    
    # Remove rows with NA
    betadraws_l <- betadraws_l[complete.cases(betadraws_l), ]
    betadraws_u <- betadraws_u[complete.cases(betadraws_u), ]
    
    # Skip if all rows removed
    if(nrow(betadraws_l) == 0 || nrow(betadraws_u) == 0) {
      message("All beta draws are NA at iteration ", i)
      next
    }
    
    if(is.null(betadraws_l)||is.null(betadraws_u)||!is.matrix(betadraws_l)||!is.matrix(betadraws_u)){
      message("No beta draws produced")
      next
    }
    
    #extract means of beta draws
    beta_l <- colMeans(betadraws_l, na.rm = TRUE)
    beta_u <- colMeans(betadraws_u, na.rm = TRUE)
    
    #prediction of quantile based on last point forecast
    pred_l <- beta_l[1] + beta_l[2] * last_tv_lag + beta_l[3] * last_pred
    pred_u <- beta_u[1] + beta_u[2] * last_tv_lag + beta_u[3] * last_pred
    
    #new row
    out_list[[index]] <- new_pred_row(
      country = country,
      forecast_year = forecast_year_end,
      target_year = target_year_end,
      target_quarter = target_quarter_end,
      target = target,
      horizon = h,
      tau = tau,
      lower_bound = pred_l,
      upper_bound = pred_u,
      truth_value = truth_value,
      prediction = last_pred
    )
    
    index <- index + 1
    
  }
  
  predictions <- bind_rows(out_list)
  
  return(predictions)
}



#'function draws n samples from an Inverse Gauss distribution 
#'using Michael-Shucany-Haas Algorithm
#'@param n sample size
#'@param mu mean
#'@param lambda shape of Inverse Gauss Distr.
rinvgauss <- function(n, mu, lambda){
  #draw from Chi-Squared 
  v <- rchisq(n, df = 1)
  
  w <- mu * v
  c <- mu / (2 * lambda)
  
  x1 <- mu + c * (w - sqrt(w * (4 * lambda + w)))
  p1 <- mu / (mu + x1)
  
  #draw y from uniform distr (0,1)
  y <- runif(n, 0, 1)
  
  x <- ifelse(y >= p1, mu * mu / x1, x1)
  
  return(x)
}


#'!Function not used in prediction
#'function estimates quantiles through a bayesian quantile regression model
#'
#'@description The BQR Model uses a normal-inverse-Gaussian representation of the
#'Asymmetric Laplace Distribution (Kotz et. al 1998 for mixture representation of ALD page 179) 
#'The MCMC Gibbs sampling method is used to 
#'iteratively sample the regression parameters. The mean of the sampled parameters is
#'outputed after discarding burn in samples
#'
#'Method from Carriero et. al 2025
#'
#'@param y dependent regression variable (i.e. truth value)
#'@param X regression matrix; may be dataframe (i.e. predicted value)
#'@param tau quantile to be estimated
#'@param n_iter number of samples to be calculated; default=3000
#'@param burn burn-in rate of samples to be discarded before calculating sample mean; default=500
#'@param beta0 initial mean of regression vector beta; default=NULL --> set in function to 0
#'@param B0 initial covariance matrix of regression vector beta; default=NULL --> set in 
#'function to diagonal matrix of 500 (high variance --> very little prior information) 
#'The prior distribution of beta is ~N(beta0, B0)
#'@param n0 prior parameter of inverse Gamma distribution of sigma InvG(n0,s0)
#'@param s0 prior parameter of inverse Gamma distribution of sigma InvG(n0,s0)
#'@param seed
#'
bqr <- function(y, X, p = 0.5, n_iter = 3000, burn = 500, beta0 = NULL, B0 = NULL, use_minesota = TRUE, lambda1 = 0.04, lambda2 = 0.25, n0 = 0, s0 = 0, seed = NULL){
  
  #=====================================
  #seed it specified
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  #=====================================
  #prepare data
  
  #dependent variable y
  y <- as.numeric(y)
  
  #Regressor matrix X
  if(is.data.frame(X)){
    X <- as.matrix(X)
  }
  if(!is.matrix(X)){
    stop("X must be Matrix or dataframe")
  }
  
  #add intercept
  X <- cbind(1, X)
  
  n <- length(y)
  k <- ncol(X)
  
  if(nrow(X) != n){
    stop("X must have same number of rows as y")
  }
  
  #=====================================
  #Minesota Prior or Prior with default values if not specified
  #Minesota Prior parameters as specified in Carriero et. al 2025
  
  #prior mean of beta always 0
  if(is.null(beta0)){
    beta0 <- rep(0, k)
  }
  
  if(use_minesota){
    
    #variance of dependent variable and regressor
    sy2 <- var(y)
    #variance for intercept and lagged regressor coefficent are defined independently 
    sx2 <- apply(X[, c(-1, -2), drop = FALSE], 2, var)
    
    prior_var <- numeric(k)
    
    #intercept variance uninformative
    prior_var[1] <- 1000 * sy2
    
    #prior of regression coefficient (first coefficient lagged value)
    prior_var[2] <- lambda1
    
    if(k > 2){
      prior_var[3:k] <- lambda1 * lambda2 * (sy2 / sx2)
    }
    
    #define prior Covariance matrix of beta
    B0 <- diag(prior_var)
    
  }else{
    #prior covariance matrix of beta 
    if(is.null(B0)){
      B0 <- diag(10, k)
    }  
  }
  
  #=====================================
  #Define constants of ALD mixture representation from Kotz et. al 1998
  #constants based on Kozumi und Kobayashi 2011
  
  theta <- (1 - 2 * p) / (p * (1 - p))
  tau2 <- 2 / (p * (1 - p))
  
  #=====================================
  #initialize storage for sampled parameters
  
  #storage for sampled regression parameters
  beta_storage <- matrix(NA_real_, n_iter, k)
  
  #storage for sampled scale parameter sigma
  sigma_storage <- numeric(n_iter)
  
  #=====================================
  #initial values for parameters beta and sigma
  
  beta <- rep(0, k)
  sigma <- var(y)
  z <- rep(1, n) #not needed for first draw with current sample order
  
  #=====================================
  #Gibbs sampling loop
  #Order:
  #1. sample latent variable v from inverse Gauss distribution 
  #--> function rinvgauss
  #conditional on beta, sigma
  #
  #2. sample sigma from inverse Gamma distribution 
  #
  #3. sample beta from normal distribution   
  for(i in 1:n_iter){
    
    #=====================================
    #1. draw v from general inverse Gauss 
    
    r <- as.numeric(y - X %*% beta)
    r2 <- pmax(r * r, .Machine$double.eps)
    
    #parameters for Inverse Gauss distribution
    delta2 <- r2/ (tau2 * sigma)
    gamma2 <- 2 / sigma + theta * theta / (tau2 * sigma)
    
    mu_w <- sqrt(gamma2 / pmax(delta2, .Machine$double.eps))
    lambda_w <- gamma2
    
    #draw w from inverse Gauss distribution
    w <- rinvgauss(n, mu = mu_w, lambda = lambda_w)
    w <- pmax(w, 1e-6)
    #take reciprocal value of w to get z (Jørgensen, B. (1982)) since z itself 
    #has conditional distribution of general inverse Gaussian GIG(1/2, a, b)
    #V ~ GIG(1/2, a, b) <-> 1/Z ~ IG(sqrt(b/a), b)
    #!note: Jørgensen 1982 and Kozumi & Kobayashi 2011 have different definitions for 
    #the parameters of the GIG density function. a, b from Jørgensen 1982 is equivalent 
    #delta^2 and gamma^2 respectively
    v <- 1/w
    #=====================================
    #2. draw sigma from inverse Gamma 
    
    res <- as.numeric(y - X %*% beta - theta * v)
    res2 <- pmax(res * res, .Machine$double.eps)
    
    #parameters for inverse Gamma distr. --> Kozumi & Kobayashi 2011
    n_sigma <- n0 + 3 * n
    s_sigma <- s0 + 2 * sum(v) + sum(res2 / (tau2 * v))
    
    #draw sigma from inverse Gamma by drawing from Gamma first and taking inverse
    inv_sigma <- rgamma(1, n_sigma / 2, s_sigma / 2)
    sigma <- 1 / pmax(inv_sigma, .Machine$double.eps)
    
    #=====================================
    #3. draw beta from Normal distribution
    
    v_inv_vec <- 1 / pmax(v, 1e-12)
    XV <- X * v_inv_vec
    
    #calculate posterior Cov-Matrix of beta
    B0_inv <- diag(1 / diag(B0))
    var_post_inv <- 1 / (tau2 * sigma) * crossprod(X,XV) + B0_inv
    var_post <- solve(var_post_inv)
    #ensure symmetry
    var_post <- (var_post + t(var_post)) / 2
    
    #calculate posterior mean of beta
    mean_post <- var_post %*% 
      (1 / (tau2 * sigma) * crossprod(X, (y - theta * v) * v_inv_vec) + B0_inv %*% beta0)
    
    #draw beta from multivariate Normal distribution (mvtnorm package required)
    if(!requireNamespace("mvtnorm", quietly = TRUE)){
      stop("mvtnorm package needed")
    }
    beta <- as.numeric(mvtnorm::rmvnorm(1, mean = as.numeric(mean_post), sigma = var_post))
    
    #=====================================
    #4. store sampled parameters
    
    beta_storage[i,] <- beta
    sigma_storage[i] <- sigma
    
    #=====================================
    #end of loop
  }
  
  #=====================================
  #return mean of sampled betas and sigma
  
  #discard burn-in
  index <- (burn+1):n_iter
  
  out_list <- list(
    beta_mean = colMeans(beta_storage[index, , drop = FALSE]),
    sigma_mean = mean(sigma_storage[index]),
    p = p,
    beta_draws = beta_storage[index,],
    sigma_draws = sigma_storage[index]
  )
  
  return(out_list)
}