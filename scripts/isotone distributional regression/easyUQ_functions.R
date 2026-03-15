#==========================================================
#Functions for the EasyUQ isotone distributional regression
#==========================================================


#'EasyUQ isotonic distributional regression
#'
#'@param x vector of point forecast
#'@param y vector of truth values
#'
#'@return F_hat = Matrix of density functions for each value of x, defined at 
#'discrete values of y. full Rows = density function of y for given x. Columns = unique values of y
#'
easyUQ_idr <- function(x, y){
  
  #sort value pairs by order of x vector
  order_x <- order(x)
  x <- x[order_x]
  y <- y[order_x]
  n <- length(y)
  
  #threshold values of y
  y_grid <- sort(unique(y))
  m <- length(y_grid)
  #check for multiple different values
  if(m < 2){
    stop("y must contain at least two different non-NA values")
  }
  
  if(n < 2){
    stop("at least two values of x needed for isotonic constraints")
  }
  
  #Matrix for quadratic term in objective function (defined for usage in OSQP solver)
  P <- Diagonal(n)
  
  #constraint Matrix A: theta_i - theta_(i+1) >= 0 for all i <==> A * theta >= 0
  A <- cbind(Diagonal(n-1), Matrix(0, nrow = n-1, ncol = 1)) - 
    cbind(Matrix(0, nrow = n-1, ncol = 1), Diagonal(n-1))
  #lower bound of constraint l
  l <- rep(0, n-1)
  #upper bound of constraint inf (no bound)
  u <- rep(Inf, n-1)
  
  #objective matrix with n rows for each x and m cols for each threshold of y
  F_hat <- matrix(NA_real_, nrow = n, ncol = m)
  
  settings <- osqp::osqpSettings(verbose = FALSE, warm_start = TRUE, max_iter = 2000)
  
  #initialize solver once 
  solver <- osqp::osqp(
    P = P,
    q = rep(0, n),
    A = A,
    l = l,
    u = u,
    pars = settings
  )
  
  for(j in seq_len(m)){
    #value of indicator function y <= y_j (threshold)
    z <- as.numeric(y <= y_grid[j])
    
    #linear part of objective function as defined in doc of OSQP solver
    q <- -z
    
    solver$Update(q = q)
    
    #extract solution
    F_hat[,j] <- solver$Solve()$x
  }
  
  #return solution
  list(
    x = x,
    y_grid = y_grid,
    F_hat = F_hat
  )
  
}

#'EasyUQ isotonic distributional regression
#'(using PAV algorithm)
#'@param x vector of point forecast
#'@param y vector of truth values
#'
#'@return F_hat = Matrix of density functions for each value of x, defined at 
#'discrete values of y. full Rows = density function of y for given x. Columns = unique values of y
#'
easyUQ_idr_pav <- function(x, y){
  
  #sort value pairs by order of x vector
  order_x <- order(x)
  x <- x[order_x]
  y <- y[order_x]
  n <- length(y)
  
  #threshold values of y
  y_grid <- sort(unique(y))
  m <- length(y_grid)
  #check for multiple different values
  if(m < 2){
    stop("y must contain at least two different non-NA values")
  }
  
  if(n < 2){
    stop("at least two values of x needed for isotonic constraints")
  }
  
  
  #objective matrix with n rows for each x and m cols for each threshold of y
  F_hat <- matrix(NA_real_, nrow = n, ncol = m)
  
  index <- seq_len(n)
  
  for(j in seq_len(m)){
    #value of indicator function y <= y_j (threshold)
    z <- as.numeric(y <= y_grid[j])
    
    fit <- isotone::gpava(
      z = index,
      y = -z,
      weights = rep(1,n)
    )
    
    
    #extract solution
    F_hat[,j] <- - fit$x
  }
  
  #return solution
  list(
    x = x,
    y_grid = y_grid,
    F_hat = F_hat
  )
  
}

#'function returns estimated cdfs of fitted model given vector x_new of new
#'values via linear interpolation
#'@param model fitted EasyUQ model from easyUQ_idr function
#'@param x_new new x for which density function should be estimated
#'
#'@return density funtion of y for given x_new
interpolate_easyUQ_cdf <- function(model, x_new){
  
  #check for non existent value of x_new
  if(is.null(x_new) || all(is.na(x_new))){
    return(NULL)
  }
  
  #extract estimated distributions from model
  x <- model$x
  F_hat <- model$F_hat
  
  F_new <- matrix(NA_real_, nrow = length(x_new), ncol = ncol(F_hat))
  
  #for each new x interpolation of estimated cdf as described in Walz et. al 2024
  for(j in seq_len(ncol(F_hat))){
    #rule 2 ensures cases 
    #1. case: x_new < smallest x from model input --> F_x_1(y)
    #2. case: x_new > biggest x from model input --> F_x_n(y)
    F_new[, j] <- approx(x = x, y = F_hat[, j], xout = x_new, rule = 2)$y
  }
  
  return(F_new)
}


#'function calculates specified quantiles of fitted EasyUQ model based on new data vector x
#'1. Linear interpolation based on x_new to obtain full density function
#'2. Linear interpoltation based on quantile levels calculate quantiles
#'
#'@param model fitted EasyUQ model from EasyUQ_idr function
#'@param x_new new x for which density function should be estimated
#'@param probs vector of quantile levels
predict_easyUQ_quantiles <- function(model, x_new, probs){
  
  #check for non existent value of x_new
  if(is.null(x_new) || all(is.na(x_new))){
    return(NULL)
  }
  
  #calculate new cdfs based on x_new 
  F_new <-  interpolate_easyUQ_cdf(model = model, x_new = x_new)
  y_grid <- model$y_grid
  
  quants <- matrix(NA_real_, nrow = length(x_new), ncol = length(probs))
  
  for(i in seq_len(nrow(F_new))){
    #check for insufficient data
    valid <- !(is.na(F_new[i, ]) | is.na(y_grid))
    
    if(sum(valid) < 2 || length(unique(F_new[valid])) < 2){
      next
    }
    
    #linear interpolation: 
    quants[i, ] <- approx(x = F_new[i, valid], y = y_grid[valid], xout = probs, rule = 2)$y
  }
  
  return(quants)
}


#'Function fits EasyUQ model to data and constructs lower and upper bounds of a 
#'forecast interval based on specified confidence level
#'
#'@param x vector of point forecasts
#'@param y vector of truth values
#'@param tau confidence level of forecast intervals
#'@param x_new new value of point forecast on the basis of which the forecast is made
#'
#'@return lower_bound and upper_bound of the forecast interval
fit_easyUQ <- function(x, y, tau, x_new){
  #fit model on data
  fit <- easyUQ_idr(x, y)
  
  probs <- c((1 - tau) / 2, (1 + tau) / 2)
  #caclulate quantile of y for new values of x
  pred <- predict_easyUQ_quantiles(model = fit, x_new = x_new, probs = probs)
  
  return(list(
    lower_bound = pred[,1],
    upper_bound = pred[,2]
  ))
}


#=================================================================
# Loop function
#=================================================================

#'
#'Function iterates over df using an expanding window and calculates prediction intervals
#'for a specified confidence level method = EasyUQ
#'
#'@param df dataframe with columns: country; target_year; horizon; forecast_year; 
#'pred_gdp; pred_cpi; tv_gdp; tv_cpi
#'@param country String of country name to be filtered 
#'@param tau confidence level of forecast intervals
#'@param target target variable to be filtered ("gdp", "cpi")
#'@param h horizon (0.0, 0.5, 1.0, 1.5)
#'
fit_easyUQ_idr <- function(df, country, tau, target, h){
  
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
    pred_vec <- data_by_country[1:i,][[paste0("pred_", target)]]
    
    #truth value vector
    data_tv1 <- data_by_country[1:i,][[paste0("tv_", target)]]
    
    #last prediction, truth value of point after current window
    last_pred <- as.numeric(data_by_country[[paste0("pred_", target)]][i+1])
    truth_value <- as.numeric(data_by_country[[paste0("tv_", target)]][i+1])
    
    #forecast year of point after expanding window for prediction
    forecast_year_end <- data_by_country[["forecast_year"]][i + 1]
    
    #forecast quarter of point after expanding window for prediction
    forecast_quarter_end <- as.numeric(data_by_country[["forecast_quarter"]][i + 1])
    
    #target year of point after expanding window for prediction
    target_year_end <- data_by_country[["target_year"]][i + 1]
    
    #target quarter of point after expanding window for prediction
    target_quarter_end <- (forecast_quarter_end + 4 * h - 1) %% 4 +1
    
    #skip if only NAs
    if(all(is.na(pred_vec)) || is.null(pred_vec) || 
       all(is.na(data_tv1)) || is.null(data_tv1)){
      message("no valid data for ", country, "\nfor target year: ", target_year_end)
      next
    }
    
    #fit EasyUQ model
    fit <- tryCatch({
      fit_easyUQ(x = pred_vec, y = as.numeric(data_tv1), tau = tau, x_new = last_pred)
    }, error = function(e){
      message("Fit failed for ", country, "\n for target year: ", target_year_end, "\n", e$message)
      NULL
    })
    
    if(is.null(fit)){
      next
    }
    
    pred_l <- tryCatch({
      fit$lower_bound[1]
    }, error = function(e){
      message("lower not computed for, ", country, "\n target year: ", target_year_end, "\n", e$message)
      NA_real_
    })
    
    pred_u <- tryCatch({
      fit$upper_bound[1]
    }, error = function(e){
      message("lower not computed for, ", country, "\n target year: ", target_year_end, "\n", e$message)
      NA_real_
    })
    
    #check for false lengths 
    if (!all(
      length(as.numeric(pred_l)) == 1,
      length(as.numeric(pred_u)) == 1,
      length(last_pred) == 1,
      length(truth_value) == 1
    )) {
      next
    }
    
    #new row
    out_list[[index]] <- new_pred_row(
      country = country,
      forecast_year = as.numeric(forecast_year_end),
      target_year = as.numeric(target_year_end),
      target_quarter = as.numeric(target_quarter_end),
      target = target,
      horizon = h,
      tau = tau,
      lower_bound = as.numeric(pred_l),
      upper_bound = as.numeric(pred_u),
      truth_value = as.numeric(truth_value),
      prediction = as.numeric(last_pred)
    )
    
    index <- index + 1
    
  }
  
  if(length(out_list) == 0){
    return(predictions)  
  }
  
  bind_rows(out_list)
}