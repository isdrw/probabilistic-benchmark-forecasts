# utility_functions.R
# Modular utility functions

# ===========================
## Scoring functions
# ===========================

#'calculate Interval Score
#'@description
#'Function calculates Interval Score based on given truth values lower
#'and upper bounds of Interval and level tau of Interval. 
#'
#'@note
#'input variables truth_value, lower_bound and upper_bound must be same length
#'
#'@param truth_value numeric vector of truth values
#'@param lower_bound numeric vector of lower bound of interval
#'@param upper_bound numeric vector of upper bound of interval
#'@param tau scalar value of prob of interval (i.e. 0.9 for 90% Interval)
#'
#'@return numeric vector of Interval Scores with same length as input vectors
interval_score <- function(truth_value, lower_bound, upper_bound, tau){
  
  #check for null vector
  if (is.null(truth_value) || is.null(lower_bound) ||
      is.null(upper_bound) || is.null(tau)) {
    return(NA_real_)
  }
  
  #check for same length vector
  n <- length(truth_value)
  if (length(lower_bound) != n || length(upper_bound) != n) {
    warning("All inputs must have the same length.")
    return(rep(NA_real_, n))
  }
  
  #non NA index
  valid <- !(is.na(truth_value) | is.na(lower_bound) | is.na(upper_bound))
  
  #output vector 
  IS <- rep(NA_real_,n)
  
  #data prep
  if(any(valid)){
    u <- upper_bound[valid]
    l <- lower_bound[valid]
    y <- truth_value[valid]
    tau_vec <- rep(tau,length(y))
    
    #interval score computation
    IS[valid] <- (u - l) + 2/(1 - tau_vec) * (l - y) * (y < l) + 2/(1 - tau) * (y - u) * (y > u)  
  }
  
  return(IS)
}

#'calculate Weighted Interval Score
#'@description
#'Function calculates weighted Interval Score based on given truth values lower
#'and upper bounds of Interval and set of levels of tau. 
#'
#'@note
#'input variables truth_value, lower_bound and upper_bound must be same dimensions
#'first vector of truth_value, lower_bound and upper_bound must be in line with first tau of tau set
#'
#'@param truth_value matrix n x k each column one tau
#'@param lower_bound matrix n x k each column one tau
#'@param upper_bound matrix n x k each column one tau
#'@param tau_set numeric vector of prob of intervals (i.e. c(0.5,0.9) for 50% and 90% Interval)
#'
#'@return numeric vector of weighted Interval Scores with same length as input vectors
weighted_interval_score <- function(truth_value, lower_bound, upper_bound, tau_set) {
  #number of taus
  k <- length(tau_set)
  
  #length of truth_values
  n <- nrow(truth_value)
  
  #check for valid tau
  if(k==0){
    warning("tau must be non empty vector of interval probs")
    return(rep(NA_real_, n))
  }
  
  #set of interval scores col=taus and row=interval scores 
  IS_mat <- matrix(NA_real_, nrow=n, ncol=k)
  
  #calculate interval scores for each tau
  for(i in 1:k){
    IS_mat[,i] <- interval_score(truth_value[,i], lower_bound[,i], upper_bound[,i], tau_set[i])
  }
  
  #weights 
  weights <- matrix((rep(1,k) - tau_set)/2, ncol=1)
  #weighted interval score computation
  WIS <- (IS_mat %*% weights) / (k+0.5)
  
  return(WIS)
}


# ===========================
## prediction output functions
# ===========================

#'create output dataframe for prediction of interval
#'@param interval bool value TRUE --> output dataframe for prediction of intervals
#'FALSE --> output dataframe for prediction of point
#'
#'@return initialized output dataframe 
init_output_df <- function(interval=TRUE) {
  df <- data.frame()
  if(interval){
    df <- data.frame(
      country = character(),
      forecast_year = numeric(),
      target_year = numeric(),
      target_quarter = numeric(),
      horizon = numeric(),
      target = character(),
      tau = numeric(),
      lower_bound = numeric(),
      upper_bound = numeric(),
      truth_value = numeric(),
      stringsAsFactors = FALSE
    )
  }else{
    df <- data.frame(
      country = character(),
      forecast_year = numeric(),
      target_year = numeric(),
      target_quarter = numeric(),
      horizon = numeric(),
      target = character(),
      prediction=numeric(),
      truth_value = numeric(),
      stringsAsFactors = FALSE
    )
  }
  return(df)
}

#'fill new row of prediction 
#'@param interval bool value TRUE --> output dataframe for prediction of intervals
#'FALSE --> output dataframe for prediction of point
#'
#'@return new filled row for output dataframe 
new_pred_row <- function(country, forecast_year, target_year, target, target_quarter=NA, 
                         horizon=NA, tau=NA, lower_bound=NA, 
                         upper_bound=NA, truth_value=NA, interval=TRUE, prediction=NA) {
  
  new_row <- data.frame()
  if(interval){
    new_row <- data.frame(
      country = country,
      forecast_year = forecast_year,
      target_year = target_year,
      target_quarter = target_quarter,
      horizon = horizon,
      target = target,
      tau = tau,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      truth_value = truth_value,
      stringsAsFactors = FALSE
    )
  }else{
    new_row <- data.frame(
      country = country,
      forecast_year = forecast_year,
      target_year = target_year,
      target_quarter = target_quarter,
      horizon = horizon,
      target = target,
      prediction = prediction,
      truth_value = truth_value,
      stringsAsFactors = FALSE
    )
  }
  return(new_row)
}

# ---------------------------
# 4. Data preparation helpers
# ---------------------------
prepare_lqr_input <- function(df, country, target, horizon_value) {
  sub <- df[df$country == country & df$target == target & df$horizon == horizon_value, , drop = FALSE]
  ensure_types(sub)
}

extract_window <- function(data, i, R, pred_col = "prediction", tv_col = "tv_1") {
  n <- nrow(data)
  start <- i - R + 1
  if(start < 1) stop("Window start < 1")
  pred_vec <- data[start:i, pred_col]
  tv_vec   <- data[start:i, tv_col]
  next_tv  <- if((i + 1) <= n) data[i + 1, tv_col] else NA_real_
  list(pred = pred_vec, tv1 = tv_vec, tv1_next = next_tv,
       forecast_year_end = data$forecast_year[i],
       target_year_end = data$target_year[i],
       end_row = i, n = n)
}

# ---------------------------
# 5. Model fitting observation based
# ---------------------------


#'compute prediction interval for method unconditional quantiles (observation based)
#'@description
#'function computes prediction interval for given set of observations and a level tau by
#'returning the respective unconditional quantiles
#'
#'@param obs numeric vector of observations
#'@param tau numeric value of tau 
#'@param n_ahead numeric value for number of predictions 
#'
#'@return lower and upper bound of predictions interval 
#'\describe{
#'   \item{pred_l}{Lower bound prediction}
#'   \item{pred_u}{Median prediction}
#' }
unconditional_quantiles <- function(obs, tau, n_ahead = 4) {
  #check for valid tau
  if (!is.numeric(tau) || tau <= 0 || tau >= 1) {
    stop("tau must be between 0 and 1")
  }
  
  #replace NAs with median 
  obs[is.na(obs)] <- median(obs,na.rm=TRUE)
  
  #prediction
  pred_l <- quantile(obs, probs = (1-tau)/2, type=7, na.rm=TRUE)
  pred_u <- quantile(obs, probs = (1+tau)/2, type=7, na.rm=TRUE)
  
  #same quantile prediction for all horizons
  preds_l <- rep(pred_l, n_ahead)
  preds_u <- rep(pred_u, n_ahead)
  
  return(list(
    preds_l = preds_l, 
    preds_u = preds_u
  ))
}


#'fit quantile autoregressive model QAR(p) and return fit (observation based)
#'@description
#'function fits three quantile autoregressive QAR(p) models on given observations based on given
#'level tau and returns fitted models with quantiles q_1 = (1-tau)/2; q_2 = =(1+tau)/2 and q_3 = 0.5
#'
#'@param obs numeric vector of observations
#'@param tau numeric value of tau
#'@param nlag numeric value for nlag=p of QAR(p) model; default p=1 
#'
#'@return fitted models for lower bound, upper bound and median 
#'\describe{
#'   \item{fit_l}{Lower bound QAR fit}
#'   \item{fit_u}{Upper bound QAR fit}
#'   \item{fit_m}{Median QAR fit}
#' }
fit_qar <- function(obs, tau, nlag=1) {
  #check if library installed and loaded
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is required but not installed.")
  }
  
  #check for sufficient length of obs
  if (length(obs) < nlag + 2) {
    warning("QAR(",nlag,") Model needs at least ", nlag+2, " observations to fit model")
    return(NULL)
  }
  
  #substitute NAs with median
  obs[is.na(obs)] <- median(obs,na.rm=TRUE)
  
  #lag data
  data_lagged <- lapply(1:nlag, function(L) dplyr::lag(obs, n = L))
  names(data_lagged) <- paste0("lag_",1:nlag)
  
  #dataframe for regression
  data_reg <- data.frame(
    y = obs, 
    do.call(cbind, data_lagged)
  )
  
  #remove NAs
  data_reg <- stats::na.omit(data_reg)
  
  #check for non empty dataframe
  if (nrow(data_reg) == 0) {
    warning("No valid rows after lagging & removing NA.")
    return(NULL)
  }
  
  #lower quantile
  fit_l <- tryCatch({
    quantreg::rq(formula = y ~ ., tau = (1-tau)/2, data = data_reg)
  },error=function(e){
    message("lower QAR fit failed ", e$message)
    NULL
  })
  
  #median for prediction 
  fit_m <- tryCatch({
    quantreg::rq(formula = y ~ ., tau = 0.5, data = data_reg)
  },error=function(e){
    message("median QAR fit failed ", e$message)
    NULL
  })
  
  fit_u <- tryCatch({
    quantreg::rq(formula = y ~ ., tau = (1+tau)/2, data = data_reg)
  },error=function(e){
    message("upper QAR fit failed ", e$message)
    NULL
  })
  
  return(list(
    fit_l = fit_l, 
    fit_u = fit_u, 
    fit_m = fit_m
  ))
}



#'predict quantile of fitted autoregressive models QAR(p) (observation based)
#'@description
#'function predicts quantiles of given fitted models for n steps ahead. For multiple predictions
#'median prediction is used as a new point prediction
#'
#'@param last_obs numeric vector of last observations (must be length of nlag of fitted models)
#'for nlag=2 must include obs_(t-1) and obs_(t-2)
#'@param fit_l lm object lower bound QAR fit 
#'@param fit_m lm object median QAR fit 
#'@param fit_u lm object upper bound QAR fit 
#'@param n_ahead numeric value for number of predicted values default=4 (for quarterly data)
#'
#'@note
#'models must be of same order; observation vector must contain last value/values
#'including respective lagged values; obs vector must be length of nlag of fitted models
#'
#'@return fitted models for lower bound, upper bound and median 
#'\describe{
#'   \item{pred_l}{Lower bound predictions}
#'   \item{pred_u}{Upper bound predictions}
#'   \item{pred_m}{Median QAR predictions}
#' }
predict_qar <- function(last_obs, fit_l, fit_m, fit_u, n_ahead=4) {

  if (is.null(fit_l) || is.null(fit_m) || is.null(fit_u)) {
    stop("All fit_l, fit_m, and fit_u must be non-null fitted QAR models.")
  }
  
  #order of models 
  nlag <- length(coef(fit_l))-1
  
  # convert to numeric lag vector
  last_obs <- as.numeric(last_obs)
  
  if (length(last_obs) != nlag) {
    stop("last_obs must have length equal to nlag used in model fitting.")
  }
  
  #prediction vectors
  pred_l <- numeric(n_ahead)
  pred_m <- numeric(n_ahead)
  pred_u <- numeric(n_ahead)
  
  # current lag state
  lag_vec <- last_obs
  
  for (h in 1:n_ahead) {
    
    #construct dataframe for prediction: one row, named lag_1, lag_2, ...
    new_data <- as.data.frame(t(lag_vec))
    names(new_data) <- paste0("lag_", 1:nlag)
    
    #predict lower, median, upper quantile
    pred_l[h] <- as.numeric(predict(fit_l, newdata = new_data))
    pred_m[h] <- as.numeric(predict(fit_m, newdata = new_data))
    pred_u[h] <- as.numeric(predict(fit_u, newdata = new_data))
    
    #update lag vector: recursive --> median prediction becomes new first lag
    lag_vec <- c(pred_m[h], head(lag_vec, nlag - 1))
  }
  
  return(list(
    pred_l = pred_l,
    pred_m = pred_m,
    pred_u = pred_u
  ))
}

# Requires quantreg
fit_lqr_models <- function(pred_vec, tv_vec, tau) {
  # build dataframe for rq: tv_vec ~ pred_vec
  df_fit <- data.frame(data_tv1 = tv_vec, data_pred = pred_vec)
  # remove NA rows
  ok <- complete.cases(df_fit)
  df_fit <- df_fit[ok, , drop = FALSE]
  if(nrow(df_fit) < 5) return(list(fit_l = NULL, fit_m = NULL, fit_u = NULL))
  tau_l <- (1 - tau) / 2
  tau_m <- 0.5
  tau_u <- (1 + tau) / 2
  fit_l <- tryCatch(quantreg::rq(data_tv1 ~ data_pred, tau = tau_l, data = df_fit), error = function(e) NULL)
  fit_m <- tryCatch(quantreg::rq(data_tv1 ~ data_pred, tau = tau_m, data = df_fit), error = function(e) NULL)
  fit_u <- tryCatch(quantreg::rq(data_tv1 ~ data_pred, tau = tau_u, data = df_fit), error = function(e) NULL)
  list(fit_l = fit_l, fit_m = fit_m, fit_u = fit_u)
}

predict_lqr <- function(fits, last_pred) {
  if(is.null(fits$fit_l) || is.null(fits$fit_m) || is.null(fits$fit_u)) return(list(lower = NA_real_, upper = NA_real_, median = NA_real_))
  newdata <- data.frame(data_pred = last_pred)
  lower <- tryCatch(as.numeric(predict(fits$fit_l, newdata = newdata)), error = function(e) NA_real_)
  upper <- tryCatch(as.numeric(predict(fits$fit_u, newdata = newdata)), error = function(e) NA_real_)
  median <- tryCatch(as.numeric(predict(fits$fit_m, newdata = newdata)), error = function(e) NA_real_)
  list(lower = lower, upper = upper, median = median)
}

# ---------------------------
# 6. Output row builder
# ---------------------------
create_output_row <- function(country, fy, ty, fq, h, target, tau, pred_l, pred_u, truth) {
  data.frame(
    country = country,
    forecast_year = fy,
    target_year = ty,
    target_quarter = fq,
    horizon = h,
    target = target,
    tau = tau,
    lower_bound = pred_l,
    upper_bound = pred_u,
    truth_value = truth,
    stringsAsFactors = FALSE
  )
}

# ---------------------------
# 7. Modular fit_lqr (uses the helpers)
# ---------------------------
# df must contain columns: country, target, horizon, prediction, tv_1, forecast_year, target_year, target_quarter
fit_lqr_modular <- function(df, tau = 0.6, target_name = "ngdp_rpch", R = 11, horizons = c(0.5, 1.0), pred_col = "prediction", tv_col = "tv_1") {
  if(!requireNamespace("quantreg", quietly = TRUE)) stop("Package 'quantreg' required. Install with install.packages('quantreg').")
  df <- ensure_types(df)
  predictions <- init_output_df()
  countries <- unique(df$country)
  for(country in countries) {
    for(h in horizons) {
      data_by_country <- prepare_lqr_input(df, country, target_name, h)
      if(nrow(data_by_country) < R + 1) next
      for(i in seq(R, nrow(data_by_country) - 1)) { # -1 because we access i+1 for truth
        win <- extract_window(data_by_country, i, R, pred_col = pred_col, tv_col = tv_col)
        if(all(is.na(win$pred)) || all(is.na(win$tv1))) next
        fits <- fit_lqr_models(win$pred, win$tv1, tau)
        if(is.null(fits$fit_l) || is.null(fits$fit_u) || is.null(fits$fit_m)) next
        last_pred <- tail(win$pred, 1)
        preds <- predict_lqr(fits, last_pred)
        # compute forecast target quarter (quarter arithmetic preserved)
        fq <- data_by_country$target_quarter[i]
        fy <- data_by_country$forecast_year[i]
        # next target year/quarter (for readability use the same logic as earlier)
        fq_h <- fq + 1
        fy_h <- fy + (fq_h - 1) %/% 4
        fq_h <- ((fq_h - 1) %% 4) + 1
        new_row <- create_output_row(country, fy, fy_h, fq_h, h, target_name, tau, preds$lower, preds$upper, win$tv1_next)
        predictions <- rbind(predictions, new_row)
      }
    }
  }
  predictions <- ensure_types(predictions)
  predictions
}

# ---------------------------
# 8. Small convenience: batch WIS per group
# ---------------------------
compute_WIS_by_group <- function(pred_df, taus_to_use = c(0.5, 0.8)) {
  if(!requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages 'dplyr' and 'tidyr' required.")
  }
  library(dplyr); library(tidyr)
  pred_df <- ensure_types(pred_df)
  # compute IS column if missing
  if(!"IS" %in% names(pred_df)) {
    pred_df$IS <- interval_score(pred_df$truth_value, pred_df$lower_bound, pred_df$upper_bound, pred_df$tau)
  }
  wide <- pred_df %>%
    filter(tau %in% taus_to_use) %>%
    pivot_wider(names_from = tau, values_from = IS, names_prefix = "IS_tau_") 
  # gather IS columns and taus
  is_cols <- grep("^IS_tau_", names(wide), value = TRUE)
  tau_vals <- as.numeric(gsub("IS_tau_", "", is_cols))
  wide$WIS <- apply(wide[is_cols], 1, function(ISrow) weighted_interval_score_multi(as.numeric(ISrow), tau_vals))
  wide
}

# End of utility_functions.R
