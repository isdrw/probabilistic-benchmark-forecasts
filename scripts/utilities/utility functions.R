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
  valid <- !(is.na(truth_value) || is.na(lower_bound) || is.na(upper_bound))
  
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
#'input variables truth_value, lower_bound and upper_bound must be same length
#'
#'@param truth_value numeric vector of truth values
#'@param lower_bound numeric vector of lower bound of interval
#'@param upper_bound numeric vector of upper bound of interval
#'@param tau_set numeric vector of prob of intervals (i.e. c(0.5,0.9) for 50% and 90% Interval)
#'
#'@return numeric vector of weighted Interval Scores with same length as input vectors
weighted_interval_score_multi <- function(truth_value, lower_bound, upper_bound, tau_set) {
  #number of taus
  k <- length(tau_set)
  if(k==0){
    warning("tau must be non empty vector of interval probs")
    return(rep(NA_real_, n))
  }
  #length of truth_values
  n <- length(truth_value)
  
  #set of interval scores col=taus and row=interval scores 
  IS_mat <- matrix(NA_real_, nrow=n, ncol=k)
  
  #calculate interval scores for each tau
  for(i in 1:k){
    IS_mat[,i] <- interval_score(truth_value, lower_bound, upper_bound, tau_set[i])
  }
  
  #weights 
  weights <- (rep(1,k) - tau_set)/2
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
new_pred_row <- function(country, forecast_year, target_year, target, forecast_qaurter=NA, 
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
  return(df)
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
# 5. Model fitting helpers
# ---------------------------


#'compute prediction interval for method unconditional quantiles (observation based)
#'@description
#'function computes prediction interval for given set of observations and a level tau by
#'returning the respective unconditional quantiles
#'
#'@param obs numeric vector of observations
#'@param tau numeric value of tau 
#'
#'@return lower and upper bound of prediction interval
unconditional_quantiles <- function(obs, tau) {
  if (!is.numeric(tau) || tau <= 0 || tau >= 1) {
    stop("tau must be between 0 and 1")
  }
  
  #prediction  
  lower_bound <- quantile(obs, probs=(1-tau)/2, type=7, na.rm=TRUE)
  upper_bound <- quantile(obs, probs=(1+tau)/2, type=7, na.rm=TRUE)
  
  return(c(lower_bound=lower_bound, upper_bound=upper_bound))
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
