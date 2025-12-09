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


#'functions calculates interval scores of given prediction dataframe
#'@description The function takes in a prediction dataframe with columns 
#'truth_value, lower_bound, upper_bound and tau (long format) and adds a column IS 
#'for the interval score of each row
#'
#'@note df must contain aforementioned columns 
#'
#'@param df prediction dataframe 
calc_IS_of_df <- function(df){
  df %>%
    group_by(tau) %>%
    mutate(
      IS = interval_score(truth_value, lower_bound, upper_bound, tau[1])
    ) %>%
    ungroup()
}


#'function calculates mean interval scores for all taus
#'
#'@note dataframe must contain column IS --> function calc_IS_of_df
#'@param df prediction dataframe
summarise_IS_of_df <- function(df){
  df %>% 
    group_by(target, tau) %>%
    summarise(
      mean_IS = round(mean(IS, na.rm = TRUE),6),
      .groups = "drop"
    )
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

#'function creates matrix for given set of taus and column name
#'@description The function creates a matrix with dimensions length(column) X length(taus)
#'where each column is a vector of the specified column name from the dataframe df 
#'for each tau in taus the respective column of the dataframe is pulled
#'The Matrix is used for the calculation of the WIS with the function 
#'weighted_interval_score(truth_value, lower_bound, upper_bound, tau_set)
#'@param df dataframe of predictions
#'@param column name to be pulled
#'@param taus set of taus to be 
make_tau_matrix <- function(df, column, taus) {
  #convert taus to character because of comparison issues of numeric values (0.3 and 0.6)
  taus <- as.character(taus)
  
  df %>%
    dplyr::mutate(tau = as.character(tau)) %>%
    dplyr::filter(tau %in% taus) %>%
    dplyr::group_by(tau) %>%
    dplyr::summarise(vals = list(.data[[column]]), .groups = "drop") %>%
    dplyr::arrange(tau) %>%
    dplyr::pull(vals) %>%
    do.call(cbind, .)
}


calc_WIS_of_df <- function(df, taus = c(0.5, 0.8)){
  #lower_bound matrix
  lower_bound_mat <- make_tau_matrix(df = df, "lower_bound", taus = taus)
  
  #upper_bound matrix
  upper_bound_mat <- make_tau_matrix(df = df, "upper_bound", taus = taus)
  
  #truth_value matrix
  truth_value_mat <- make_tau_matrix(df = df, "truth_value", taus = taus)
  
  #calculate WIS vector
  WIS <- weighted_interval_score(
    lower_bound = lower_bound_mat, 
    upper_bound = upper_bound_mat, 
    truth_value = truth_value_mat, 
    tau_set = taus
  )
  
  return(WIS)
}


summarise_WIS_of_df <- function(df){
  #WIS for 50% and 80% intervals per target
  WIS_58 <- df %>%
    group_by(target) %>%
    group_modify(~{
      wis_vec <- calc_WIS_of_df(.x, taus = c(0.5, 0.8))
      tibble(WIS_58 = round(mean(as.numeric(wis_vec), na.rm = TRUE), 5))
    })
  
  #WIS for 10% ... 90% intervals per target
  WIS_all <- df %>%
    group_by(target) %>%
    group_modify(~{
      wis_vec <- calc_WIS_of_df(.x, taus = seq(0.1, 0.9, 0.1))
      tibble(WIS_all = round(mean(as.numeric(wis_vec), na.rm = TRUE), 5))
    })
  
  #Combine into one tibble
  result <- left_join(WIS_58, WIS_all, by = "target")
  
  return(result)
}



is_covered <- function(df){
  df %>% 
    mutate(
      covered = truth_value >= lower_bound &
        truth_value <= upper_bound
    )
}

summarise_coverage_of_df <- function(df){
  df %>% 
    group_by(target, tau) %>%
    summarise(
      coverage = round(mean(covered, na.rm = TRUE),6),
      .groups = "drop"
    )
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
                         forecast_quarter = NA, horizon=NA, tau=NA, lower_bound=NA, 
                         upper_bound=NA, truth_value=NA, interval=TRUE, prediction=NA) {
  
  new_row <- data.frame()
  if(interval){
    new_row <- data.frame(
      country = country,
      forecast_year = forecast_year,
      forecast_quarter = forecast_quarter,
      target_year = target_year,
      target_quarter = target_quarter,
      horizon = horizon,
      target = target,
      tau = tau,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      truth_value = truth_value,
      prediction = prediction,
      stringsAsFactors = FALSE
    )
  }else{
    new_row <- data.frame(
      country = country,
      forecast_year = forecast_year,
      forecast_quarter = forecast_quarter,
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
# 5. Model fitting observation based
# ---------------------------

#'function fits normal distribution on vector x
#'
#'@param x numeric vector of observations
#'@param mean numeric value of default mean (0) to be returned if fit fails
#'#'@param sd numeric value of default standard deviation (1) to be returned if fit fails
fit_normal_distribution <- function(x,mean = 0,sd = 1){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean)<2 || sd(x_clean)==0){
    #return standard normal distribution
    return(list(estimate=c(mean = mean, sd = sd)))
  }
  
  fit_n <- tryCatch(
    fitdist(x_clean,distr = "norm"),
    error = function(e){
      warning("fitdist failed to fit normal distribution: ", conditionMessage(e))
      #return standard normal distribution
      return(list(estimate=c(mean = mean, sd = sd)))
    }
  )
  
  return(fit_n)
}

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

# ---------------------------
## PAVA correction function
# ---------------------------

#'function applies pava-type algorithm on x
#'@desctiption function replaces points that violate isotonicity 
#'with their pairwise mean and repeats until no violations are found within given tolerance;
#'This approach will lead to same weights within groups of violations, 
#'i.e. group x_i,...,x_i+h violate isotonicity will all be replaced with roughly
#'1/h * sum(x_i,...,x_i+h)
#'
#'@note function not suitable for large amount of data
#'@param x numeric vector of data
#'@param increasing boolean, TRUE --> x_i <= x_i+1, FALSE --> x_i >= x_i+1
#'@param tolerance numeric value of tolerance to which comparison will be evaluated

pava_correction <- function(x, increasing=TRUE, tolerance=1e-12){
  na_idx <- is.na(x)
  x_clean <- x[!na_idx]
  n <- length(x_clean)
  
  if(n <= 1){
    return(x)
  }
  
  repeat{
    violations_found <- FALSE
    for(i in seq_len(n-1)){
      #increasing order
      if(increasing && (x_clean[i] - x_clean[i+1] > tolerance)){
        x_clean[c(i,i+1)] <- mean(c(x_clean[i],x_clean[i+1]))
        violations_found <- TRUE
      }  
      #decreasing order 
      if(!increasing && (x_clean[i+1] - x_clean[i] > tolerance)){
        x_clean[c(i,i+1)] <- mean(c(x_clean[i],x_clean[i+1]))
        violations_found <- TRUE
      }
      
    }
    if(violations_found == FALSE){
      break
    }
  }
  x_out <- x
  x_out[!na_idx] <- x_clean
  
  return(x_out)
}


#'function applies PAVA algorithm on interval bounds of prediction dataframe
#'
#'@note prediction dataframe must contain columns country, target, forecast_year, 
#'tau, lower_bound, upper_bound, prediction
#'
#'@param df prediction dataframe
pava_correct_df <- function(df){
  df %>% 
    group_by(country, target, forecast_year, tau) %>%
    group_modify(~{
      #pava corrections for interval widths over all horizons
      interval_widths <- .x$prediction - .x$lower_bound
      interval_widths <- pava_correction(interval_widths)
      
      #update bounds
      .x$lower_bound <- .x$prediction - interval_widths
      .x$upper_bound <- .x$prediction + interval_widths
      
      .x
    }) %>% ungroup()
}
