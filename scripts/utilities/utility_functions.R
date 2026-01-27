# utility_functions.R


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
  
  #check valid input
  if (is.null(truth_value) || is.null(lower_bound) || is.null(upper_bound)) {
    return(NA_real_)
  }
  
  #ensure matrix shape
  truth_value <- as.matrix(truth_value)
  lower_bound <- as.matrix(lower_bound)
  upper_bound <- as.matrix(upper_bound)
  
  #number of taus
  k <- length(tau_set)
  
  #length of truth_values
  n <- nrow(truth_value)
  
  #check matrix dimensions
  if(!all(dim(truth_value) == dim(lower_bound), dim(truth_value) == dim(upper_bound))){
    stop("truth_value, lower_bound and upper_bound must all have same dimensions")
  }
  
  #check for valid tau
  if(k==0){
    warning("tau must be non empty vector of interval probs")
    return(rep(NA_real_, n))
  }
  
  #set of interval scores col=taus and row=interval scores 
  IS_mat <- matrix(NA_real_, nrow = n, ncol = k)
  
  #calculate interval scores for each tau
  for(i in seq_len(k)){
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
  
  taus_chr <- as.character(taus)
  
  tmp <- df %>%
    dplyr::mutate(tau = as.character(tau)) %>%
    dplyr::filter(tau %in% taus_chr) %>%
    dplyr::group_by(tau) %>%
    dplyr::summarise(vals = list(.data[[column]]), .groups = "drop") %>%
    dplyr::arrange(tau)
  
  if (nrow(tmp) == 0) {
    return(matrix(NA_real_, nrow = 0, ncol = length(taus)))
  }
  
  mats <- tmp$vals
  
  # ensure all elements are vectors
  if (any(vapply(mats, is.null, logical(1)))) {
    return(matrix(NA_real_, nrow = 0, ncol = length(taus)))
  }
  
  do.call(cbind, mats)
}



calc_WIS_of_df <- function(df, taus = c(0.5, 0.8)){
  #lower_bound matrix
  lower_bound_mat <- make_tau_matrix(df = df, "lower_bound", taus = taus)
  
  #upper_bound matrix
  upper_bound_mat <- make_tau_matrix(df = df, "upper_bound", taus = taus)
  
  #truth_value matrix
  truth_value_mat <- make_tau_matrix(df = df, "truth_value", taus = taus)
  
  if (is.null(lower_bound_mat) || is.null(upper_bound_mat) || 
      is.null(truth_value_mat) || nrow(lower_bound_mat) == 0) {
    return(NA_real_)
  }
  
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


summarise_eval <- function(df){
  #coverage
  coverage_df <- df %>% 
    summarise_coverage_of_df()
  
  #interval scores
  IS_df <- df %>% 
    summarise_IS_of_df()
  
  #weighted interval scores
  WIS_df <- df %>%
    summarise_WIS_of_df()
  
  #combine results into one dataframe
  summary_df <- coverage_df %>%
    dplyr::left_join(IS_df, by = c("target", "tau")) %>% 
    dplyr::left_join(WIS_df, by = c("target"))
  
  return(summary_df)
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

#'function fits t distribution on vector x
#'
#'@param x numeric vector of observations
#'@param mean numeric value of default mean (0) to be returned if fit fails
#'@param sd numeric value of default standard deviation (1) to be returned if fit fails
#'@param df numeric value of default degree of freedom (10 for moderate heavy tails)
fit_t_distribution <- function(x,mean = 0, sd = 1, df = 10){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean)<3 || sd(x_clean)==0){
    #return standard t distribution
    return(list(mean = mean, sd = sd, df = df))
  }
  
  #start list for optimizer 
  start_vals <- list(
    m = mean(x_clean),
    s = sd(x_clean),
    df = 10
  )
  
  fit_t <- tryCatch(
    MASS::fitdistr(x_clean, densfun = "t", start = start_vals),
    error = function(e){
      warning("fitdist failed to fit t distribution: ", conditionMessage(e))
      NULL
    }
  )
  
  if(is.null(fit_t)){
    return(list(mean = mean, sd = sd, df = df))
  }
  
  return(list(
      mean = fit_t$estimate["m"],
      sd = fit_t$estimate["s"],
      df = fit_t$estimate["df"]
    )
  )
}


#'function fits skewed t distribution on vector x
#'
#'@note Package fGarch required
#'@param x numeric vector of observations
#'@param mean numeric value of default location parameter (0) to be returned if fit fails
#'@param sd numeric value of default scale parameter (1) to be returned if fit fails
#'@param nu numeric value of default shape (10 df) parameter to be returned if fit fails
#'@param xi numeric value of default skewness (1.5)
fit_skewed_t_distribution <- function(x, mean = 0, sd = 1, nu = 10, xi = 1.5){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean) < 5 || sd(x_clean)==0){
    #return default parameters
    return(list(mean = mean, sd = sd, nu = nu, xi = xi))
  }
  
  #Package fGarch required
  fit_skewed_t <- tryCatch(
    fGarch::sstdFit(x_clean),
    error = function(e){
      warning("fitdist failed to fit t distribution: ", conditionMessage(e))
      NULL
    }
  )
  
  if(is.null(fit_skewed_t)){
    return(list(mean = mean, sd = sd, nu = nu, xi = xi))
  }
  
  return(list(
    mean = fit_skewed_t$estimate["mean"],
    sd = fit_skewed_t$estimate["sd"],
    nu = fit_skewed_t$estimate["nu"],
    xi = fit_skewed_t$estimate["xi"]
  )
  )
}


#'function fits Asymmetric Laplace distribution on vector x
#'
#'@param x numeric vector of observations
fit_ald_distribution <- function(x){
  #remove NAs and infinite values
  x_clean <- x[!is.na(x)&is.finite(x)]
  
  #check for singular value and/or sd == 0
  if(length(x_clean) < 3 || sd(x_clean)==0){
    #return default parameters
    return(list(mu = 0, sigma = 1, p = 0.5))
  }
  
  fit <- tryCatch(
    ald::mleALD(x_clean),
    error = function(e){
      warning("fitdist failed to fit ALD: ", conditionMessage(e))
      NULL
    }
  )
  
  mu_hat <- fit$par[1]
  sigma_hat <- fit$par[2]
  p <- fit$par[3]
  
  #return estimated parameters
  return(
    list(
      mu = mu_hat,
      sigma = sigma_hat,
      p = p
    )
  )
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
## data transformation/ correction
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



#'function calculates value of check loss function from residual u and quantile level tau
#'
#'@param u residual (x - x_hat)
#'@param tau quantile level 
check_loss <- function(u, tau){
  ifelse(u >= 0, tau * u, (1 - tau) * (-u))
}


aggregate_to_annual_input <- function(df){
  
  #triangular weights
  weights <- as.numeric((1 - abs(4 - 1:7) / 4))
  
  df %>% 
    #index for combination of target_year and target_quarter
    dplyr::mutate(
      tq_index = 4 * target_year + target_quarter
    ) %>%
    
    dplyr::group_by(country, horizon) %>%
    
    dplyr::group_modify(~{
      
      #target years of group
      target_years <- sort(unique(.x$target_year))
      
      out_list <- list()
      
      for(t in target_years){
        #from Q2 in year t-1 to Q4 in year t
        required_index <- (4 * (t-1) + 2):(4 * t + 4)
        
        #window for aggregation 
        window <- .x %>% 
          filter(tq_index %in% required_index) %>%
          arrange(tq_index)
        
        #return empty tibble in case of insufficient amount of predicted quarters
        if(nrow(window) != 7){
          next
        }
        
        #convert to log growth
        log_pred_cpi <- log1p(window$pred_cpi / 100) 
        log_pred_gdp <- log1p(window$pred_gdp / 100)
        log_tv_cpi <- log1p(window$tv_cpi / 100)
        log_tv_gdp <- log1p(window$tv_gdp / 100)
        
        #temporal aggregation of quarterly lower, upper bound and truth value
        pred_cpi_annual <- 100 * sum(weights * log_pred_cpi)
        pred_gdp_annual <- 100 * sum(weights * log_pred_gdp)
        tv_cpi_annual <- 100 * sum(weights * log_tv_cpi)
        tv_gdp_annual <- 100 * sum(weights * log_tv_gdp)
        
        out_list[[as.character(t)]] <- tibble(
          country = window$country[1],
          forecast_year = window$forecast_year[7],
          forecast_quarter = window$forecast_quarter[7],
          target_year = t,
          target_quarter = window$target_quarter[7],
          horizon = window$horizon[1],
          pred_cpi = pred_cpi_annual,
          pred_gdp = pred_gdp_annual,
          tv_cpi = tv_cpi_annual,
          tv_gdp = tv_gdp_annual
        )
      }
      
      bind_rows(out_list)
      
    }) %>% 
    
    dplyr::ungroup() 
}


# ---------------------------
#  distribution/ sampling methods
# ---------------------------

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

#'function simulates response variable y based on fitted 0.5-BQR model and its respective
#'draws of beta and sigma
#'
#'@param fit fitted BQR model from function bqr
#'@param x_star vector of regressor variable (1, x1, ..., xk); must include intercept 1
predict_bqr <- function(fit, x_star){
  
  #extract parameter draws from fit
  beta_draws <- fit$beta_draws
  sigma_draws <- fit$sigma_draws
  p <- fit$p
  
  #parameters of ALD mixture representation
  theta <- (1 - 2 * p) / (p * (1 - p))
  tau2 <- 2 / (p * (1 - p))
  
  n <- nrow(beta_draws)
  
  #vector for calculated ys
  y_hat <- numeric(n)
  
  for(i in 1:n){
    beta <- beta_draws[i,]
    sigma <- sigma_draws[i]
    
    #sample z from exponential Distribution E(1/sigma) 
    z <- rexp(1, rate = 1)
    
    #sample u from standard normal distribution
    u <- rnorm(1,0,1)
    
    #caclulate y from ALD mixture representation
    y_hat[i] <- sum(beta * x_star) + theta * z + sqrt(tau2 * sigma * z) * u
  }
  
  return(y_hat)
}


#'EasyUQ isotonic distributional regression
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
  
  for(j in seq_len(m)){
    #value of indicator function y <= y_j (threshold)
    z <- as.numeric(y <= y_grid[j])
    
    #linear part of objective function as defined in doc of OSQP solver
    q <- -z
    
    #solve quadratic problem with osqp solver
    solver <- osqp::osqp(
      P = P,
      q = q,
      A = A,
      l = l,
      u = u
    )
    
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

#'function returns estimated cdfs of fitted model given vector x_new of new
#'values via linear interpolation
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

fit_easyUQ <- function(x, y, tau = 0.8, x_new) {
  # Remove NAs
  valid <- !(is.na(x) | is.na(y))
  x <- x[valid]
  y <- y[valid]
  
  # Check minimal conditions
  if (length(x) < 3 || length(unique(x)) < 2 || length(unique(y)) < 2 || is.na(x_new)) {
    return(NULL)
  }
  
  # Define lower and upper quantile probabilities
  probs <- c((1 - tau)/2, (1 + tau)/2)
  
  tryCatch({
    # Fit IDR: x must be data.frame, y must be numeric
    idr_fit <- isodistrreg::idr(y = y, X = data.frame(x = x))
    
    # Ensure x_new is a data.frame
    x_new_df <- if (!is.data.frame(x_new)) data.frame(x = x_new) else x_new
    
    # Predict using IDR
    pred <- predict(idr_fit, data = x_new_df)
    
    # Extract quantiles
    pred_q <- qpred(pred, quantiles = probs)
    
    # Return numeric results
    if (is.vector(pred_q)) {
      list(
        lower_bound = pred_q[1],
        upper_bound = pred_q[2]
      )
    } else {
      # For multiple predictions
      data.frame(
        lower_bound = pred_q[, 1],
        upper_bound = pred_q[, 2]
      )
    }
    
  }, error = function(e) {
    message("IDR error: ", e$message)
    NULL
  })
}


