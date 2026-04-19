# utility_functions.R


# ===========================
## Scoring functions
# ===========================

#'!!! Generative AI; level = low --> debugging
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
#!!!


#'!!! Generative AI; level = low --> debugging
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
    group_by(target, horizon, tau) %>%
    summarise(
      mean_IS = round(mean(IS, na.rm = TRUE),6),
      .groups = "drop"
    )
}
#!!!

#'!!! Generative AI; level = low --> debugging
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
#!!!

#'!!! Generative AI; level = low --> debugging
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
#!!!

#'!!! Generative AI; level = low --> debugging
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
#!!!

#'!!! Generative AI; level = low --> debugging
summarise_WIS_of_df <- function(df){
  #WIS for 50% and 80% intervals per target
  WIS_58 <- df %>%
    group_by(target, horizon) %>%
    group_modify(~{
      wis_vec <- calc_WIS_of_df(.x, taus = c(0.5, 0.8))
      tibble(WIS_58 = round(mean(as.numeric(wis_vec), na.rm = TRUE), 5))
    })
  
  #WIS for 10% ... 90% intervals per target
  WIS_all <- df %>%
    group_by(target, horizon) %>%
    group_modify(~{
      wis_vec <- calc_WIS_of_df(.x, taus = seq(0.1, 0.9, 0.1))
      tibble(WIS_all = round(mean(as.numeric(wis_vec), na.rm = TRUE), 5))
    })
  
  #Combine into one tibble
  result <- left_join(WIS_58, WIS_all, by = c("target", "horizon"))
  
  return(result)
}
#!!!


is_covered <- function(df){
  df %>% 
    mutate(
      covered = truth_value >= lower_bound &
        truth_value <= upper_bound
    )
}

summarise_coverage_of_df <- function(df){
  df %>% 
    group_by(target, horizon, tau) %>%
    summarise(
      coverage = round(mean(covered, na.rm = TRUE),6),
      .groups = "drop"
    )
}

#'!!! Generative AI; level = medium --> debugging + dpylr function syntax
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
    dplyr::left_join(IS_df, by = c("target", "horizon", "tau")) %>% 
    dplyr::left_join(WIS_df, by = c("target", "horizon"))
  
  return(summary_df)
}
#!!!

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
## data transformation/ correction
# ---------------------------

#'!!! Generative AI; level = medium --> debugging + dplyr syntax
#'function applies Pool Adjacent Violators Algorithm (PAVA) on intervals from each horizon
#' and ensures isotonicity over all horizons
#'
#'@note requires library "isotone" and "dplyr"
#'
#'@param df prediction dataframe. Must contain columns country, target, tau, 
#'target_year, target_quarter, horizon, lower_bound, upper_bound
pava_correct_df <- function(df){
  
  df %>% 
    #interval lengths and midpoints
    dplyr::mutate(
      W = upper_bound - lower_bound,
      M = (upper_bound + lower_bound) / 2
    ) %>%
      dplyr::group_by(country, target, tau, target_year, target_quarter) %>%
    dplyr::arrange(suppressWarnings(as.numeric(horizon)), .by_group = TRUE) %>%
    dplyr::group_modify(~{
      
      valid <- !is.na(.x$W) & !is.na(.x$M)
      
      W_isotonic <- rep(NA_real_, nrow(.x))
      
      #pava correction of intervals
      W_isotonic[valid] <- as.numeric(isotone::gpava(z = seq_len(sum(valid)), y = .x$W[valid])$x)
      
      #recalculate bounds
      .x$lower_bound <- .x$M - W_isotonic / 2
      .x$upper_bound <- .x$M + W_isotonic / 2
      
      dplyr::select(.x,-M,-W)
    }) %>%
    dplyr::ungroup()
    
}

#'function calculates value of check loss function from residual u and quantile level tau
#'
#'@param u residual (x - x_hat)
#'@param tau quantile level 
check_loss <- function(u, tau){
  ifelse(u >= 0, tau * u, (1 - tau) * (-u))
}


#'!!! Generative AI, level = medium --> debugging and dplyr syntax 
#'function aggregates quarterly input dataframe to annual values
#'
#'@description
#'7  quarters with weights 0.25, 0.5, 0.75, 1.0, 0.75, 0.5, 0.25 (sum = 4) 
#'are summed to annual growth rate. The quarterly values must be quarter on quarter growth
#'rates (in percent). The output is an annual growth rate (in percent)
#'@note Dataframe must be produced by function fit_arima in file ts_annual_data.R
#'to match horizons: for horizon 0.0 only one quarter ahead prediction from 3rd quarter
#'for horizon 0.5 three quarter ahead predictions from 1st quarter
#'for horizon 1.0 five quarter ahead predictions from 3rd quarter 
#'for horizon 1.5 seven quarter ahead predictions from 1st quarter
#'
#'@param df input dataframe (from quarterly TS predictions). Must contain columns target_year, target_quarter
#'forecast_year, forecast_quarter, country, horizon, pred_cpi, pred_gdp, tv_cpi, tv_gdp 
aggregate_to_annual_input <- function(df){
  
  # triangular weights for 7-quarter aggregation
  weights <- as.numeric((1 - abs(4 - 1:7) / 4))
  
  out <- df %>%
    dplyr::mutate(
      tq_index = 4 * target_year + target_quarter
    ) %>%
    dplyr::group_by(country) %>%
    dplyr::group_modify(~{
      
      forecast_years <- sort(unique(.x$forecast_year))
      horizons <- sort(as.numeric(unique(.x$horizon)))
      out_list <- list()
      idx <- 1
      
      for(f in forecast_years){
        
        for(h in horizons){
          #target year based on horizon
          t <- ifelse(h == 0.0 | h == 0.5, f, f + 1)
          
          #required indexes for truth value
          required_index <- (4 * (t - 1) + 2):(4 * t + 4)
          
          # required window 
          truth_df <- .x %>%
            filter(tq_index %in% required_index) %>%
            distinct(tq_index, .keep_all = TRUE) %>%
            arrange(tq_index, forecast_year)
          
          if (nrow(truth_df) != 7){
            message("not exactly 7 truth values")
            next
          } 
          
          tv_cpi_vec <- truth_df$tv_cpi
          tv_gdp_vec <- truth_df$tv_gdp
          
          pred_df <- .x %>%
            filter(
              forecast_year == f,
              horizon == h
            ) %>%
            arrange(tq_index)
          
          # number of predictions needed depending on horizon
          # for h = 0.0 --> 1, 0.5 --> 3 ... 
          n_pred <- as.integer(h / 0.25 + 1)
          
          if (nrow(pred_df) != n_pred){
            message("not the correct number of predictions. Needed: ", 
                    n_pred, ", Got: ", nrow(pred_df))
            next
          } 
          
          #needed truth values
          is_truth <- seq_len(7) <= 7 - n_pred
          
          #combine truth values and prediction values to vector
          cpi_source <- c(tv_cpi_vec[is_truth], pred_df$pred_cpi)
          gdp_source <- c(tv_gdp_vec[is_truth], pred_df$pred_gdp)
          
          
          #aggregation of truth values
          log_tv_cpi <- log1p(tv_cpi_vec / 100)
          log_tv_gdp <- log1p(tv_gdp_vec / 100)
          
          tv_cpi_annual <- 100 * sum(weights * log_tv_cpi)
          tv_gdp_annual <- 100 * sum(weights * log_tv_gdp)
          
          
          #aggregation of combined truth/pred sources
          log_pred_cpi <- log1p(cpi_source / 100)
          log_pred_gdp <- log1p(gdp_source / 100)
          
          pred_cpi_annual <- 100 * sum(weights * log_pred_cpi)
          pred_gdp_annual <- 100 * sum(weights * log_pred_gdp)
          
          # store aggregated row
          out_list[[idx]] <- tibble(
            forecast_year = f,
            forecast_quarter = tail(pred_df$forecast_quarter, n = 1),
            target_year = t,
            horizon = h,
            pred_cpi = pred_cpi_annual,
            pred_gdp = pred_gdp_annual,
            tv_cpi = tv_cpi_annual,
            tv_gdp = tv_gdp_annual
          )
          
          idx <- idx + 1
        }
      }
      
      dplyr::bind_rows(out_list)
    }) %>%
    dplyr::ungroup()
  
  out
}
#'!!!

