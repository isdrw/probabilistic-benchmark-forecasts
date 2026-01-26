# data_transformation_functions.R
# Modular utility functions

# ===========================
## preparation of "WEOforecasts_prefilter.parquet" data
# # path "data/raw/IMF WEO/WEOforecasts_prefilter.parquet"
# ===========================


#'load data from "WEOforecasts_prefilter.parquet"
#'
#'@note arrow package must be installed and loaded
load_WEO_data <- function(){
  #path
  file_path <- r"(data/raw/IMF WEO\WEOforecasts_prefilter.parquet)"
  
  #read data file
  df <- read_parquet(file_path)
  
  return(df)
}

#'function removes unnecessary columns from data "WEOforecasts_prefilter.parquet"
#'
#'@param df dataframe of "WEOforecasts_prefilter.parquet"
remove_cols_WEO <- function(df){
  df %>% 
    dplyr::select(-WEO_Country_Code, -ISOAlpha_3Code, 
                -type, -cgroup, -economy, -euro, -g20, 
                -geoEmer, -geolInc, -oil, -tv_2, 
                -tv_1.5, -tv_0.5)
}


#'function transforms "WEOforecasts_prefilter.parquet" into wide format by target
#'
#'@note function remove_cols() must already be applied to df
#'
#'@param df dataframe of "WEOforecasts_prefilter.parquet"
to_wide_format_WEO <- function(df){
  df %>%
    pivot_wider(
      names_from = target,
      values_from = c(prediction,tv_1)
    )
}

#'function transforms forecast_season column into forecast_quarter column S --> 2 F --> 4
#'
#'@note function to_wide_format() must already be appplied to df
#'
#'@param df dataframe of "WEOforecasts_prefilter.parquet"
season_to_quarter_WEO <- function(df){
  df %>%
    mutate(
      forecast_quarter = ifelse(
        forecast_season == "S",
        2,
        4
      )
    ) %>%
    dplyr::select(-forecast_season) %>%
    relocate(forecast_quarter, .after = forecast_year)
}

#'function renames columns 
#'
#'@note function season_to_quarter() must already be applied to df
#'
#'@param df dataframe of "WEOforecasts_prefilter.parquet"
rename_cols_WEO <- function(df){
  df %>%
    rename(
      tv_gdp = tv_1_ngdp_rpch,
      tv_cpi = tv_1_pcpi_pch,
      pred_gdp = prediction_ngdp_rpch,
      pred_cpi = prediction_pcpi_pch
    )
} 


#'function prepares dataframe WEOforecasts_prefilter.parquet"
#'
#'@param df dataframe of "WEOforecasts_prefilter.parquet"
prepare_WEO_df <- function(df){
  df %>%
    remove_cols_WEO() %>%
    to_wide_format_WEO() %>%
    season_to_quarter_WEO() %>%
    rename_cols_WEO()
}


#'load and prepare WEOforecasts_prefilter.parquet" data
#'
#'@note dplyr and arrow must be installed and loaded
load_and_prepare_WEO_data <- function(){
  df <- load_WEO_data() %>%
    prepare_WEO_df()
  
  return(df)
}


# ===========================
## preparation of "oecd_quarterly_data.csv" data
# path "data/raw/IMF WEO/oecd_quarterly_data.csv"
# ===========================

#'load data from "oecd_quarterly_data.csv"
load_oecd_data <- function(){
  #path
  file_path <- r"(data/raw/IMF WEO\oecd_quarterly_data.csv)"
  
  #read data file
  df <- read.csv(file_path)
  
  return(df)
}

#'removes unnecessary columns from "oecd_quarterly_data.csv" dataframe
#'
#'@param df dataframe of "oecd_quarterly_data.csv"
remove_cols_oecd <- function(df){
  df %>%
    dplyr::select(-ciss)
}


#'splits date column from form "yyyy Q{1,2,3,4}" into seperate columns
#'forecast_year and forecast_quarter in form "yyyy" and "{1,2,3,4}" respectively
#'
#'@note function remove_cols_oecd must already be applied to df;
#'year and quarter columns are renamed to forecast_year and forecast_quarter
#'in order to be consistent with WEO dataframe even though oecd data doesn't
#'contain predictions
#'
#'@param df dataframe of "oecd_quarterly_data.csv"
split_date_oecd <- function(df){
  df %>% 
    separate(dt, into = c("forecast_year", "forecast_quarter"), sep = " ") %>%
    mutate(forecast_year = as.numeric(forecast_year),
           forecast_quarter = as.numeric(gsub("Q", "", forecast_quarter)))
}

#'renames columns 
#'
#'@note function split_date_oecd() must already be applied to df;
#'gdp and cpi are renamed to tv_gdp and tv_cpi respectively to be consistent
#'with WEO dataframe even though OECD data doesnt contain predictions
#'
#'@param df dataframe of "oecd_quarterly_data.csv"
rename_cols_oecd <- function(df){
  df %>%
    rename(country = ccode,
           tv_gdp = gdp,
           tv_cpi = cpi)
}

#'prepare oecd dataframe
#'
#'@param df dataframe of "oecd_quarterly_data.csv"
prepare_oecd_df <- function(df){
  df %>%
    remove_cols_oecd() %>%
    split_date_oecd() %>%
    rename_cols_oecd()
}

#'load and prepare oecd data
#'
load_and_prepare_oecd_data <- function(){
  df <- load_oecd_data() %>%
    prepare_oecd_df()
  
  return(df)
}

# ===========================
## preparation of random walk data based on quarterly data 
## of oecd_quarterly_data.csv
## path "data/raw/IMF WEO/oecd_quarterly_data.csv"
# ===========================

#'load data from "point_prediction_rw.csv"
#'in folder data/processed
#'
load_RW_data <- function(){
  #path
  file_path <- r"(data/processed/point predictions/point_predictions_rw.csv)"
  
  #read data file
  df <- read.csv(file_path)
  
  return(df)
}


#'function transforms "point_predictions_rw.csv" into wide format by target
#'
#'@param df dataframe of "point_predictions_rw.csv"
to_wide_format_RW <- function(df){
  df %>%
    pivot_wider(
      names_from = target,
      values_from = c(prediction,truth_value)
    )
}


#'renames columns of ""point_predictions_rw.csv"
#'
#'@note function to_wide_format_RW() must already be applied to df
#'
#'@param df dataframe of "point_predictions_rw.csv"
rename_cols_RW <- function(df){
  df %>%
    rename(pred_gdp = prediction_tv_gdp,
           pred_cpi = prediction_tv_cpi,
           tv_gdp = truth_value_tv_gdp,
           tv_cpi = truth_value_tv_cpi)
}

#'function prepares dataframe "point_predictions_rw.csv"
#'
#'@param df dataframe of "point_predictions_rw.csv"
prepare_RW_df <- function(df){
  df %>%
    to_wide_format_RW() %>%
    rename_cols_RW()
}


#'load and prepare "point_predictions_rw.csv" data
#'
#'@note dplyr and arrow must be installed and loaded
load_and_prepare_RW_data <- function(){
  df <- load_RW_data() %>%
    prepare_RW_df()
  
  return(df)
}

# ===========================
## preparation of ARIMA(1,0,0) data based on quarterly data 
## of oecd_quarterly_data.csv
## path "data/raw/IMF WEO/oecd_quarterly_data.csv"
# ===========================

#'load data from "point_predictions_arima1_0_0.csv"
#'in folder data/processed
#'
load_ARIMA1_0_0_data <- function(){
  #path
  file_path <- r"(data/processed/point predictions/point_predictions_arima1_0_0.csv)"
  
  #read data file
  df <- read.csv(file_path)
  
  return(df)
}


#'function transforms "point_predictions_arima1_0_0.csv" into wide format by target
#'
#'@param df dataframe of "point_predictions_arima1_0_0.csv"
to_wide_format_ARIMA1_0_0 <- function(df){
  df %>%
    pivot_wider(
      names_from = target,
      values_from = c(prediction,truth_value)
    )
}


#'renames columns of "point_predictions_arima1_0_0.csv"
#'
#'@note function to_wide_format_ARIMA1_0_0() must already be applied to df
#'
#'@param df dataframe of "point_predictions_arima1_0_0.csv"
rename_cols_ARIMA1_0_0 <- function(df){
  df %>%
    rename(pred_gdp = prediction_tv_gdp,
           pred_cpi = prediction_tv_cpi,
           tv_gdp = truth_value_tv_gdp,
           tv_cpi = truth_value_tv_cpi)
}

#'function prepares dataframe "point_predictions_arima1_0_0.csv"
#'
#'@param df dataframe of "point_predictions_arima1_0_0.csv"
prepare_ARIMA1_0_0_df <- function(df){
  df %>%
    to_wide_format_ARIMA1_0_0() %>%
    rename_cols_ARIMA1_0_0()
}


#'load and prepare "point_predictions_arima1_0_0.csv" data
#'
#'@note dplyr and arrow must be installed and loaded
load_and_prepare_ARIMA1_0_0_data <- function(){
  df <- load_ARIMA1_0_0_data() %>%
    prepare_ARIMA1_0_0_df()
  
  return(df)
}


# ===========================
## preparation of ARIMA(1,1,0) data based on quarterly data 
## of oecd_quarterly_data.csv
## path "data/raw/IMF WEO/oecd_quarterly_data.csv"
# ===========================

#'load data from "point_predictions_arima1_1_0.csv"
#'in folder data/processed
#'
load_ARIMA1_1_0_data <- function(){
  #path
  file_path <- r"(data/processed/point predictions/point_predictions_arima1_1_0.csv)"
  
  #read data file
  df <- read.csv(file_path)
  
  return(df)
}


#'function transforms "point_predictions_arima1_1_0.csv" into wide format by target
#'
#'@param df dataframe of "point_predictions_arima1_1_0.csv"
to_wide_format_ARIMA1_1_0 <- function(df){
  df %>%
    pivot_wider(
      names_from = target,
      values_from = c(prediction,truth_value)
    )
}


#'renames columns of "point_predictions_arima1_1_0.csv"
#'
#'@note function to_wide_format_ARIMA1_1_0() must already be applied to df
#'
#'@param df dataframe of "point_predictions_arima1_1_0.csv"
rename_cols_ARIMA1_1_0 <- function(df){
  df %>%
    rename(pred_gdp = prediction_tv_gdp,
           pred_cpi = prediction_tv_cpi,
           tv_gdp = truth_value_tv_gdp,
           tv_cpi = truth_value_tv_cpi)
}

#'function prepares dataframe "point_predictions_arima1_0_0.csv"
#'
#'@param df dataframe of "point_predictions_arima1_0_0.csv"
prepare_ARIMA1_1_0_df <- function(df){
  df %>%
    to_wide_format_ARIMA1_1_0() %>%
    rename_cols_ARIMA1_1_0()
}


#'load and prepare "point_predictions_arima1_0_0.csv" data
#'
#'@note dplyr and arrow must be installed and loaded
load_and_prepare_ARIMA1_1_0_data <- function(){
  df <- load_ARIMA1_1_0_data() %>%
    prepare_ARIMA1_1_0_df()
  
  return(df)
}


# ===========================
## preparation of ARIMA auto data based on quarterly data 
## of oecd_quarterly_data.csv
## path "data/raw/IMF WEO/oecd_quarterly_data.csv"
# ===========================

#'load data from "point_predictions_arima_auto.csv"
#'in folder data/processed
#'
load_ARIMA_auto_data <- function(){
  #path
  file_path <- r"(data/processed/point predictions/point_predictions_arima_auto.csv)"
  
  #read data file
  df <- read.csv(file_path)
  
  return(df)
}


#'function transforms "point_predictions_arima_auto.csv" into wide format by target
#'
#'@param df dataframe of "point_predictions_arima_auto.csv"
to_wide_format_ARIMA_auto <- function(df){
  df %>%
    pivot_wider(
      names_from = target,
      values_from = c(prediction,truth_value)
    )
}


#'renames columns of "point_predictions_arima_auto.csv"
#'
#'@note function to_wide_format_ARIMA_auto() must already be applied to df
#'
#'@param df dataframe of "point_predictions_arima_auto.csv"
rename_cols_ARIMA_auto <- function(df){
  df %>%
    rename(pred_gdp = prediction_tv_gdp,
           pred_cpi = prediction_tv_cpi,
           tv_gdp = truth_value_tv_gdp,
           tv_cpi = truth_value_tv_cpi)
}

#'function prepares dataframe "point_predictions_arima_auto.csv"
#'
#'@param df dataframe of "point_predictions_arima_auto.csv"
prepare_ARIMA_auto_df <- function(df){
  df %>%
    to_wide_format_ARIMA_auto() %>%
    rename_cols_ARIMA_auto()
}


#'load and prepare "point_predictions_arima_auto.csv" data
#'
#'@note dplyr and arrow must be installed and loaded
load_and_prepare_ARIMA_auto_data <- function(){
  df <- load_ARIMA_auto_data() %>%
    prepare_ARIMA_auto_df()
  
  return(df)
}

