# Import packages
library(dplyr)
library(readr)

convert_timeStamp <- function(dframe, unix_ts_col_name){
    #' Derived columns based on a unix timestamp one.
    #' 
    #' @description This function takes an integer column representing a unix timestamp value and derived multiple date related columns. 
    #' Ex: date (YYYY-mm-dd), year (YYYY), month (mm), etc.
    #' @param dframe data.frame. The DataFrame that contains the unix timestamp column
    #' @param unix_ts_col_name character. The name of the unix timestamp column
    #' @usage convert_timeStamp(myDF, "unix_ts")
    #' @return The input DataFrame with the new "human readableù/usable" date related columns.
    
    dframe2 <- dframe %>%
        mutate(
            timeStamp = as.POSIXct(dframe[[unix_ts_col_name]], origin = "1970-01-01 12:00:00"),
            date        = as.Date(format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%Y-%m-%d"), format = "%Y-%m-%d"),
            year        = format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%Y"),
            month       = format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%m"),
            day         = format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%d"),
            hour        = format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%H"),
            minute      = format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%M")
        ) %>%
        select(-all_of(unix_ts_col_name))

    return(dframe2)
}

yearly_consumption <- function(dframe, cons_col_name){
    #' Computes yearly consumption of either electricity (kWh), water (kL) or gaz (m^2)
    #' 
    #' @description Sums all the consumption and divide by the number of years/total days of data collection.
    #' @param dframe data.frame. Input DataDrame containing all consumption data.
    #' @param cons_col_name character. Name of the column that is related to the consumption data that needs to be summarized. 
    #' @usage yearly_consumption(myDF, "avg_rate")
    #' @return The input DataFrame summarized with the yearly consumption.

    dframe2 <- dframe %>% 
        summarise(
            nDays = sum(n_distinct(date)),
            total_consumption = sum(dframe[[cons_col_name]], na.rm  = TRUE) / 1000,
            yearly_consumption_v1 = (total_consumption / nDays) * 365,
            yearly_consumption_v2 = total_consumption / 2
        )  
    return(dframe2)
}