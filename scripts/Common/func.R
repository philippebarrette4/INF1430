# Import packages
library(dplyr)
library(readr)
library(lubridate)

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
            timeStamp   = as.POSIXct(   dframe[[unix_ts_col_name]], origin = "1970-01-01 12:00:00"),
            date        = as.Date(      format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%Y-%m-%d"), format = "%Y-%m-%d"),
            year        = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%Y")),
            month       = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%m")),
            week        = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%W")),
            day         = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%d")),
            hour        = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%H")),
            minute      = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%M"))
        ) %>%
        select(timeStamp, date, year, month, week, day, hour, minute, everything()) %>%
        select(-all_of(unix_ts_col_name))

    return(dframe2)
}

weather_summarise <- function(dframe) {
    #' Round and calculate median and mean values for different weather metrics.
    #' 
    #' @description Rounds at 2 digits and calculate means and median values of different weather related columns.
    #' @param dframe data.frame. Input DataDrame containing all weather data.
    #' @usage weather_summarise(weatherDF)
    #' @return The input DataFrame summarized with the mean and median values.
    df <- dframe %>%
        summarise(
            avg_temp        = round(mean(`Temp (C)`), 2),
            med_temp        = round(median(`Temp (C)`), 2),
            avg_dewpt_temp  = round(mean(`Dew Point Temp (C)`), 2),
            med_dewpt_temp  = round(median(`Dew Point Temp (C)`), 2),
            avg_rel_hum_pct = round(mean(`Rel Hum (%)`), 2),
            med_rel_hum_pct = round(median(`Rel Hum (%)`), 2),
            avg_wind_dir    = round(mean(`Wind Dir (10s deg)`), 2),
            med_wind_dir    = round(median(`Wind Dir (10s deg)`), 2),
            avg_wind_spd    = round(mean(`Wind Spd (km/h)`), 2),
            med_wind_spd    = round(median(`Wind Spd (km/h)`), 2),
            avg_visib       = round(mean(`Visibility (km)`), 2),
            med_visib       = round(median(`Visibility (km)`), 2),
            avg_stn_press   = round(mean(`Stn Press (kPa)`), 2),
            med_stn_press   = round(median(`Stn Press (kPa)`), 2),
            avg_hmdx        = round(mean(Hmdx), 2),
            med_hmdx        = round(median(Hmdx), 2),
            avg_wind_chill  = round(mean(`Wind Chill`), 2),
            med_wind_chill  = round(median(`Wind Chill`), 2)
        )
    return(df)
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