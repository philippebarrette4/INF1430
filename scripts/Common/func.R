library(dplyr)
library(readr)

convert_timeStamp <- function(dframe, unix_ts_col_name){
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
    dframe2 <- dframe %>% 
        summarise(
            nDays = sum(n_distinct(date)),
            total_consumption = sum(dframe[[cons_col_name]], na.rm  = TRUE) / 1000,
            yearly_consumption_v1 = (total_consumption / nDays) * 365,
            yearly_consumption_v2 = total_consumption / 2
        )  
    return(dframe2)
}