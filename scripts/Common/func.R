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
            timeStamp   = as.POSIXct(   dframe[[unix_ts_col_name]], origin = "1970-01-01 12:00:00"),
            date        = as.Date(      format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%Y-%m-%d"), format = "%Y-%m-%d"),
            year        = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%Y")),
            month       = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%m")),
            day         = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%d")),
            hour        = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%H")),
            minute      = as.integer(   format(as.POSIXct(unix_ts, origin = "1970-01-01 12:00:00"), format = "%M"))
        ) %>%
        select(timeStamp, date, year, month, day, hour, minute, everything()) %>%
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

# Create a function that filters a DataFrame based on year, month and range of day numbers
filter_date_range <- function(pDF, pYear, pMonth, pDay1, pDay2) {
    if(all(c("year", "month", "day") %in% colnames(pDF), TRUE)) {
        pFilteredDF <- pDF %>% filter(year == pYear, month == pMonth, day > pDay1, day <= pDay2)
    } else {
        stop("The DataFrame provided in parameter is missing one or more of these columns: 'year', 'month' and 'day'!")
    }
    return(pFilteredDF)
}

# Create function that calculates an instant by day
create_instant_time <- function(pDF) {
    # Check if necessary columns exist
    if(all(c("hour", "date") %in% colnames(pDF), TRUE)) {
        # Create DataFrame with 'instant' column
        InstantDF <- pDF %>%
            mutate(
                time = format(strptime(hour, format="%H"), format = "%H:00"),
                date_time = paste0(date, " ", time),
                unix_ts = as.numeric(as.POSIXct(date_time)),
                instant = (unix_ts - min(unix_ts)) / (24 * 3600)
            ) %>%
            select(-c(time, date_time, unix_ts))
    } else {
        stop("The DataFrame provided in parameter is missing one or more of these columns: 'hour', 'date' and 'unix_ts'!")
    }
    return(InstantDF)
}

# Define a function tha generates a weekly line plot based on a DataFrame
create_base_weekly_line_plot <- function(pDF, pXaxis, pYaxis, plabelProperties, pAvgYaxis, pYlimMin, pYlimMax, pHasXaxis=FALSE) {
    # Parameters to string
    pXaxisStr <- deparse(substitute(pXaxis))
    pYaxisStr <- deparse(substitute(pYaxis))
    # Check if necessary columns exist
    if(all(c(pXaxisStr, pYaxisStr) %in% colnames(pDF), TRUE)) {
        # Plot definition
        base_plt <- ggplot(pDF, aes(x = {{ pXaxis }}, y = {{ pYaxis }})) +
            geom_line() +
            scale_x_continuous(breaks = 0:7, labels = 0:7) + 
            geom_label(data = pDF %>% filter({{ pYaxis }} < quantile({{ pYaxis }}, 0.05)), aes(label = {{ pYaxis }}), color = plabelProperties$pct05_color, fill = plabelProperties$pct05_fill) +
            geom_label(data = pDF %>% filter({{ pYaxis }} > quantile({{ pYaxis }}, 0.95)), aes(label = {{ pYaxis }}), color = plabelProperties$pct95_color, fill = plabelProperties$pct95_fill) +
            theme_light() +
            ylim(pYlimMin, pYlimMax) + 
            geom_hline(yintercept = pAvgYaxis, color = "blue", linetype = "dotted", linewidth = 1) + 
            geom_label(label = "Average", x = 7, y = pAvgYaxis, color = "blue")

        if(pHasXaxis){
            plt <- base_plt + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
        } else {
            plt <- base_plt + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
        }

    } else {
        stop("The DataFrame provided in parameter is missing one or more of the axis defined in parameters!")
    }
    return(plt)
}

#=================================================
# CORRELATION FUNCTIONS
#=================================================

weather_consumption_correlation <- function(pDF, pTimeFrame) {
    colsList <- c("avg_temp", "avg_dewpt_temp", "avg_rel_hum_pct", "avg_wind_dir", "avg_wind_spd", "avg_visib", "avg_stn_press", "avg_hmdx", "avg_wind_chill")
    if(all(colsList %in% colnames(pDF), TRUE)) {

        avg_temp_cor          <- round(cor(pDF["avg_temp"],          pDF["consumption"]), 3)
        avg_dewpt_temp_cor    <- round(cor(pDF["avg_dewpt_temp"],    pDF["consumption"]), 3)
        avg_rel_hum_pct_cor   <- round(cor(pDF["avg_rel_hum_pct"],   pDF["consumption"]), 3)
        avg_wind_dir_cor      <- round(cor(pDF["avg_wind_dir"],      pDF["consumption"]), 3)
        avg_wind_spd_cor      <- round(cor(pDF["avg_wind_spd"],      pDF["consumption"]), 3)
        avg_visib_cor         <- round(cor(pDF["avg_visib"],         pDF["consumption"]), 3)
        avg_stn_press_cor     <- round(cor(pDF["avg_stn_press"],     pDF["consumption"]), 3)
        avg_hmdx_cor          <- round(cor(pDF["avg_hmdx"],          pDF["consumption"]), 3)
        avg_wind_chill_cor    <- round(cor(pDF["avg_wind_chill"],    pDF["consumption"]), 3)

        pTimeFrame <- str_to_title(toString(pTimeFrame))

        variable_one        <- rep("Consumption", 9)
        variable_two        <- c("Average Temperature", "Average Dew Point", "Average Relative Humidity (%)", "Average Wind Direction", "Average Wind Speed", "Average Visibility", "Average Station Pressure", "Average Humidex", "Average Wind Chill")
        time_frame          <- rep(pTimeFrame, 9)
        correlation         <- c(avg_temp_cor, avg_dewpt_temp_cor, avg_rel_hum_pct_cor, avg_wind_dir_cor, avg_wind_spd_cor, avg_visib_cor, avg_stn_press_cor, avg_hmdx_cor, avg_wind_chill_cor)

        CorrDF <- data.frame(variable_one, variable_two, time_frame, correlation)

    } else {
        stop("Some columns are missing in the Data Frame provided in parameters.")
    }

    return(CorrDF)
}