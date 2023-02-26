#=====================================================
# IMPORT LIBRARIES & CONFIG
#=====================================================

# Data transformation
library(dplyr)
library(readr)

#=====================================================
# RAW DATAFRAME: READ CSV FILE
#=====================================================

# Read 'Climate_HistoricalNormals.csv' file
# Specify column types
rawDF <- read_csv(
    "data/Climate_HourlyWeather.csv",
    col_types = cols(
        `Date/Time` = col_datetime(),
        Year = col_integer(),
        Month = col_integer(),
        Day = col_integer(),
        Time = col_time(),
        `Data Quality` = col_character(),
        `Temp (C)` = col_double(),
        `Temp Flag` = col_character(),
        `Dew Point Temp (C)` = col_double(),
        `Dew Point Temp Flag` = col_character(),
        `Rel Hum (%)` = col_integer(),
        `Rel Hum Flag` = col_character(),
        `Wind Dir (10s deg)` = col_integer(),
        `Wind Dir Flag` = col_character(),
        `Wind Spd (km/h)` = col_integer(),
        `Wind Spd Flag` = col_character(),
        `Visibility (km)` = col_double(),
        `Visibility Flag` = col_character(),
        `Stn Press (kPa)` = col_double(),
        `Stn Press Flag` = col_character(),
        Hmdx = col_double(), # TBD
        `Hmdx Flag` = col_character(),
        `Wind Chill` = col_integer(), # TBD
        `Wind Chill Flag` = col_character(),
        Weather = col_character()
    )
)

#=====================================================
# SILVER DATAFRAME: CLEAN UP COLUMNS
#=====================================================

# Replace NA values
# All NAs are replaced with the median value of their respective column
silverDF <- rawDF %>%
mutate(
    hour                    =  as.integer(Time) / 3600,
    `Temp (C)`              =  coalesce(`Temp (C)`,             median(`Temp (C)`,              na.rm = TRUE)),
    `Dew Point Temp (C)`    =  coalesce(`Dew Point Temp (C)`,   median(`Dew Point Temp (C)`,    na.rm = TRUE)),
    `Rel Hum (%)`           =  coalesce(`Rel Hum (%)`,          median(`Rel Hum (%)`,           na.rm = TRUE)),
    `Wind Dir (10s deg)`    =  coalesce(`Wind Dir (10s deg)`,   median(`Wind Dir (10s deg)`,    na.rm = TRUE)),
    `Wind Spd (km/h)`       =  coalesce(`Wind Spd (km/h)`,      median(`Wind Spd (km/h)`,       na.rm = TRUE)),
    `Visibility (km)`       =  coalesce(`Visibility (km)`,      median(`Visibility (km)`,       na.rm = TRUE)),
    `Stn Press (kPa)`       =  coalesce(`Stn Press (kPa)`,      median(`Stn Press (kPa)`,       na.rm = TRUE)),
    Hmdx                    =  coalesce(Hmdx,                   median(Hmdx,                    na.rm = TRUE)),
    `Wind Chill`            =  coalesce(`Wind Chill`,           median(`Wind Chill`,            na.rm = TRUE))
)


#=====================================================
# GOLD DATAFRAMES: AGGREGATE DATA
#=====================================================

# Aggregation of values by hour
goldHourlyDF <- silverDF %>%
    mutate(date = as.Date(`Date/Time`)) %>%
    rename(year = Year, month = Month, day = Day) %>%
    group_by(year, month, day, date, hour) %>%
    summarise(
        avg_temp        = mean(`Temp (C)`),
        med_temp        = median(`Temp (C)`),
        avg_dewpt_temp  = mean(`Dew Point Temp (C)`),
        med_dewpt_temp  = median(`Dew Point Temp (C)`),
        avg_rel_hum_pct = mean(`Rel Hum (%)`),
        med_rel_hum_pct = median(`Rel Hum (%)`),
        avg_wind_dir    = mean(`Wind Dir (10s deg)`),
        med_wind_dir    = median(`Wind Dir (10s deg)`),
        avg_wind_spd    = mean(`Wind Spd (km/h)`),
        med_wind_spd    = median(`Wind Spd (km/h)`),
        avg_visib       = mean(`Visibility (km)`),
        med_visib       = median(`Visibility (km)`),
        avg_stn_press   = mean(`Stn Press (kPa)`),
        med_stn_press   = median(`Stn Press (kPa)`),
        avg_hmdx        = mean(Hmdx),
        med_hmdx        = median(Hmdx),
        avg_wind_chill  = mean(`Wind Chill`),
        med_wind_chill  = median(`Wind Chill`)
    )

# Aggregation of values by day
goldDailyDF <- silverDF %>%
    mutate(date = as.Date(`Date/Time`)) %>%
    rename(year = Year, month = Month, day = Day) %>%
    group_by(year, month, day, date) %>%
    summarise(
        avg_temp        = mean(`Temp (C)`),
        med_temp        = median(`Temp (C)`),
        avg_dewpt_temp  = mean(`Dew Point Temp (C)`),
        med_dewpt_temp  = median(`Dew Point Temp (C)`),
        avg_rel_hum_pct = mean(`Rel Hum (%)`),
        med_rel_hum_pct = median(`Rel Hum (%)`),
        avg_wind_dir    = mean(`Wind Dir (10s deg)`),
        med_wind_dir    = median(`Wind Dir (10s deg)`),
        avg_wind_spd    = mean(`Wind Spd (km/h)`),
        med_wind_spd    = median(`Wind Spd (km/h)`),
        avg_visib       = mean(`Visibility (km)`),
        med_visib       = median(`Visibility (km)`),
        avg_stn_press   = mean(`Stn Press (kPa)`),
        med_stn_press   = median(`Stn Press (kPa)`),
        avg_hmdx        = mean(Hmdx),
        med_hmdx        = median(Hmdx),
        avg_wind_chill  = mean(`Wind Chill`),
        med_wind_chill  = median(`Wind Chill`)
    )

# Aggregation by month
goldMonthlyDF <- silverDF %>%
    rename(year = Year, month = Month) %>%
    group_by(year, month) %>%
    summarise(
        avg_temp        = mean(`Temp (C)`),
        med_temp        = median(`Temp (C)`),
        avg_dewpt_temp  = mean(`Dew Point Temp (C)`),
        med_dewpt_temp  = median(`Dew Point Temp (C)`),
        avg_rel_hum_pct = mean(`Rel Hum (%)`),
        med_rel_hum_pct = median(`Rel Hum (%)`),
        avg_wind_dir    = mean(`Wind Dir (10s deg)`),
        med_wind_dir    = median(`Wind Dir (10s deg)`),
        avg_wind_spd    = mean(`Wind Spd (km/h)`),
        med_wind_spd    = median(`Wind Spd (km/h)`),
        avg_visib       = mean(`Visibility (km)`),
        med_visib       = median(`Visibility (km)`),
        avg_stn_press   = mean(`Stn Press (kPa)`),
        med_stn_press   = median(`Stn Press (kPa)`),
        avg_hmdx        = mean(Hmdx),
        med_hmdx        = median(Hmdx),
        avg_wind_chill  = mean(`Wind Chill`),
        med_wind_chill  = median(`Wind Chill`)
    )

#=====================================================
# WRITE GOLD DATAFRAMES TO CSV FILES
#=====================================================

# Hourly weather Data Frame
write_csv(goldHourlyDF,     append = FALSE, file = "curated/weather/gold_hourly_weather.csv",   col_names = TRUE)
# Daily weather Data Frame
write_csv(goldDailyDF,      append = FALSE, file = "curated/weather/gold_daily_weather.csv",    col_names = TRUE)
# Monthly weather Data Frame
write_csv(goldMonthlyDF,    append = FALSE, file = "curated/weather/gold_monthly_weather.csv",  col_names = TRUE)