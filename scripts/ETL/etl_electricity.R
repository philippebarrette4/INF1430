#=====================================================
# IMPORT LIBRARIES & CONFIG
#=====================================================

# Data transformation
library(dplyr)
library(readr)
# Import common func.R file
source("scripts/Common/func.R")

#=====================================================
# RAW DATAFRAME: READ CSV FILE
#=====================================================
# Read CSV file: Whole House Electricity
rawDF <- read_csv(
    "./data/Electricity_WHE.csv", show_col_types = FALSE,
    col_types = cols(
        unix_ts = col_double(),
        V       = col_double(),
        I       = col_double(),
        f       = col_double(),
        DPF     = col_double(),
        APF     = col_double(),
        P       = col_double(),
        Pt      = col_double(),
        Q       = col_double(),
        Qt      = col_double(),
        S       = col_double(),
        St      = col_double()
    )
)

#=====================================================
# SILVER DATAFRAME: CLEAN UP COLUMNS
#=====================================================

# Derive unix timestamp column to date related columns: year, month, day
# Compute the real Watts consumption
silverDF <- rawDF %>%
            convert_timeStamp("unix_ts") %>%
            mutate(avg_rate_modif = if_else(is.na(St - lag(St)), 0, St - lag(St)))

#=====================================================
# GOLD DATAFRAMES: AGGREGATE DATA
#=====================================================

# Aggregation of consumption (kWh) by hour
goldHourlyDF <- silverDF %>%
    group_by(year, month, day, date, hour) %>%
    summarise(
        consumption = sum(avg_rate_modif) / 1000
    )

# Aggregation of consumption (kWh) by day
goldDailyDF <- silverDF %>%
    group_by(year, month, day, date) %>%
    summarise(
        consumption = sum(avg_rate_modif) / 1000
    )

# Aggregation of consumption (kWh) by month
goldMonthlyDF <- silverDF %>%
    group_by(year, month) %>%
    summarise(
        consumption = sum(avg_rate_modif) / 1000
    )

#=====================================================
# WRITE GOLD DATAFRAMES TO CSV FILES
#=====================================================

# Hourly electricity Data Frame
write_csv(goldHourlyDF,     append = FALSE, file = "curated/electricity/gold_hourly_electricity.csv",   col_names = TRUE)
# Daily electricity Data Frame
write_csv(goldDailyDF,      append = FALSE, file = "curated/electricity/gold_daily_electricity.csv",    col_names = TRUE)
# Monthly electricity Data Frame
write_csv(goldMonthlyDF,    append = FALSE, file = "curated/electricity/gold_monthly_electricity.csv",  col_names = TRUE)