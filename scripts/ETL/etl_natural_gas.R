#=====================================================
# IMPORT LIBRARIES & CONFIG
#=====================================================

# Data transformation
library(dplyr)
library(readr)
library(here)
# Import common func.R file
source(here::here("Scripts/Common", "func.R"))

#=====================================================
# RAW DATAFRAME: READ CSV FILE
#=====================================================
# Read CSV file: Whole House Electricity
rawDF <- read_csv(
    here::here("Data/Bronze", "NaturalGas_WHG.csv"),
    show_col_types = FALSE,
    col_types = cols(
        unix_ts     = col_double(),
        counter     = col_double(),
        avg_rate    = col_double(),
        inst_rate   = col_double()
    )
)

#=====================================================
# SILVER DATAFRAME: CLEAN UP COLUMNS
#=====================================================

# Derive unix timestamp column to date related columns: year, month, day
# Compute the real Watts consumption
silverDF <- rawDF %>%
            convert_timeStamp("unix_ts") %>%
            mutate(avg_rate_modif = avg_rate / 60)

#=====================================================
# GOLD DATAFRAMES: AGGREGATE DATA
#=====================================================

# Aggregation of consumption (m3) by hour
goldHourlyDF <- silverDF %>%
    group_by(year, month, day, date, hour) %>%
    summarise(
        consumption = sum(avg_rate_modif)
    )
    

# Aggregation of consumption (m3) by day
goldDailyDF <- silverDF %>%
    group_by(year, month, day, date) %>%
    summarise(
        consumption = sum(avg_rate_modif)
    )

# Aggregation of consumption (m3) by month
goldMonthlyDF <- silverDF %>%
    group_by(year, month) %>%
    summarise(
        consumption = sum(avg_rate_modif)
    )

#=====================================================
# WRITE GOLD DATAFRAMES TO CSV FILES
#=====================================================

# Hourly natural gas Data Frame
write_csv(goldHourlyDF,     append = FALSE, file = here::here("Data/Gold/NaturalGas", "gold_hourly_natural_gas.csv"),   col_names = TRUE)
# Daily natural gas Data Frame
write_csv(goldDailyDF,      append = FALSE, file = here::here("Data/Gold/NaturalGas", "gold_daily_natural_gas.csv"),    col_names = TRUE)
# Monthly natural gas Data Frame
write_csv(goldMonthlyDF,    append = FALSE, file = here::here("Data/Gold/NaturalGas", "gold_monthly_natural_gas.csv"),  col_names = TRUE)