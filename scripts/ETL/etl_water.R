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
# Read CSV file: Whole House Water
rawDF <- read_csv(
    "data/Water_WHW.csv", show_col_types = FALSE,
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
            convert_timeStamp("unix_ts")

#=====================================================
# GOLD DATAFRAMES: AGGREGATE DATA
#=====================================================

# Aggregation of consumption (kL) by day
goldDailyDF <- silverDF %>%
    group_by(year, month, day, date) %>%
    summarise(
        consumtpion = sum(avg_rate)
    )

# Aggregation of consumption (kL) by month
goldMonthlyDF <- silverDF %>%
    group_by(year, month) %>%
    summarise(
        consumtpion = sum(avg_rate)
    )

#=====================================================
# WRITE GOLD DATAFRAMES TO CSV FILES
#=====================================================

# Daily water Data Frame
write_csv(goldDailyDF,      append = FALSE, file = "curated/water/gold_daily_water.csv",    col_names = TRUE)
# Monthly water Data Frame
write_csv(goldMonthlyDF,    append = FALSE, file = "curated/water/gold_monthly_water.csv",  col_names = TRUE)