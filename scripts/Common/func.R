# Import packages
library(tidyr)

#=================================================
# CONVERSION FUNCTIONS
#=================================================

#' Derived columns based on a unix timestamp one.
#' 
#' @description This function takes an integer column representing a unix timestamp value and derived multiple date related columns. 
#'              Ex: date (YYYY-mm-dd), year (YYYY), month (mm), etc.
#' @param dframe data.frame. The DataFrame that contains the unix timestamp column
#' @param unix_ts_col_name character. The name of the unix timestamp column
#' @usage convert_timeStamp(myDF, "unix_ts")
#' @return The input DataFrame with the new "human readableù/usable" date related columns.
convert_timeStamp <- function(dframe, unix_ts_col_name){
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

#=================================================
# AGGREGATION FUNCTIONS
#=================================================

#' Computes yearly consumption of either electricity (kWh), water (kL) or gaz (m^2)
#' 
#' @description Sums all the consumption and divide by the number of years/total days of data collection.
#' @param dframe data.frame. Input DataDrame containing all consumption data.
#' @param cons_col_name character. Name of the column that is related to the consumption data that needs to be summarized. 
#' @usage yearly_consumption(myDF, "avg_rate")
#' @return The input DataFrame summarized with the yearly consumption.
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

#=================================================
# DATA WRANGLING FUNCTIONS
#=================================================

#' @description Filter a dataframe by a specific date range.
#' @param pDF: A dataframe to filter.
#' @param pYear: An integer representing the year to filter by.
#' @param pMonth: An integer representing the month to filter by.
#' @param pDay1: An integer representing the minimum day to filter by (inclusive).
#' @param pDay2: An integer representing the maximum day to filter by (exclusive).
#' @return A filtered dataframe containing only the rows that match the specified date range.
filter_date_range <- function(pDF, pYear, pMonth, pDay1, pDay2) {
    # Check if necessary columns exist
    if(all(c("year", "month", "day") %in% colnames(pDF), TRUE)) {
        # Filter dataframe based on year, month, day range
        pFilteredDF <- pDF %>% filter(year == pYear, month == pMonth, day > pDay1, day <= pDay2)
    } else {
        stop("The DataFrame provided in parameter is missing one or more of these columns: 'year', 'month' and 'day'!")
    }
    return(pFilteredDF)
}

#' @description Creates a new DataFrame with an 'instant' column that represents the time elapsed from the earliest timestamp in the input DataFrame.
#' @param pDF: A DataFrame containing at least the following columns: 'hour' (in format HH), 'date' (in format yyyy-mm-dd) and 'unix_ts' (in POSIXct format). 
#'             These columns will be used to calculate the 'instant' column.
#' @return A new DataFrame with the same columns as the input DataFrame, plus an additional column called 'instant', which represents the time elapsed from the earliest timestamp in the input DataFrame.
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

#=================================================
# PLOT FUNCTIONS
#=================================================

#' @description Function to create a base weekly line plot with customization options.
#' @param pDF: Data frame with data to be plotted.
#' @param pXaxis: Name of the column to be used as X-axis.
#' @param pYaxis: Name of the column to be used as Y-axis.
#' @param plabelProperties: List with label customization properties.
#' @param pAvgYaxis: Y value to be used as horizontal line representing the average.
#' @param pYlimMin: Minimum limit for the Y-axis.
#' @param pYlimMax: Maximum limit for the Y-axis.
#' @param pHasXaxis: Boolean indicating whether or not the X-axis should be included.
#' @usage create_base_weekly_line_plot(myDF, instant, consumption, label_properties_list, avg_hour_consumption, 0, 6, TRUE)
#' @return plt: The resulting ggplot2 plot.
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

#' @description Calculate the correlation between weather variables and energy consumption.
#' @param pDF: A data frame containing columns for the weather variables and energy consumption.
#' @param pTimeFrame: A character string representing the time frame for which the correlation is being calculated. (Example: "hourly" or "daily")
#' @usage weather_consumption_correlation(myDF, "hourly")
#' @return A data frame containing four columns: 
#'     1) "variable_one" - A character string "Consumption" repeated 9 times representing energy consumption.
#'     2) "variable_two" - A character vector containing the names of 9 weather variables for which correlation is calculated.
#'     3) "time_frame"   - A character string representing the time frame for which the correlation is being calculated.
#'     4) "correlation"  - A numeric vector containing the correlation coefficients between the energy consumption and weather variables.
weather_consumption_correlation <- function(pDF, pTimeFrame) {
    # Define a list of column names to be used for correlation calculation
    colsList <- c("avg_temp", "avg_dewpt_temp", "avg_rel_hum_pct", "avg_wind_dir", "avg_wind_spd", "avg_visib", "avg_stn_press", "avg_hmdx", "avg_wind_chill")
    # Check if all the columns in the colsList are present in the provided data frame
    if(all(colsList %in% colnames(pDF), TRUE)) {
        # Calculate the correlation between each column and the "consumption" column using round() function with 3 decimal places
        avg_temp_cor          <- round(cor(pDF["avg_temp"],          pDF["consumption"]), 3)
        avg_dewpt_temp_cor    <- round(cor(pDF["avg_dewpt_temp"],    pDF["consumption"]), 3)
        avg_rel_hum_pct_cor   <- round(cor(pDF["avg_rel_hum_pct"],   pDF["consumption"]), 3)
        avg_wind_dir_cor      <- round(cor(pDF["avg_wind_dir"],      pDF["consumption"]), 3)
        avg_wind_spd_cor      <- round(cor(pDF["avg_wind_spd"],      pDF["consumption"]), 3)
        avg_visib_cor         <- round(cor(pDF["avg_visib"],         pDF["consumption"]), 3)
        avg_stn_press_cor     <- round(cor(pDF["avg_stn_press"],     pDF["consumption"]), 3)
        avg_hmdx_cor          <- round(cor(pDF["avg_hmdx"],          pDF["consumption"]), 3)
        avg_wind_chill_cor    <- round(cor(pDF["avg_wind_chill"],    pDF["consumption"]), 3)
        # Convert the time frame parameter to lower case and repeat it for 9 times
        pTimeFrame <- tolower(toString(pTimeFrame))
        time_frame          <- rep(pTimeFrame, 9)
        # Create three vectors containing the variable_one, variable_two and correlation values respectively
        variable_one        <- rep("Consumption", 9)
        variable_two        <- c("Average Temperature", "Average Dew Point", "Average Relative Humidity (%)", "Average Wind Direction", "Average Wind Speed", "Average Visibility", "Average Station Pressure", "Average Humidex", "Average Wind Chill")
        correlation         <- c(avg_temp_cor, avg_dewpt_temp_cor, avg_rel_hum_pct_cor, avg_wind_dir_cor, avg_wind_spd_cor, avg_visib_cor, avg_stn_press_cor, avg_hmdx_cor, avg_wind_chill_cor)
        # Combine the above three vectors to create a data frame "CorrDF"
        CorrDF <- data.frame(variable_one, variable_two, time_frame, correlation)

    } else {
        # If some columns are missing in the data frame, stop the execution and print the error message
        stop("Some columns are missing in the Data Frame provided in parameters.")
    }
    # Return the CorrDF data frame
    return(CorrDF)
}

#' @description Computes the pairwise correlation between variables in a data frame for a given time frame.
#' @param pDF: A data frame containing the variables to be correlated.
#' @param pIndVarVec: A vector of strings containing the names of the variables to be correlated.
#' @param pTimeFrame: A string indicating the time frame of the data.
#' @return corrDF: A data frame containing the pairwise correlation between variables, as well as the time frame of the data.
pairwise_correlations_function <- function(pDF, pIndVarVec, pTimeFrame) {
    # Create an empty data frame with three columns to store the correlation results
    corrDF <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("independantVariableOne", "independantVariableTwo", "correlation"))
    # Loop over all possible pairs of variables and compute the correlation
    for (i in 1:length(pIndVarVec)) {
        for (j in (i+1):length(pIndVarVec)) {
            # Only compute the correlation if both variables are not NA and are not identical
            if(!is.na(pIndVarVec[i]) & !is.na(pIndVarVec[j]) & pIndVarVec[i] != pIndVarVec[j]) {
                # Create a new row in the correlation data frame with the variable names and correlation coefficient
                newRow <- c(pIndVarVec[i], pIndVarVec[j], round(cor(pDF[pIndVarVec[i]], pDF[pIndVarVec[j]]), 3))
                corrDF[nrow(corrDF) + 1, ] <- newRow
            }
        }
    }
    # Convert the correlation column to numeric and add a column for the time frame
    corrDF <- corrDF %>% 
        mutate(
            correlation = as.numeric(correlation),
            timeFrame = pTimeFrame
        )
    # Return the correlation data frame
    return(corrDF)
}

#=================================================
# MODEL TRAINING
#=================================================

#' @description This function performs k-fold cross-validation on a linear regression model.
#' @param pDF: data frame to be used
#' @param pKFolds: number of folds to be used in cross-validation
#' @param pFormula: formula for the linear regression model
#' @usage cross_validation_func(my_data_frame, 5, "response ~ predictor1 + predictor2")
#' @return Data frame with predicted values from cross-validation
cross_validation_func <- function(pDF, pKFolds, pFormula, pModel, pPredCol) {
    # Get the number of rows in the data frame
    nRows <- nrow(pDF)
    # Split the data into k folds using kWayCrossValidation function from a package (not shown here)
    splitPlan <- kWayCrossValidation(nRows, pKFolds, NULL, NULL)
    k <- pKFolds
    pDF[pPredCol] <- 0
    for(i in 1:k) {
        # Get the indices of the current fold for training and testing
        split <- splitPlan[[i]]
        if(pModel == "linear_regression"){
            # Train a linear regression model on the training data
            model <- lm(pFormula, data = pDF[split$train, ])
            # Use the trained model to predict the response variable for the testing data
            pDF[pPredCol][split$app, ] <- predict(model, pDF[split$app, ])
        } else if(pModel == "random_forest"){
            model <- ranger(pFormula, pDF[split$train, ], num.trees = 500, respect.unordered.factors = "order")
            # Use the trained model to predict the response variable for the testing data
            pDF[pPredCol][split$app, ] <- predict(model, pDF[split$app, ])$predictions
        } else {
            stop("Please provide a valid model algorithm ('linear_regression', 'random_forest')")
        }
    }
    # Return the data frame with predicted values from cross-validation
    return(pDF)
}

#=================================================
# MODEL PERFORMANCE METRICS
#=================================================

#' @description This function calculates the root mean squared error (RMSE) between two columns of a data frame.
#' @param pDF: data frame to be used
#' @param pCol: name of the column containing the true values
#' @param pPredCol: name of the column containing the predicted values
#' @usage rmse_func(my_data_frame, "actual_col", "predicted_col")
#' @return RMSE value
rmse_func <- function(pDF, pCol, pPredCol) {
    # Calculate the error between the predicted and true values
    err <- (pDF[pPredCol] - pDF[pCol])[[pPredCol]]
    # Square the errors
    err2 <- err^2
    # Calculate the mean of the squared errors and take the square root to get RMSE
    rmse <- sqrt(mean(err2))
    # Return the RMSE value
    return(rmse)
}

#' This function calculates the coefficient of determination (R-squared) for a linear regression model.
#' @param pDF: data frame to be used
#' @param pCol: name of the column containing the true values
#' @param pPredCol: name of the column containing the predicted values
#' @usage r_squared_func(my_data_frame, "actual_col", "predicted_col")
#' @return R-squared value
r_squared_func <- function(pDF, pCol, pPredCol) {
    # Calculate the error between the predicted and true values
    err <- pDF[pPredCol] - pDF[pCol]
    # Calculate the residual sum of squares (RSS)
    rss <- sum(err[, pPredCol]^2)
    # Calculate the total sum of squares (SSTO)
    toterr <- pDF[pCol][[pCol]] - mean(as.list(pDF[pCol])[[pCol]])
    sstot <- sum(toterr^2)
    # Calculate the R-squared value
    r_squared <- 1 - (rss/sstot)
    # Return the R-squared value
    return(r_squared)  
}
