# Load library for reading Excel files
library(readxl)

# Read the Excel file and use the first row as column names
data <- read_excel("C:\\Users\\as922\\Downloads\\istanbulstockexchange.xlsx", col_names = TRUE)

# Question 1
# Spliting Dataset
# Load  library
library(dplyr)

data$date <- as.Date(data$date)

# Define the split date (15th September 2010)
split_date <- as.Date("2010-09-15")

# Split the data into training and test datasets
training_data <- data %>% filter(date <= split_date)
test_data <- data %>% filter(date > split_date)

# QUestion 2
# Plot Histogram 
# Load the necessary libraries 
library(ggplot2)
library(gridExtra)

# Create a directory to store the plots
dir.create("histograms")

# List of variable names to create histograms for
variable_names <- c("ise", "sp", "dax", "ftse", "nikkei", "bovespa", "eu", "em")

# Create an empty list to store plot objects
plot_list <- list()

# Create histograms for all variables and add them to the plot list
for (var in variable_names) {
  p <- ggplot(training_data, aes(x = .data[[var]])) +
    geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
    labs(title = paste("Histogram of", var, "Variable"))
  
  plot_list <- c(plot_list, list(p))
}

# Arrange the plots in a grid and save as a single image
grid.arrange(grobs = plot_list, ncol = 2)


## Summary of variables
summary_data <- summary(training_data)

# Print the summary statistics
print(summary_data)

## Outliers in training datasets

# Function to detect outliers using Z-score and provide count
detect_outliers <- function(data, threshold = 3) {
  z_scores <- scale(data)
  outliers <- abs(z_scores) > threshold
  num_outliers <- sum(outliers)
  return(list(indices = which(outliers), count = num_outliers))
}

# Loop through each variable and detect outliers
for (var in variable_names) {
  result <- detect_outliers(training_data[[var]])
  
  cat("Outliers in", var, ": Count =", result$count, "\n")
  if (result$count > 0) {
    cat("Indices:", result$indices, "\n")
  } else {
    cat("No outliers found.\n")
  }
}

# Missing data
# Find missing data for each column
missing_count <- sapply(training_data, function(x) sum(is.na(x)))

# Display count of missing data for each variable
print(missing_count)

# Distribution

# Perform Shapiro-Wilk test and print p-values with normality indication
shapiro_results <- sapply(training_data[variable_names], function(x) {
  shapiro_test <- shapiro.test(x)
  p_value <- shapiro_test$p.value
  is_normal <- ifelse(p_value > 0.05, "Likely Normal", "Not Likely Normal")
  return(c(p_value, is_normal))
})

# Print results
rownames(shapiro_results) <- c("P-Value", "Normality")
print(shapiro_results)

# Correlation analysis

# Load the psych package for corr.test function
install.packages("psych")
library(psych)


# Perform correlation analysis with p-values
correlation_matrix <- corr.test(training_data[, variable_names], method = "pearson")

# Print correlations with p-values less than 0.05
significant_correlations <- correlation_matrix$r
significant_correlations[correlation_matrix$p > 0.05] <- NA
print(significant_correlations)


# Question 3: Modelling
install.packages("tseries")
# Load necessary packages
library(tseries)

# Create a time series  object
ts_data <- ts(training_data$ise, frequency = 365)

# Take first differences to obtain stationarity
differenced_ts <- diff(ts_data)

# Perform Augmented Dickey-Fuller test
adf_test <- adf.test(differenced_ts)

# Print the test results
print(adf_test)

# ACF and PACF
# Load necessary packages
library(forecast)

# Fit ARIMA and auto ARIMA models and store AIC and BIC values
model_table <- data.frame(Model = character(),
                          AIC = numeric(),
                          BIC = numeric(),
                          stringsAsFactors = FALSE)

# Fit ARIMA model
arima_model <- arima(ts_data, order = c(0, 0, 0))
model_table <- rbind(model_table, c("ARIMA(0,0,0)", arima_model$aic, arima_model$bic))

# Fit auto ARIMA model
autoarima_model <- auto.arima(ts_data)
model_table <- rbind(model_table, c("Auto ARIMA", autoarima_model$aic, autoarima_model$bic))

# Print the formatted table
print(model_table)

# Set up the layout for side-by-side plots
par(mfrow = c(1, 2))  # Arrange plots in a 1x2 grid

# ACF plot
acf(differenced_ts, main = "ACF of Differenced Time Series")

# PACF plot
pacf(differenced_ts, main = "PACF of Differenced Time Series")

# Assumptions
# Residuals
# Obtain the residuals from the model
residuals <- residuals(autoarima_model)

dev.off()
# Plot the residuals
plot(residuals, type = "l", main = "Residuals from Auto ARIMA Model")

# Perform Ljung-Box test for autocorrelation in residuals
ljung_box_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
print(ljung_box_test)

# Set up the layout for side-by-side plots
par(mfrow = c(1, 2))  # Arrange plots in a 1x2 grid

# Plot ACF of residuals
acf(residuals, lag.max = 20, main = "ACF of Residuals")

# Plot PACF of residuals
pacf(residuals, lag.max = 20, main = "PACF of Residuals")


#Forecast the Istanbul Stock Exchange 100 Index for the test dataset 
# Load library
library(forecast)

# Forecast using ARIMA model
forecast_arima <- forecast(arima_model, h = length(ts_data)/4)

# Forecast using auto ARIMA model
forecast_autoarima <- forecast(autoarima_model, h = length(ts_data)/4)

# Plot the forecasted values for ARIMA
plot(forecast_arima, main = "Forecast using ARIMA (0,0,0)")

# Plot the forecasted values for Auto ARIMA
plot(forecast_autoarima, main = "Forecast using Auto ARIMA")

# Add actual values to the plots
lines(ts_data, col = "blue")

# Extract forecasted values from ARIMA
point_forecast_arima <- forecast_arima$mean

# Extract forecasted values from Auto ARIMA
point_forecast_autoarima <- forecast_autoarima$mean

# Calculate the mean of the arima forecasted values
mean_forecast_arima <- mean(point_forecast_arima)

# Print the mean forecast
print(mean_forecast_arima)

# Calculate the mean of the autoarima forecasted values
mean_forecast_autoarima <- mean(point_forecast_autoarima)

# Print the mean forecast
print(mean_forecast_autoarima)

# Extract actual values
actual_values <- test_data$ise

# Align data to the shorter length
min_length <- min(length(point_forecast_arima), length(point_forecast_autoarima), length(actual_values))
point_forecast_arima <- point_forecast_arima[1:min_length]
point_forecast_autoarima <- point_forecast_autoarima[1:min_length]
actual_values <- actual_values[1:min_length]

# Calculate accuracy measures for ARIMA
accuracy_measures_arima <- accuracy(ts(point_forecast_arima), ts(actual_values))

# Calculate accuracy measures for Auto ARIMA
accuracy_measures_autoarima <- accuracy(ts(point_forecast_autoarima), ts(actual_values))

# Print accuracy measures for ARIMA
print("Accuracy measures for ARIMA:")
print(accuracy_measures_arima)

# Print accuracy measures for Auto ARIMA
print("Accuracy measures for Auto ARIMA:")
print(accuracy_measures_autoarima)



