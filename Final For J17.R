# Load necessary libraries
library(xts)
library(imputeTS)
 

# Set working directory
setwd('C:/Users/mfeizbahr/Desktop/Time series/Main Files')

# Read data (daily data)
mydata <- read.csv('j17.csv')

# Convert Date column to Date format
mydata$Date <- as.Date(mydata$Date, format = '%m/%d/%Y')

# Create an xts object for water level data
WL.xts <- xts(mydata$Elevation, order.by = mydata$Date)

# Check for missing values
if (any(is.na(WL.xts))) {
  # Impute missing values using na_interpolation function
  WL.xts <- na_interpolation(WL.xts)
  print("Missing Values Imputed.")
}

plot(WL.xts, xlab = 'Date', ylab = 'Elevation', col = 'darkred')


# Calculate and plot ACF and PACF
ACF <- acf(WL.xts, lag.max = 30, plot = TRUE, main = "ACF")
PACF <- pacf(WL.xts, lag.max = 30, plot = TRUE, main = "PACF")
 
# Convert to time series
WL.ts <- ts(mydata$Elevation, frequency = 365, start = c(2023, 1))

# Perform additive decomposition
WL.decomp <- decompose(WL.ts, type = "additive")

# Plot the decomposed time series
autoplot(WL.decomp)


# Compute and plot the cross-correlation function with a second time series (TS.y)
# Replace TS.y with your own second time series data if available
#TS.y <- read.csv('j17.csv')
#TS.y$Date <- as.Date(TS.y$Date, format = '%m/%d/%Y')
#TS.y.xts <- xts(TS.y$Value, order.by = TS.y$Date)

#ccf(WL.xts[,1], TS.y.xts[,1])

# Summarize the water level data

mydata$Date <- as.Date(mydata$Date, format = '%m/%d/%Y')

# Check for missing values in Discharge column
if (any(is.na(mydata$Elevation))) {
  print("Discharge column contains missing values.")
}

# Calculate summary statistics for Discharge column
summary(mydata$Elevation)


 
 
 
# Create an xts object for water level data
WL.xts <- xts(mydata$Elevation, order.by = mydata$Date)

# Calculate the 30-day moving average
MA30 <- rollmean(WL.xts, k = 30, na.pad = TRUE)

# Calculate the 90-day moving average
MA90 <- rollmean(WL.xts, k = 90, na.pad = TRUE)

# Calculate the 365-day moving average
MA365 <- rollmean(WL.xts, k = 365, na.pad = TRUE)

# Plot the 30-day moving average
plot(MA30, xlab = 'Date', ylab = 'Elevation', main = '30-day Moving Average', col = "red")

# Plot the 90-day moving average
plot(MA90, xlab = 'Date', ylab = 'Elevation', main = '90-day Moving Average', col = "green")

# Plot the 365-day moving average
plot(MA365, xlab = 'Date', ylab = 'Elevation', main = '365-day Moving Average', col = "blue")


# Add a legend to the plot
legend("topleft", legend = c("30-day MA", "90-day MA", "365-day MA"), 
       col = c("red", "green", "blue"), lty = 1)


summary(MA30); summary(MA90); summary(MA365)






