## input file path please adjust
input_file_path <- "/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Data/DAX40_Data.csv"

## load stock data
DAX40_data <- read.csv(input_file_path)

## filter for the first 6 months
DAX40_data <- subset(DAX40_data, Date >= "2023-06-01" & Date <= "2023-11-30")

## remove date column
DAX40_data <- DAX40_data[,-1]

## function to calculate daily returns
calculate_daily_returns <- function(return) {
  daily_returns <- diff(return) / head(return, -1) 
  return(daily_returns)
}

## calculate daily volatility
calculate_volatility <- function(volatiliy) {
  # daily returns
  daily_returns <- calculate_daily_returns(volatiliy)
  #  mean of the daily return
  mean_return <- mean(daily_returns, na.rm = TRUE)
  # volatiliity
  M <- length(daily_returns)
  daily_volatility <- sqrt(sum((daily_returns - mean_return)^2) / (M - 1))
  return(daily_volatility)
}

## apply function
DAX40_volatility <- apply(DAX40_data, 2, calculate_volatility)

## df
DAX40_volatility <- data.frame(DailyVolatility = DAX40_volatility)

## save
write.csv(DAX40_volatility, file = "DAX40_volatility.csv", row.names = TRUE)
