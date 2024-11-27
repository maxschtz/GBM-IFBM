## input file paths - please adjust
input_file_path <- "/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Data/DAX40_Data.csv"

## load stock data
DAX40_data <- read.csv(input_file_path)

## filter for first 6 months
DAX40_data <- subset(DAX40_data, Date >= "2023-06-01" & Date <= "2023-11-30")

## remove date column
DAX40_data <- DAX40_data[,-1]

## function to calculate the drift
daily_drift <- function(drift) {
  daily_returns <- diff(drift) / head(drift, -1)
  # mean of the daily returns
  return(mean(daily_returns, na.rm = TRUE))
}

## apply fucntion
DAX40_drift <- apply(DAX40_data,2,daily_drift)
# df
DAX40_drift <- data.frame(DailyDrift = DAX40_drift)

## save
write.csv(DAX40_drift, file = "DAX40_drift.csv", row.names = TRUE)