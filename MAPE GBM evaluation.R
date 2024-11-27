## setwd - please change
setwd("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Evaluation")
## packags
library(tidyverse)

#### load simulations and data - please change file path ####
load("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/GBM simulations/GBM_simulations.RData")
week_1_data <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Parameters/week_1_data.csv")
week_1_data <- week_1_data %>% arrange(week_1_data[, 1])
week_2_data <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Parameters/week_2_data.csv")
week_2_data <- week_2_data %>% arrange(week_2_data[, 1])
month_1_data <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Parameters/month_1_data.csv")
month_1_data <- month_1_data %>% arrange(month_1_data[, 1])
month_6_data <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Parameters/month_6_data.csv")
month_6_data <- month_6_data %>% arrange(month_6_data[, 1])

#### one week MAPE ####
week_1_MAPEs <- data.frame(stock = character(), MAPE = numeric(), stringsAsFactors = FALSE)
# loop thru each stock
for (i in 1:nrow(week_1_data)) {
  stock_name <- week_1_data[i, 1]
  actual_price <- week_1_data[i,2]
  
  stock_simulation <- simulations[[i]]
  simulated_prices_day_5 <- stock_simulation %>%
    filter(trading_days ==5) %>%
    pull(price)
  # MAPE
  percentage_errors_day_5 <- abs((actual_price - simulated_prices_day_5) / actual_price) * 100
  mape <- mean(percentage_errors_day_5) 
  
  week_1_MAPEs <- rbind(week_1_MAPEs, data.frame(stock = stock_name, MAPE = mape))
}
write.csv(week_1_MAPEs, file = "GBM_week_1_MAPEs.csv", row.names = FALSE)

#### two week MAPE ####
week_2_MAPEs <- data.frame(stock = character(), MAPE = numeric(), stringsAsFactors = FALSE)
# loop thru each stock
for (i in 1:nrow(week_2_data)) {
  stock_name <- week_2_data[i, 1]
  actual_price <- week_2_data[i,2]
  
  stock_simulation <- simulations[[i]]
  simulated_prices_day_10 <- stock_simulation %>%
    filter(trading_days ==10) %>%
    pull(price)
  # MAPE
  percentage_errors_day_10 <- abs((actual_price - simulated_prices_day_10) / actual_price) * 100
  mape <- mean(percentage_errors_day_10) 
  
  week_2_MAPEs <- rbind(week_2_MAPEs, data.frame(stock = stock_name, MAPE = mape))
}
write.csv(week_2_MAPEs, file = "GBM_week_2_MAPEs.csv", row.names = FALSE)

#### one month MAPE ####
month_1_MAPEs <- data.frame(stock = character(), MAPE = numeric(), stringsAsFactors = FALSE)
# loop thru each stock
for (i in 1:nrow(month_1_data)) {
  stock_name <- month_1_data[i, 1]
  actual_price <- month_1_data[i,2]
  
  stock_simulation <- simulations[[i]]
  simulated_prices_month_1 <- stock_simulation %>%
    filter(trading_days ==19) %>%
    pull(price)
  # MAPE
  percentage_errors_month_1 <- abs((actual_price - simulated_prices_month_1) / actual_price) * 100
  mape <- mean(percentage_errors_month_1) 
  
  month_1_MAPEs <- rbind(month_1_MAPEs, data.frame(stock = stock_name, MAPE = mape))
}
write.csv(month_1_MAPEs, file = "GBM_month_1_MAPEs.csv", row.names = FALSE)

#### 6 month MAPE ####
month_6_MAPEs <- data.frame(stock = character(), MAPE = numeric(), stringsAsFactors = FALSE)
# loop thru each stock
for (i in 1:nrow(month_6_data)) {
  stock_name <- month_6_data[i, 1]
  actual_price <- month_6_data[i,2]
  
  stock_simulation <- simulations[[i]]
  simulated_prices_month_6 <- stock_simulation %>%
    filter(trading_days ==124) %>%
    pull(price)
  # MAPE
  percentage_errors_month_6 <- abs((actual_price - simulated_prices_month_6) / actual_price) * 100
  mape <- mean(percentage_errors_month_6) 
  
  month_6_MAPEs <- rbind(month_6_MAPEs, data.frame(stock = stock_name, MAPE = mape))
}
write.csv(month_6_MAPEs, file = "GBM_month_6_MAPEs.csv", row.names = FALSE)

