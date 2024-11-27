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

#### Mean price calculation ####
## function
calculate_mean <- function(simulations, trading_day) {
  mean_simulated_values <- data.frame(stock = character(), mean_simulated = numeric(),stringsAsFactors = FALSE)
  # Loop
  for (i in 1:length(simulations)) {
    stock_simulation <- simulations[[i]]
    stock_name <- stock_simulation$stock[1]
    # trading day
    simulated_prices <- stock_simulation %>%
      filter(trading_days ==trading_day) %>%
      pull(price)
    #mean
    mean_price <- mean(simulated_prices, na.rm = TRUE)
    mean_simulated_values <- rbind(mean_simulated_values,data.frame(stock = stock_name, mean_simulated = mean_price))
  }
  return(mean_simulated_values)
}

# 1week (5 trading days)
mean_simulations_week_1 <- calculate_mean(simulations, trading_day = 5)
# 2 weeks (10 trading days)
mean_simulations_week_2 <- calculate_mean(simulations, trading_day = 10)
# 1 month (Dec), 19 trading days
mean_simulations_month_1 <- calculate_mean(simulations, trading_day = 19)
# 6 month, 124 trading days
mean_simulations_month_6 <- calculate_mean(simulations, trading_day = 124)



#### Merge and log transorm ####
# week 1
merged_week_1 <- week_1_data %>%
  rename(stock = X, actual_price = week_1) %>%
  inner_join(mean_simulations_week_1, by= c("stock"))
merged_week_1 <- merged_week_1 %>%
  mutate(
    log_actual = log(actual_price),
    log_simulated = log(mean_simulated)
  )
# week 2
merged_week_2 <- week_2_data %>%
  rename(stock = X, actual_price = week_2) %>%
  inner_join(mean_simulations_week_2, by= c("stock"))
merged_week_2 <- merged_week_2 %>%
  mutate(
    log_actual = log(actual_price),
    log_simulated = log(mean_simulated)
  )
# 1 month
merged_month_1 <- month_1_data %>%
  rename(stock = X, actual_price = month_1) %>%
  inner_join(mean_simulations_month_1, by= c("stock"))
merged_month_1 <- merged_month_1 %>%
  mutate(
    log_actual = log(actual_price),
    log_simulated = log(mean_simulated)
  )
# 6 month
merged_month_6 <- month_6_data %>%
  rename(stock = X, actual_price = month_6) %>%
  inner_join(mean_simulations_month_6, by= c("stock"))
merged_month_6 <- merged_month_6 %>%
  mutate(
    log_actual = log(actual_price),
    log_simulated = log(mean_simulated)
  )

#### t test  ####
t_test_result_week_1 <- t.test(
  merged_week_1$log_simulated,
  merged_week_1$log_actual,
  paired = TRUE
)

t_test_result_week_2 <- t.test(
  merged_week_2$log_simulated,
  merged_week_2$log_actual,
  paired = TRUE
)

t_test_result_month_1 <- t.test(
  merged_month_1$log_simulated,
  merged_month_1$log_actual,
  paired = TRUE
)
t_test_result_month_6 <- t.test(
  merged_month_6$log_simulated,
  merged_month_6$log_actual,
  paired = TRUE
)
