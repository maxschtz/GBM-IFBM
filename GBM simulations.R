## this was programmed with the help of the template of the following site:
# https://robotwealth.com/efficiently-simulating-geometric-brownian-motion-in-r/

## seed
set.seed(2024)

## packages
library(tidyverse)

## setwd - please change
setwd("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/GBM simulations")

## get data - please adjust
drift <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Parameters/DAX40_drift.csv")
S0s <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Parameters/S0_data.csv")
volatility <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis/Parameters/DAX40_volatility.csv")

## making sure they are in the same order 
drift <- drift %>% arrange(drift[, 1])
S0s <- S0s %>% arrange(S0s[, 1])
volatility <- volatility %>% arrange(volatility[, 1])



## defining function
gbm_loop <- function(nsim, t, mu, sigma, S0, dt) {
  gbm <- matrix(ncol = nsim, nrow = t)
  for (simu in 1:nsim) {
    gbm[1, simu] <- S0
    for (day in 2:t) {
      # random number from normal distribution
      epsilon <- rnorm(1)
      #gbm process
      gbm[day, simu] <- gbm[(day-1), simu] * exp((mu - sigma^2 / 2) * dt + sigma * epsilon * sqrt(dt))
    }
  }
  
  return(gbm)
}

## Simulating:
# 124 trading days (trading days in the 2nd 6 months)
# S0 = stock value at end of 2023-11-30
## number of simulations
nsim <- 10000
## number of trading days to be simulated
t <- 125
# actually 124, but Day 2 is the first day being simulated after the inital day (S0)
# day 125 is the end of the 124 trading day (30.05.2024)
## time increments - daily
dt <- 1

## list for all simualtions
simulations <- list()

## GBM looping thru each stock
for (i in 1:40) {
  stock_name <- drift[i,1]
  mu <- drift[i,2]
  S0 <-S0s[i,2]
  sigma <- volatility[i,2]
  
  #running gbm simulation
  gbm_simulation <- gbm_loop(nsim,t, mu, sigma, S0, dt)
  
  # store in df
  gbm_df <- as.data.frame(gbm_simulation) %>%
    mutate(ix = 1:nrow(gbm_simulation)) %>%
    # removing S0
    filter(ix != 1) %>%
    mutate(ix = ix -1) %>%
    rename(trading_days = ix) %>%
    pivot_longer(-trading_days, names_to ='sim', values_to = 'price') %>%
    mutate(stock = stock_name)
  # Store df
  simulations[[i]] <- gbm_df
}

# save
save(simulations, file = "GBM_simulations.RData")

## pplot
for (i in 1:40) {
  stock_data <- simulations[[i]]
  stock_name <- stock_data$stock[1]
  
  plot <- ggplot(stock_data, aes(x = trading_days, y = price, color = sim)) +
    geom_line(alpha = 0.3) +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(title = paste("10000 GBM simulations for :", stock_name),
         x = "Trading Days",
         y = "Simulated Price")
  
  # print and save
  print(plot)
  ggsave(filename = paste0("GBM_simulations_", stock_name, ".png"), plot = plot, width = 8, height = 6)
}

