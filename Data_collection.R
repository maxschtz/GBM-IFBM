## install packages
  install.packages("quantmod")

## fetch packages
  library(quantmod)

## Set dates
  Start_date <- as.Date("2023-06-01")
  End_date <- as.Date("2024-05-31")

## Define DAX40 tickers
  DAX40_tickers <- c(
    "ADS.DE", "AIR.DE", "ALV.DE", "BAS.DE", "BAYN.DE", "BMW.DE", "BNR.DE", "CON.DE", 
    "1COV.DE", "DBK.DE", "DB1.DE", "DTE.DE", "DHL.DE", "EOAN.DE", "FRE.DE", 
    "HEI.DE", "HEN3.DE", "IFX.DE", "MBG.DE", "MRK.DE", "MTX.DE", "MUV2.DE", 
    "PAH3.DE", "QIA.DE", "RWE.DE", "SAP.DE", "SRT3.DE", "SIE.DE", "SHL.DE", 
    "SY1.DE", "VOW3.DE", "VNA.DE", "ZAL.DE", "RHM.DE", "CBK.DE", "P911.DE",
    "ENR.DE", "BEI.DE", "DTG.DE", "HNR1.DE"
  )
  
## Fetch stock data from yahoo
  DAX40_Data <- lapply(DAX40_tickers, function(ticker) {
    getSymbols(ticker, src = "yahoo", from = Start_date, to = End_date, auto.assign = FALSE)
  })
  
## Getting adjusted prices (accounting for stock splits) and creating a df
  DAX40_Data_Adj <- lapply(DAX40_Data, function(stock_data) {
    data.frame(Date = index(stock_data), Adjusted = Ad(stock_data))
  })
  
## Merging into one df
  DAX40_Data <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), DAX40_Data_Adj)

# rename
  colnames(DAX40_Data) <- c("Date", sub("\\.DE\\.Adjusted$", "", colnames(DAX40_Data)[-1]))
  
# calculating the average of the value before and after the NA
  DAX40_Data$DHL <- na.approx(DAX40_Data$DHL, na.rm = FALSE)
  
## Save as csv
  write.csv(DAX40_Data, "DAX40_Data.csv", row.names = FALSE)