library(tidyverse)
library(openxlsx)
library(tidyquant)
library(Quandl)
library(timetk)
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)

# Quandl API Key for our EOD stock prices subscription
quandl_api_key("mRJDZwn3giwAm1kowtFr")

# Read tickers, buckets, and min/max weights from Excel file
## ASSUMPTIONS FOR SHEET1:
## 1. At least 2 securities per bucket
## 2. Each security in a given bucket has the same min weights and max weights
## 3. All min/max weights are viable (for a given bucket: sum of min weights must be <1, sum of max weights must be >1)
xl_sheet1 <- readxl::read_excel("tickers.xlsx", sheet = "Sheet1")
## ASSUMPTIONS FOR SHEET2:
## 1. 
xl_sheet2 <- readxl::read_excel("tickers.xlsx", sheet = "Sheet2")

# Split the tickers into their buckets
tickers_list <- split(xl_sheet1, f =  xl_sheet1$Bucket)

# For loop to iterate over each bucket
counter <- 1
for (i in tickers_list) {
  # Create bucket names
  name <- paste("Bucket", counter, sep = "")
  assign(name, i[1])
  
  # Pull tickers from buckets and add EOD prefix to get data from Quandl between 2 dates (must be at least 3 years)
  stock_prices <- group_by(tq_get(sprintf("EOD/%s", i[1] %>% pull()), get = "quandl", from = "2016-01-01", to = "2020-11-01"), symbol)
  name_prices <- paste(name, "_prices", sep = "")
  assign(name_prices, stock_prices)
  
  # Calculate returns on the stocks over the time period and expand the dataframe so the columns are the ticker names
  stock_returns <- stock_prices %>%
    tq_transmute(select     = adj_close,
                 mutate_fun = periodReturn,
                 period     = "daily",
                 type       = "log",
                 col_rename = "returns")
  stock_returns <- pivot_wider(stock_returns, names_from = symbol, values_from = returns)
  
  # Coerce the stock returns into a xts time series
  stock_returns <- stock_returns %>% tk_xts(silent = TRUE)
  name_returns <- paste(name, "_returns", sep = "")
  assign(name_returns, stock_returns)
  
  # Create a portfolio
  init <- portfolio.spec(assets = colnames(stock_returns))
  
  # Add the constraint on the minimum and maximum weight that a single stock can have
  init <- add.constraint(portfolio = init, type = "box", min = min(i[3]), max = max(i[4]))
  
  # Add the quadratic objective for the solver
  qu <- add.objective(portfolio = init, type = "quadratic_utility", risk_aversion = 1)
  
  # Optimize the portfolio based on the above constraints and objectives using quadratic programming
  opt_qu <- optimize.portfolio(R = stock_returns, portfolio = qu, optimize_method = "quadprog")
  print(opt_qu)
  
  # Extract the optimized weights and put them in a data frame with the tickers
  weights <- extractWeights(opt_qu) %>% as.data.frame()
  name_weights <- paste(name, "_weights", sep = "")
  assign(name_weights, cbind(i[1], weights))
  
  # Increase counter
  counter <- counter + 1
}

# Export the final data frame to an Excel file
#wb <- createWorkbook()
#addWorksheet(wb, "Sheet1")
#writeData(wb, "Sheet1", df, colNames = FALSE)
#saveWorkbook(wb, "weights.xlsx", overwrite = TRUE)

