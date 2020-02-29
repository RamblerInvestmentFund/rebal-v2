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

# Pull tickers from Excel file
tickers = readxl::read_excel("tickers.xlsx", col_names = FALSE) %>% pull()

# Add EOD prefix to tickers so they pull end of day prices from QUandl
modified_tickers = sprintf("EOD/%s", tickers)

# Pull EOD stock prices from Quandl between 2 dates, must be at least 3 years for it to work properly
stock_prices = group_by(tq_get(modified_tickers, get = "quandl", from = "2016-01-01", to = "2020-01-01"), symbol)

# Calculate returns on the stocks over the time period
stock_returns = stock_prices %>%
  tq_transmute(select     = adj_close,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "arithmetic",
               col_rename = "returns") %>%
  pivot_wider(names_from = symbol, values_from = returns)

# Coerce the stock returns into a xts time series
stock_returns = stock_returns %>% tk_xts(silent = TRUE)

# Create a portfolio
init = portfolio.spec(assets = colnames(stock_returns))

# Add the constraint on the minimum and maximum weight that a single stock can have
init = add.constraint(portfolio = init, type = "box", min = 0.005, max = 0.1)

# Add the quadratic objective for the solver
qu = add.objective(portfolio = init, type = "quadratic_utility", risk_aversion = 1)

# Optimize the portfolio based on the above constraints and objectives using quadratic programming
opt_qu = optimize.portfolio(R = stock_returns, portfolio = qu, optimize_method = "quadprog")
print(opt_qu)

# Extract the optimized weights and put them in a data frame with the tickers
weights = extractWeights(opt_qu) %>% as.data.frame()
df = cbind(tickers, weights)

# Export the final data frame to an Excel file
wb = createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", df, colNames = FALSE)
saveWorkbook(wb, "weights.xlsx", overwrite = TRUE)
