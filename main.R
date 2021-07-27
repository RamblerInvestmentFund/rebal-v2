library(tidyverse)
library(openxlsx)
library(tidyquant)
library(Quandl)
library(timetk)
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)

# Quandl API Key for our EOD stock prices subscription
quandl_api_key("#####")

# Pull tickers and groups from Excel file and order alphabetically by group
xl_tickers <- readxl::read_excel("tickers.xlsx", sheet = "Tickers")
xl_tickers <- xl_tickers[order(xl_tickers$Group),]
xl_groups <- readxl::read_excel("tickers.xlsx", sheet = "Groups")
xl_groups <- xl_groups[order(xl_groups$Group),]

# Split the tickers into a list of lists by their groups and create a numeric list of lists (via for loop, inefficient solution sorry)
split_groups = split(rownames(xl_tickers), xl_tickers$Group)
groups <- list()
counter <- 1
for (i in split_groups) {
  groups[[counter]] <- as.numeric(i)
  counter <- counter + 1
}

# Add EOD prefix to tickers so they pull end of day prices from Quandl
modified_tickers <- sprintf("EOD/%s", xl_tickers$Ticker)

# Pull EOD stock prices from Quandl (must be at least 3 years for it to work properly)
stock_prices <- tq_get(modified_tickers, get = "quandl", from = Sys.Date()-(365*3), to = Sys.Date())

# Calculate returns on the stocks over the time period
stock_returns <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adj_close,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "returns") %>%
  pivot_wider(names_from = symbol, values_from = returns)

# Coerce the stock returns into a xts time series
stock_returns <- stock_returns %>% tk_xts(silent = TRUE)

# Create a portfolio
pspec <- portfolio.spec(assets = colnames(stock_returns))

# Add full investment constraint so the sum of all weights is 1
pspec <- add.constraint(portfolio=pspec,
                        type="full_investment")

# Add constraint on the min and max for each asset
pspec <- add.constraint(portfolio = pspec, 
                       type = "box", 
                       min = xl_tickers$MinWeight, 
                       max = xl_tickers$MaxWeight)

# Add constraint on the min and max for each group
pspec <- add.constraint(portfolio = pspec, 
                        type = "group", 
                        groups = groups, 
                        group_min = xl_groups$GroupMin, 
                        group_max = xl_groups$GroupMax)

# Add the quadratic objective for the solver
qu <- add.objective(portfolio = pspec, 
                    type = "quadratic_utility", 
                    risk_aversion = 1)

# Optimize the portfolio based on the above constraints and objectives using quadratic programming
opt_qu <- optimize.portfolio(R = stock_returns, 
                             portfolio = qu, 
                             optimize_method = "quadprog",
                             trace = TRUE)
print(opt_qu)

# Extract the mean and standard deviation and annualize them
obj_measures <- extractObjectiveMeasures(opt_qu)
ann_return <- round(obj_measures$mean * 252, 4)
print(paste("Expected Annualized Return:", ann_return))
ann_vol <- round(obj_measures$StdDev * sqrt(252), 4)
print(paste("Standard Deviation:", ann_vol))

# Calculate portfolio utility
utility <- round(ann_return - 0.5 * 1 * ann_vol**2, 4)
print(paste("Portfolio Utility:", utility))

# Extract the optimized weights and put them in a data frame with the tickers
weights <- extractWeights(opt_qu) %>% as.data.frame()
df <- cbind(xl_tickers$Ticker, round(weights, 4))
colnames(df) <- c("Ticker","Weight")

# Combine stats into a data frame
stat_return <- list("Expected Annualized Return", ann_return)
stat_vol <- list("Standard Deviation", ann_vol)
stat_utility <- list("Portfolio Utility", utility)
stats <- rbind(stat_return, stat_vol, stat_utility) %>% as.data.frame()

# Export the final data frame to an Excel file
wb <- createWorkbook()
addWorksheet(wb, "Weights")
writeData(wb, "Weights", df)
addWorksheet(wb, "Stats")
writeData(wb, "Stats", stats, colNames = FALSE)
saveWorkbook(wb, "weights.xlsx", overwrite = TRUE)

