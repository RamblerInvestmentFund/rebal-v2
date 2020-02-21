library(Quandl)
library(config)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(timetk)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)



# Loads a config file. The default path for loading the config
# is config.yml in the current directory.
config <- config::get()


modified_tickers <- sprintf("EOD/%s", config$tickers)


quandl_api_key("mRJDZwn3giwAm1kowtFr")




stock_prices2 <- group_by(tq_get(modified_tickers, get = "quandl", from = "2016-01-01", to = "2016-12-31"), symbol)

stock_prices2%>%head()

stock_returns <- stock_prices2 %>%
  tq_transmute(select     = adj_close,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "arithmetic",
               col_rename = "returns") %>%
  spread(key = symbol, value = returns)


stock_returns%>%head()

stock_returns=stock_returns%>%tk_xts(silent = TRUE)






init <- portfolio.spec(assets=colnames(stock_returns))
init <- add.constraint(portfolio=init, type="leverage", min_sum=0.99, max_sum=1.01)
init <- add.constraint(portfolio=init, type="box", min=0.05, max=0.65)


qu <- add.objective(portfolio=init, type="return", name="mean")
qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion=0.25)

print(qu)



print(stock_returns)
opt_qu <- optimize.portfolio(R=stock_returns, portfolio=qu,
                             optimize_method="ROI",
                             trace=TRUE)
print(opt_qu)

print(extractWeights(opt_qu))

weights <- extractWeights(opt_qu)
write.table(weights, file = "weights.csv", sep=", ", col.names = FALSE)
