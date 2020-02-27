library(tidyverse)
# library(config) # No longer needed if we aren't using config.yml
library(tidyquant)
library(Quandl)
library(timetk)
library(PortfolioAnalytics)
# library(DEoptim) # I dont think DEoptim is being used
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)



# Loads a config file. The default path for loading the config
# is config.yml in the current directory.
# config <- config::get()
# modified_tickers2 <- sprintf("EOD/%s", config$tickers)
# I commented out the above and replaced it with read excel because we will likely be provided an excel file with the tickers



tickers = readxl::read_excel("tickers.xlsx", col_names = FALSE) %>% pull()

modified_tickers = sprintf("EOD/%s", tickers)




quandl_api_key("mRJDZwn3giwAm1kowtFr")




stock_prices = group_by(tq_get(modified_tickers, get = "quandl", from = "2016-01-01", to = "2016-12-31"), symbol)

stock_prices %>% head()

stock_returns = stock_prices %>%
  tq_transmute(select     = adj_close,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "arithmetic",
               col_rename = "returns") %>%
  pivot_wider(names_from = symbol, values_from = returns)


stock_returns %>% head()

stock_returns=stock_returns %>% tk_xts(silent = TRUE)






init = portfolio.spec(assets=colnames(stock_returns))
init = add.constraint(portfolio=init, type="box", min=0.005, max=0.1) #if portfolio is $1M, 0.5% is 5k and 10% is 100k
#why are init and qu set as different variables? could probably pipe (%>%) all the constraints and objectives into a single varaible

#qu = add.objective(portfolio=init, type="return", name="mean")
#qu = add.objective(portfolio=qu, type="risk", name="var", risk_aversion=0.25)
qu = add.objective(portfolio=init, type="quadratic_utility", risk_aversion=1) #i think the above 2 lines can be done in 1 step. also, changed risk aversion to 1 but should probably ask todd for feedback

print(qu)



head(stock_returns) #why are you printing stock returns? this will spam the console.... i changed to head() to limit output to 6 rows
opt_qu = optimize.portfolio(R=stock_returns, portfolio=qu,
                             optimize_method="ROI",
                             trace=TRUE) #i dont think trace=true has benefits if we are using ROI as the solver
print(opt_qu)

print(extractWeights(opt_qu))

weights <- extractWeights(opt_qu)
write.table(weights, file = "weights.csv", sep=", ", col.names = FALSE)
