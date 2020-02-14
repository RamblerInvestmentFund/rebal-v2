library(Quandl)
library(config)

# Loads a config file. The default path for loading the config
# is config.yml in the current directory.
config <- config::get()

# Sets the API key for quandl
Quandl.api_key('62FeHrDxMUhWtQ1yWTsr')



eod_path <- function(ticker) {
  # this is just formatting the request to the API correctly.
  # e.g. EOD/<YOUR_TICKER>
  return(sprintf("EOD/%s", ticker))
}

for (ticker in config$tickers) {
  data <- Quandl(eod_path(ticker), start_date='2017-12-28', end_date='2017-12-28')
  print(data)
}




