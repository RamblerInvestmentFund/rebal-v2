<p align="center">
  <img height=300 src="https://raw.githubusercontent.com/RamblerInvestmentFund/assets/master/rif_logo.jpeg">
</p>

# Portfolio Rebalancer

> *take a simple idea and take it seriously* - Charlie Munger

The rebal-v2 is a rewrite of the original [Rambler Investment Fund's rebalancer](https://github.com/RamblerInvestmentFund/REBAL). 

The result was a radically simplified rebalancer with a significantly smaller codebase. 

# Method

1. Loads the portfolio tickers from an Excel spreadsheet.
2. Pulls end of day prices (3yr minimum) for each position.
3. Performs quadratic utility optimization to find optimal weights for each postition.
4. Exports the weights to another Excel sheet for consumption.



# Contributors

- Sam Sendelbach
- Ben Karenas 
