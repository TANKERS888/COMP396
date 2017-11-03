source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('example_strategies.R');

# load data
dataList <- getData(directory="PART1")

# choose strategy from example_strategies
strategy <- "big_spender"
          

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R



sMult <- 0.20 # slippage multiplier

results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')
