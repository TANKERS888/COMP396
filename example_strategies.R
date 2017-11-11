example_strategies <- c(
                        "macd",
                        "kdj"
                        )

example_params<-list(
                    "kdj"=list(series=1:10,nFastK=12,nFastD=9,nSlowD=26,lookback=50,size=1000),
                    "macd"=list(series=1:10,slow=26,fast=12,signal=9,size=400000,profitTarget=0.15                   
                    )
                      

load_strategy <- function(strategy) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}
