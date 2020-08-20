# load libraries
library(quantmod)
library(DataCombine)
library(xgboost)
library(reshape2)
library(tidyr)
library(plyr)
library(xts)
library(pracma)
library(data.table)
library(e1071)
library(dplyr)
library(tseries)

# Function 1: Data_Retrieve
Data_Retrieve <- function(tick,extra_tick,start,end = Sys.Date()){
  getSymbols(c(tick,extra_tick),src='yahoo', from = start, to = end)
  adj_close = get(tick[1])[,6]
  volume = get(tick[1])[,5]
  
  for (i in 2:length(tick)){
    new_data1 <- get(tick[i])[,6]
    new_data2 <- get(tick[i])[,5]
    adj_close = merge(adj_close,new_data1)
    volume = merge(volume,new_data2)
  }
  
  adj_close = as.data.frame(adj_close)
  adj_close$Date = row.names(adj_close)
  volume = as.data.frame(volume)
  volume$Date = row.names(volume)
  
  adj_close_long <- melt(adj_close, id.vars=c("Date"))
  volume_long <- melt(volume,  id.vars=c("Date"))
  colnames(adj_close_long)[3] = 'price'
  colnames(volume_long)[3] = 'volume'
  
  adj_close_long$variable = as.character(adj_close_long$variable)
  volume_long$variable = as.character(volume_long$variable)
  
  for (i in 1:nrow(adj_close_long)){adj_close_long$variable[i] = strsplit(adj_close_long$variable[i], split = '\\.')[[1]][1]}
  for (i in 1:nrow(volume_long)){volume_long$variable[i] = strsplit(volume_long$variable[i], split = '\\.')[[1]][1]}
  
  
  data = join(adj_close_long,volume_long,by = c('Date','variable'))
  colnames(data) = c('date', 'ticker', 'price', 'volume')
  data$date = as.Date(data$date)
  
  if (is.null(extra_tick)){
    return(data)
  }
  
  # get rid of carets in extra_tick
  for (i in 1:length(extra_tick)){extra_tick[i] = gsub("\\^","",extra_tick[i])}
  
  # add in pricing data for the extra features
  adj_close_extra = get(extra_tick[1])[,6]
  for (i in 2:length(extra_tick)){adj_close_extra = merge(adj_close_extra,get(extra_tick[i])[,6])}
  colnames(adj_close_extra) = extra_tick
  adj_close_extra = as.data.frame(adj_close_extra)
  adj_close_extra$date = as.Date(rownames(adj_close_extra))
  
  # merge crypto data with extra data (and downfill)
  data = merge(data,adj_close_extra,by = 'date',all=T)
  data = data[with(data, order(ticker, date)),]
  data = fill(data,all_of(extra_tick),.direction = 'down')
  
  return(data)
}

# params for Data_Retrieve
tickers = c('BTC-USD', 'ETH-USD', 'BCH-USD', 'ETC-USD', 'LTC-USD', 
            'DOGE-USD','DASH-USD','DCR-USD','PIVX-USD','XEM-USD',
            'XMR-USD','ZEC-USD','XRP-USD','USDT-USD','BNB-USD',
            'EOS-USD','ADA-USD','XLM-USD','LINK-USD','TRX-USD',
            'MIOTA-USD','VET-USD','BAT-USD','DGB-USD','ZRX-USD',
            'OMG-USD','ICX-USD','QTUM-USD','LSK-USD','REP-USD',
            'KNC-USD', 'WAVES-USD','SNT-USD')

####extra_tickers = c('^TNX','^GSPC','GBPUSD=X','JPY=X','^VIX','EURUSD=X')
extra_tickers = c()
start = '2016-01-01'

data = Data_Retrieve(tickers,extra_tickers,start)

 # Function 2: Feature_Generation
Feature_Generation <- function(data, momentum, reversal, adjusted_momentum,
                               acceleration, volatility, volume_volatility,extra_momentum,
                               extra_tick,max_lag,investment_horizon){
  # extra features
  if(ncol(data) == 4){
    ex_feat = c()
  } else {
    ex_feat = colnames(data)[5:ncol(data)]
  }
  
  
  # one-day past return & one-day forward return
  data$return = log(data$price / lag.xts(data$price,1))
  data$fwreturn = log(lag.xts(data$price,-1) / data$price)
  
  # create columns for all of the features to be added
  data[,paste('mom',1:length(momentum),sep='')] = NA
  data[,paste('rev',1:length(reversal),sep='')] = NA
  data[,paste('adjmom',1:length(adjusted_momentum),sep='')] = NA
  data[,paste('acc',1:length(acceleration),sep='')] = NA
  data[,paste('vol',1:length(volatility),sep='')] = NA
  data[,paste('volvol',1:length(volume_volatility),sep='')] = NA
  
  # create columns for all of the extra features to be added
  for (i in ex_feat){
    data[,paste(i,1:length(extra_momentum),sep='')] = NA
  }
  
  mom <- function(lag){
    x = data$price
    x / lag.xts(x,lag)
  }
  
  rev <- function(lag){
    x = data$price
    lag.xts(x,lag) / x
  }
  
  adjmom <- function(lag){
    x = data$price
    lag.xts(x,19) / lag.xts(x,lag)
  }
  
  acc <- function(lags){
    x = data$volume
    movavg(x,lags[1])/movavg(x,lags[2])
  }
  
  vol <- function(lag){
    x = data$return
    rollapply(x,width = lag, FUN = sd, na.pad = T, align = 'right')
  }
  
  volvol <- function(lag){
    x = data$volume
    rollapply(data$volume,width = 10, FUN = sd, na.pad = T, align = 'right')
  }
  
  extra_mom <- function(lag){
    x = data[,y]
    x / lag.xts(x,lag)
  }
  
  # get rid of carets in extra_tick
  for (i in 1:length(extra_tick)){extra_tick[i] = gsub("\\^","",extra_tick[i])}
  
  features = c('momentum', 'reversal', 'adjusted_momentum','acceleration','volatility', 'volume_volatility', extra_tick)
  non_extra = c('momentum', 'reversal', 'adjusted_momentum','acceleration','volatility', 'volume_volatility')
  features_abv = c('mom', 'rev', 'adjmom', 'acc', 'vol', 'volvol','extra_mom')
  
  for (j in 1:(length(features))){
    if ((features[j] %in% non_extra)){
      f = get(features[j]) 
      y = features_abv[j]
      fxn = get(y)
    } else{
      f = extra_momentum
      y = extra_tick[j-length(non_extra)]
      fxn = extra_mom
    }
    for (i in 1:length(f)){
      data[,paste(y,i,sep='')] = fxn(f[[i]])  
    }
  }
  
  # generate the dependent variable, "y"
  data$y = 1/mom(-investment_horizon) - 1
  
  
  # get rid of data before the max lag
  data = data[data$date > sort(unique(data$date))[max_lag + 1],]
  
  # get rid of data at the very end
  data = data[data$date < sort(unique(data$date),decreasing = T)[investment_horizon + 1],]
  
  # complete cases only
  data = data[complete.cases(data),]
  
  # add dollar volume constraint
  data$dollar_volume = data$volume*data$price
  data = data[data$dollar_volume > 5e5,]
  
  return(data)
}
# params for Feature_Generation
momentum = c(9,19,39,59,119)
reversal = c(19,39,59,119)
adjusted_momentum = c(59,119)
acceleration = list(c(10,20), c(10,40), c(10,60), c(20,40), c(20,60),c(40,60))
volatility = c(10,20,40,60,120)
volume_volatility = c(10,20,40,60,120)
###extra_momentum = c(9,19,39,59)
extra_momentum = c()
max_lag = 120
investment_horizon = 5


data.f = Feature_Generation(data,momentum,reversal,adjusted_momentum,acceleration,
                            volatility,volume_volatility,extra_momentum,extra_tickers,
                            max_lag,investment_horizon)

# Function 3: backtest
backtest <- function(data,tickers_RH, train_period, investment_horizon, Eta, pca_num, long_only){
  for (i in 1:length(extra_tickers)){extra_tickers[i] = gsub("\\^","",extra_tickers[i])}
  not_features = c("date" ,"ticker","price","volume","return",'y',extra_tickers,'fwreturn','dollar_volume')
  features = colnames(data)[!(colnames(data) %in% not_features)]
  
  days = sort(unique(data$date))
  num_round = length(days) - (train_period + investment_horizon) + 1
  
  ticker_weights = data.frame(matrix(0,nrow = length(days), ncol = length(tickers_RH)))
  colnames(ticker_weights) = tickers_RH
  rownames(ticker_weights) = as.Date(days)
  
  ticker_returns = data.frame(matrix(0,nrow = length(days), ncol = length(tickers_RH)))
  colnames(ticker_returns) = tickers_RH
  rownames(ticker_returns) = as.Date(days)
  data_RH = data[(data$ticker %in% tickers_RH),]
  for (i in 1:nrow(data_RH)){
    ticker_returns[as.character(data_RH$date[i]),data_RH$ticker[i]] = data_RH$fwreturn[i]
  }
  
  # set parameters for the XGBoost
  params <- list(booster = "gbtree", objective = "reg:linear", eta = Eta, gamma = 0,
                 max_depth = 6)
  
  # in the following loop we will:
  #   - set the train period (a to b)
  #   - set the investment period (b+1 to c)
  #   - perform PCA on the train data to distill k features into pca_num features
  #   - use 10-fold cross-validation to train the XGBoost model
  #   - set weights according to the deviation from the mean predicted return
  for (i in 1:num_round){
    a = i
    b = i + train_period - 1
    c = b + 1
    train = days[a:b]
    test = days[c]
    
    train_data = data[data$date %in% train,c(features,"y")]
    test_data = data[(data$date %in% test) & (data$ticker %in% tickers_RH),c(features,"y")]
    test_tickers = data[(data$date %in% test) & (data$ticker %in% tickers_RH),'ticker']
    PCA = prcomp(as.matrix(train_data[,features]))
    X_train = PCA$x[,1:pca_num]
    Y_train = as.matrix(train_data[,'y'])
    X_test = (as.matrix(test_data[,features])%*%PCA$rotation)[,1:pca_num]
    Y_test = as.matrix(test_data[,'y'])
    
    # group training data in a dense matrix and train the  XGBoost model
    xgb_train <- xgb.DMatrix(data = X_train, label = Y_train)
    xgbcv <- xgb.cv(params = params, data = xgb_train, nfold = 10, nrounds = 10,verbose=F)
    cv_nrounds = which.min(xgbcv$evaluation_log$test_rmse_mean)
    xgb_optb <- xgboost(params = params, data = xgb_train, nround = cv_nrounds,verbose=F)
    
    
    # group test data in a dense matrix and use the trained model to make out-of-sample predictions
    xgb_test <- xgb.DMatrix(data = X_test, label = Y_test)
    xgb_pred_out_of_sample <- predict(xgb_optb, X_test)
    
    # use out-of-sample predictions to generate portfolio weights
    # Weight Scheme #1--------------------------------------------  
    # weights = (xgb_pred_out_of_sample-mean(xgb_pred_out_of_sample))
    # weights[xgb_pred_out_of_sample < 0] = 0
    # weights[is.na(weights)] = 0
    # S = sum(abs(weights))
    # if (S == 0){S = 1}
    # weights = weights*2/(S*investment_horizon)
    
    # Weight Scheme #2--------------------------------------------  
    k = 0.8
    # if c is between 0 and 1, it is dilutive, if it is greater than 1, it magnifies
    weights = order(xgb_pred_out_of_sample,decreasing = F)^k
    weights[xgb_pred_out_of_sample < 0] = 0
    weights[is.na(weights)] = 0
    weights = weights/sum(weights)
    
    
    
    # long - only constraint
    if (long_only == T){
      weights[weights < 0] = 0
    }
    
    for (j in 1:length(weights)){
      ticker_weights[c:(c+investment_horizon-1),test_tickers[j]] = rep(weights[j],investment_horizon) + ticker_weights[c:(c+investment_horizon-1),test_tickers[j]]
    }
  }
  daily_returns = as.numeric(rowSums(ticker_returns*ticker_weights))
  cum_returns = cumprod(1+daily_returns)
  plot(as.Date(row.names(ticker_returns)),cum_returns, type = 'l')
  highpoint = cum_returns[maxdrawdown(cum_returns)$from]
  lowpoint = cum_returns[maxdrawdown(cum_returns)$to]
  return(output = list(mean = mean(daily_returns)*(252), 
                       sd = sd(daily_returns)*sqrt(252), 
                       sharpe = mean(daily_returns)/sd(daily_returns)*sqrt(252),
                       skewness = skewness(daily_returns),
                       kurtosis= kurtosis(daily_returns),
                       drawdown = lowpoint/highpoint - 1))
}

# params for the backtest
tickers_RH = c('ETH', 'ETC', 'LTC', 'DOGE', 'BCH', 'BTC') # only use tickers that are supported on Robinhood *** ADD BITCOIN SV (BSV-USD) when data issue is fixed
train_period = 25
investment_horizon = 5 # shoule match what you use in Feature_Generation
Eta = 0.4
pca_num = 10
long_only = T
d = data.f[data.f$date > '2018-06-01',]

backtest(d,tickers_RH, train_period, investment_horizon, Eta, pca_num, long_only)
