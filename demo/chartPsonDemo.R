# - Demo for modified chart.Posn function
# Author: Rahul Singh


## This demostartes usage of (altered) chart.Posn 
## with multicolumn Price data for all symbols in a single file 
## A new paramaerter Prices is added to function chart.Posn 
## It is similar to the behaviour already supported by updatePortf

require(quantmod)
require(blotter) # need to install this version , or 
# source("R/chart.Posn.R")


# try prices parameter of updateportfolio

scrips= c("AAPL","IBM","MSFT")
ls()

#fetch price data and store in a single xts object

scripPrices<- lapply (scrips, getSymbols, auto.assign = F , src = "yahoo", from="2015-01-01"    ) 
scripPrices<- lapply( scripPrices, "[", "2015")
scripPrices<- Reduce(merge, scripPrices)
scripPrices[1:3,]
Ad(scripPrices)[1:3,]

# a randomly generated tradebook
# set.seed(2048)
# tr_dates<-sort(sample(index(scripPrices), 6, replace = F))
# tr_scrips<- sample(scrips, 6, replace = T);
# tr_type <- sample(c(1,-1), 6, replace = T)

# another approach , buy and sell all on same two dates . Easy comparison
tr_dates<-sort(sample(index(scripPrices), 2, replace = F))
tr_dates<- rep(tr_dates,1, each=3)
tr_scrips<- scrips[c(1:3,1:3)]
tr_type<- rep(c(1,-1),1, each=3)


data.frame(tr_dates,tr_scrips,tr_type)


# Blotter


rm(list =ls(envir=.blotter), envir=.blotter) 

Sys.setenv(TZ="UTC")

initDate <-min(tr_dates) -5
initEq <- 1000
currency("USD") 
stock(scrips, currency = "USD", multiplier = 1)

pricetester <- "pricetester"
initPortf(name = pricetester, symbols = scrips, initDate = initDate)
initAcct(name = pricetester, portfolios = pricetester, initDate = initDate, initEq = initEq)


#add transactions
for (i in seq_along(tr_dates) ) {
  tr_dt= tr_dates[i]; tr_scr= tr_scrips[i]; 
  tr_pr= as.numeric(getPrice(scripPrices, tr_scr)[tr_dt] )
  
  addTxn(pricetester, Symbol = tr_scr, TxnDate = tr_dt, TxnPrice = tr_pr, 
         TxnQty = tr_type[i], TxnFees = 0) 
}


# update portfolio
dts<- paste(initDate,max(tr_dates)+10, sep = "/")
updatePortf(pricetester, Dates = dts, Symbols = scrips, Prices = scripPrices ) #updatePort already supports Prices
updateAcct(pricetester, Dates =  dts)
updateEndEq(pricetester, Dates =  dts)

# Portfoilio stats
tradeStats(pricetester)
getAccount(pricetester)$summary[tr_dates,]
getPortfolio(pricetester)$summary[tr_dates,]
getPortfolio(pricetester)$summary$Net.Trading.PL



# show transactions for each symbol
lapply( scrips, FUN = getTxns, Portfolio = pricetester)

##########################################
# demonstrating chart.Posn with Prices
##########################################

chart.Posn(pricetester ,Symbol = scrips[1], Dates = dts, Prices = scripPrices)
chart.Posn(pricetester ,Symbol = scrips[2], Dates = dts, Prices = scripPrices)
chart.Posn(pricetester ,Symbol = scrips[3], Dates = dts, Prices = scripPrices)

# No Symbol given, automatically picking the first symbol
chart.Posn(pricetester , Dates = dts, Prices = scripPrices)
ls(getPortfolio(pricetester)$symbols) # which one is first?

# if scripPrices represents only one symbol
chart.Posn(pricetester , Dates = dts, Prices = scripPrices[,1:6])

# if scripPrices contains only close prices
chart.Posn(pricetester , Dates = dts, Prices = Cl(scripPrices) ) 
#getPrice(Cl(scripPrices) )

# regular usage: a separate xts for one symbol
getSymbols("IBM", src="yahoo", auto.assign = T, from="2015-01-01", to="2016-01-01" )
head(IBM,3)
chart.Posn(pricetester ,Symbol = "IBM", Dates = dts)
#which should be  same as
chart.Posn(pricetester ,Symbol = "IBM", Dates = dts, Prices = scripPrices)


