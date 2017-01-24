#perTradeStats demo for increased.to.reduced

# Test Datasets
############################################### Scenario 1
Date <- seq.int(1,10,1)
TxnQty <- c(100,50,50,-100,-50,50,-50,-50,+50,-50)
TxnPrice <- c(101,102,103,104,105,106,107,108,109,110) # just using silly prices
Cum <- c(100,150,200,100,50,100,50,0,50,0)
IncSeq <- c(1,2,3,0,0,4,0,0,5,0)
IncCum <- c(100,150,200,0,0,250,0,0,300,0)
DecSeq <- c(0,0,0,-1,-2,0,-3,-4,0,-5)
DecCum <- c(0,0,0,-100,-150,0,-200,-250,0,-300)
Start <- c(0,0,0,1,2,0,3,4,0,5)

df_test1 <- data.frame(cbind(Date,TxnQty,TxnPrice,Cum,IncSeq,IncCum,DecSeq,DecCum,Start))
df_test_Inc <- df_test1[-which(df_test1[,4] == 0),]
df_test_Dec <- df_test1[-which(df_test1[,6] == 0),]

findInterval(abs(df_test1$DecCum[-which(df_test1$DecCum==0)]),df_test1$IncCum[-which(df_test1$IncCum==0)])+1

rm.strat("testport")
stock.str='IBM' # what are we trying it on
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

startDate='2006-12-31'
initEq=1000000

#portfolio.st='testport'
initPortf('testport', symbols=stock.str)

getSymbols(stock.str,from=startDate,index.class=c('POSIXt','POSIXct'))
dfxts1 <- xts(df_test1[-1],index(IBM[1:10,]))

txns <- addTxns('testport',stock.str,dfxts1)
updatePortf('testport')
out <- perTradeStats('testport',stock.str,tradeDef = "increased.to.reduced")

# Checksum End dates
ifelse(sum(out$End - index(dfxts1[which(dfxts1$DecSeq != 0)])) != 0, print("CHECK"), print("OK"))
# Checksum Start dates
idxStart <- drop(coredata(dfxts1$Start[which(dfxts1$DecSeq != 0)]))
new <- dfxts1[which(dfxts1$IncSeq != 0)][idxStart]
ifelse(sum(out$Start - index(new)) != 0, print("CHECK"), print("OK"))

############################################### Scenario 2
Date <- seq.int(1,10,1)
TxnQty <- c(100,50,50,-160,-20,20,30,200,-35,-200)
TxnPrice <- c(101,102,103,104,105,106,107,108,109,110) # just using silly prices
Cum <- c(100,150,200,40,20,40,70,270,235,35)
IncSeq <- c(1,2,3,0,0,4,5,6,0,0)
IncCum <- c(100,150,200,0,0,220,250,450,0,0)
DecSeq <- c(0,0,0,-1,-2,0,0,0,-3,-4)
DecCum <- c(0,0,0,-160,-180,0,0,0,-215,-415)
Start <- c(0,0,0,1,3,0,0,0,3,4)

df_test <- data.frame(cbind(Date,TxnQty,TxnPrice,Cum,IncSeq,IncCum,DecSeq,DecCum,Start))
df_test_Inc <- df_test[-which(df_test[,4] == 0),]
df_test_Dec <- df_test[-which(df_test[,6] == 0),]

findInterval(abs(df_test$DecCum[-which(df_test$DecCum==0)]),df_test$IncCum[-which(df_test$IncCum==0)])+1

# Scenario 2 with addTxns()
rm.strat("testport")
stock.str='IBM' # what are we trying it on
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

startDate='2006-12-31'
initEq=1000000

#portfolio.st='testport'
initPortf('testport', symbols=stock.str)

getSymbols(stock.str,from=startDate,index.class=c('POSIXt','POSIXct'))
dfxts2 <- xts(df_test[-1],index(IBM[1:10,]))

txns <- addTxns('testport',stock.str,dfxts2)
updatePortf('testport')
out <- perTradeStats('testport',stock.str,includeOpenTrade = FALSE,tradeDef = "increased.to.reduced")

# Checksum End dates
ifelse(sum(out$End - index(dfxts2[which(dfxts2$DecSeq != 0)])) != 0, print("CHECK"), print("OK"))
# Checksum Start dates
idxStart <- drop(coredata(dfxts2$Start[which(dfxts2$DecSeq != 0)]))
new <- dfxts2[which(dfxts2$IncSeq != 0)][idxStart]
ifelse(sum(out$Start - index(new)) != 0, print("CHECK"), print("OK"))

############################################### Scenario 3
Date <- seq.int(1,10,1)
TxnQty <- c(100,50,50,-25,-30,-30,25,-45,-40,-55)
TxnPrice <- c(101,102,103,104,105,106,107,108,109,110) # just using silly prices
Cum <- c(100,150,200,0,0,0,225,0,0,0)
IncSeq <- c(1,2,3,0,0,0,4,0,0,0)
IncCum <- c(100,150,200,0,0,0,225,0,0,0)
DecSeq <- c(0,0,0,-1,-2,-3,0,-4,-5,-6)
DecCum <- c(0,0,0,-25,-55,-85,0,-130,-170,-225)
Start <- c(0,0,0,1,1,1,0,1,2,3)

df_test_3 <- data.frame(cbind(Date,TxnQty,TxnPrice,Cum,IncSeq,IncCum,DecSeq,DecCum,Start))
df_test_Inc <- df_test_3[-which(df_test_3[,4] == 0),]
df_test_Dec <- df_test_3[-which(df_test_3[,6] == 0),]

findInterval(abs(df_test_3$DecCum[-which(df_test_3$DecCum==0)]),df_test_3$IncCum[-which(df_test_3$IncCum==0)])+1

# Scenario 3 with addTxns()
rm.strat("testport")
stock.str='IBM' # what are we trying it on
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

startDate='2006-12-31'
initEq=1000000

#portfolio.st='testport'
initPortf('testport', symbols=stock.str)

getSymbols(stock.str,from=startDate,index.class=c('POSIXt','POSIXct'))
dfxts3 <- xts(df_test_3[-1],index(IBM[1:10,]))

txns <- addTxns('testport',stock.str,dfxts3)
updatePortf('testport')
out <- perTradeStats('testport',stock.str,tradeDef = "increased.to.reduced")

# Checksum End dates
ifelse(sum(out$End - index(dfxts3[which(dfxts3$DecSeq != 0)])) != 0, print("CHECK"), print("OK"))
# Checksum Start dates
idxStart <- drop(coredata(dfxts3$Start[which(dfxts3$DecSeq != 0)]))
new <- dfxts3[which(dfxts3$IncSeq != 0)][idxStart]
ifelse(sum(out$Start - index(new)) != 0, print("CHECK"), print("OK"))
