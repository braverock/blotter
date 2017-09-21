#' calculate trade statistics for round turn trades.
#'
#' One 'trade' is defined as a series of transactions which make up a 'round turn'.
#' It may contain many transactions.  This function reports statistics on these
#' round turn trades which may be used on their own or which are also used
#' by other functions, including \code{\link{tradeStats}} and \code{\link{tradeQuantiles}}
#'
#' @details
#' Additional methods of determining 'round turns' are also supported.
#'
#' \strong{Supported Methods for \code{tradeDef}:}
#' \describe{
#'   \item{\code{flat.to.flat}}{From the initial transaction that moves the
#'     position away from zero to the last transaction that flattens the position
#'     make up one round turn trade for the purposes of 'flat to flat' analysis.}
#'   \item{\code{flat.to.reduced}}{The \emph{flat.to.reduced} method starts the
#'     round turn trade at the same point as \emph{flat.to.flat}, at the first
#'     transaction which moves the position from zero to a new open position. The
#'     end of each round turn is described by transactions which move the position
#'     closer to zero, regardless of any other transactions which may have
#'     increased the position along the way.}
#'   \item{\code{increased.to.reduced}}{The \emph{increased.to.reduced} method
#'     is appropriate for analyzing round turns in a portfolio which is rarely
#'     flat, or which regularly adds to and reduces positions. Every transaction
#'     which moves the position closer to zero (reduced position) will close a
#'     round turn.  this closing transaction will be paired with one or more
#'     transaction which move the position further from zero to locate the
#'     initiating transactions. \code{acfifo} is an alias for this method.}
#' }
#'
#' As with the rest of \code{blotter}, \code{perTradeStats} uses average cost
#' accounting.  For the purposes of round turns, the average cost in force is
#' the average cost of the open position at the time of the closing transaction.
#'
#' Note that a trade that is open at the end of the measured period will
#' be marked to the timestamp of the end of the series.
#' If that trade is later closed, the stats for it will likely change.
#' This is 'mark to market' for the open position, and corresponds to
#' most trade accounting systems and risk systems in including the open
#' position in reporting.
#'
#' \code{Net.Trading.PL} and MAE/MFE are calculated somewhat differently
#' depending on the \code{tradeDef}.  All three \code{tradeDef} types utilize
#' \emph{Period.Realized.PL} to calculate the trading P&L of the round turn trade.
#' If the method is \emph{flat.to.flat}, the cumulative sum of all transaction
#' realized P&L's during the round turn is added.  If the method is
#' \emph{flat.to.reduced}, the realized P&L is the period realized P&L of the
#' closing transaction.  If the method is \emph{increased.to.reduced} (or the
#' equvalent \emph{acfifo}), the realized P&L for the round turn trade will be
#' the period realized P&L potentially pro-rated by the difference in size between
#' the initiating and closing transactions.  
#' 
#' MAE and MFE are pro-rated for \emph{increased.to.reduced} (or  \emph{acfifo})
#' and \emph{flat.to.reduced} using the proportion of the total traded quantity 
#' over \emph{timespan} that is attributable to this round turn.  For these 
#' definitions of round turns, this is complicated because there can be multiple
#' initating transactions which will adjust the average cost (and thus the net P&L)
#' of the position.  After pro-rating the cash measures, the percent and tick 
#' versions are constructed by dividing by the maximum notional position cost and 
#' the tick value, respectively.
#'
#' If \code{includeFlatPeriods=TRUE}, \code{perTradeStats} will include periods
#' when the series is flat (holds no position). Flat periods at the beginning of 
#' the series will be removed, as they are presumed to hold no information, and 
#' may be easily retrived if desired. This information is likely most useful in 
#' constructing stylized facts about the trading style, calculating values such
#' as time in market.  It is also extremely useful for Monte Carlo simulation of
#' random trading strategies with similar style to the series under investigation.
#' For more information on this latter use, see \code{\link{txnsim}}.
#'  
#' @param Portfolio string identifying the portfolio
#' @param Symbol string identifying the symbol to examin trades for. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param includeOpenTrade whether to process only finished trades, or the last trade if it is still open, default TRUE
#' @param tradeDef string, one of 'flat.to.flat', 'flat.to.reduced', 'increased.to.reduced' or 'acfifo'. See Details.
#' @param \dots any other passthrough parameters
#' @param includeFlatPeriods boolean, default FALSE, whether to include flat periods in output, mostly useful for Monte Carlo simulation as in \code{\link{txnsim}}
#' @author Brian G. Peterson, Jasen Mackie, Jan Humme
#' @references Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new approach to system development and portfolio optimisation} (ISBN 978-1-905641-79-6)
#' @return
#' A \code{data.frame} containing:
#'
#' \describe{
#'      \item{Start}{the \code{POSIXct} timestamp of the start of the trade}
#'      \item{End}{the \code{POSIXct} timestamp of the end of the trade, when flat}
#'      \item{Init.Qty}{ transaction quantity initiating the trade}
#'      \item{Init.Pos}{ position held after the initiating transaction of the round turn trade}
#'      \item{Max.Pos}{the maximum (largest) position held during the open trade}
#'      \item{End.Pos}{ the remaining quantity held after closing the trade}
#'      \item{Closing.Txn.Qty}{ the transaction quantity which closes the round turn trade }
#'      \item{Num.Txns}{ the number of transactions included in this trade}
#'      \item{Max.Notional.Cost}{ the largest notional investment cost of this trade}
#'      \item{Net.Trading.PL}{ net trading P&L in the currency of \code{Symbol}}
#'      \item{MAE}{ Maximum Adverse Excursion (MAE), in the currency of \code{Symbol}}
#'      \item{MFE}{ Maximum Favorable Excursion (MFE), in the currency of \code{Symbol}}
#'      \item{Pct.Net.Trading.PL}{ net trading P&L in percent of invested \code{Symbol} price gained or lost}
#'      \item{Pct.MAE}{ Maximum Adverse Excursion (MAE), in percent}
#'      \item{Pct.MFE}{ Maximum Favorable Excursion (MFE), in percent}
#'      \item{tick.Net.Trading.PL}{  net trading P&L in ticks}
#'      \item{tick.MAE}{ Maximum Adverse Excursion (MAE) in ticks}
#'      \item{tick.MFE}{ Maximum Favorable Excursion (MFE) in ticks}
#'      \item{duration}{ \code{difftime} describing the duration of the round turn, in seconds }
#' }
#'
#' @seealso \code{\link{chart.ME}} for a chart of MAE and MFE derived from this function,
#' and \code{\link{tradeStats}} for a summary view of the performance, and
#' \code{\link{tradeQuantiles}} for round turns classified by quantile.
#' @export
perTradeStats <- function(Portfolio
                          , Symbol
                          , includeOpenTrade=TRUE
                          , tradeDef="flat.to.flat"
                          , ...
                          , includeFlatPeriods=FALSE)
{

  portf <- .getPortfolio(Portfolio)
  if(missing(Symbol)) Symbol <- ls(portf$symbols)[[1]]

  posPL <- portf$symbols[[Symbol]]$posPL

  instr <- getInstrument(Symbol)
  tick_value <- instr$multiplier*instr$tick_size

  tradeDef <- match.arg(tradeDef, c("flat.to.flat","flat.to.reduced","increased.to.reduced","acfifo"))
  if(tradeDef=='acfifo') tradeDef<-'increased.to.reduced'

  trades <- list()
  switch(tradeDef,
         flat.to.flat = {
           # identify start and end for each trade, where end means flat position
           trades$Start <- which(posPL$Pos.Qty!=0 & lag(posPL$Pos.Qty)==0)
           trades$End <- which(posPL$Pos.Qty==0 & lag(posPL$Pos.Qty)!=0)
         },
         flat.to.reduced = {
           # find all transactions that bring position closer to zero ('trade ends')
           decrPos <- diff(abs(posPL$Pos.Qty)) < 0
           # find all transactions that open a position ('trade starts')
           initPos <- posPL$Pos.Qty!=0 & lag(posPL$Pos.Qty)==0
           # 'trades' start when we open a position, so determine which starts correspond to each end
           # add small amount to Start index, so starts will always occur before ends in StartEnd
           Start <- xts(initPos[initPos,which.i=TRUE],index(initPos[initPos])+1e-5)
           End   <- xts(decrPos[decrPos,which.i=TRUE],index(decrPos[decrPos]))
           StartEnd <- merge(Start,End)
           StartEnd$Start <- na.locf(StartEnd$Start)
           if(includeOpenTrade){
             SEtail <- StartEnd[paste0(index(last(StartEnd[!is.na(StartEnd$End)])+1),'/')]
             SEtail <- SEtail[-1,]
           }
           StartEnd <- StartEnd[!is.na(StartEnd$End),]
           if(includeOpenTrade) StartEnd <- rbind(StartEnd,SEtail)
           # populate trades list
           trades$Start <- drop(coredata(StartEnd[!is.na(StartEnd$Start[]),]$Start))
           trades$End <- drop(coredata(StartEnd[!is.na(StartEnd$End),]$End))
         },
         increased.to.reduced = {
           # find all transactions that bring position closer to zero ('trade ends')
           decrPos <- diff(abs(posPL$Pos.Qty)) < 0
           decrPosCount <- ifelse(diff(abs(posPL$Pos.Qty)) < 0,-1,0)
           decrPosCount <- ifelse(decrPosCount[-1] == 0, 0, cumsum(decrPosCount[-1]))
           decrPosQty <- ifelse(diff(abs(posPL$Pos.Qty)) < 0, diff(abs(posPL$Pos.Qty)),0)
           decrPosQtyCum <- ifelse(decrPosQty[-1] == 0, 0, cumsum(decrPosQty[-1])) #subset for the leading NA
           # find all transactions that take position further from zero ('trade starts')
           incrPos <- diff(abs(posPL$Pos.Qty)) > 0
           incrPosCount <- ifelse(diff(abs(posPL$Pos.Qty)) > 0,1,0)
           incrPosCount <- ifelse(incrPosCount[-1] == 0, 0, cumsum(incrPosCount[-1]))
           incrPosQty <- ifelse(diff(abs(posPL$Pos.Qty)) > 0, diff(abs(posPL$Pos.Qty)),0)
           incrPosQtyCum <- ifelse(incrPosQty[-1] == 0, 0, cumsum(incrPosQty[-1])) #subset for the leading NA

           # Calculate txn qty
           txnqty <- rbind(incrPosQtyCum, abs(decrPosQtyCum))
           txnqty <- txnqty[-which(txnqty == 0)]
           txnqty <- rbind(xts(0,as.POSIXct("1950-01-01")),txnqty)
           txnqty <- as.data.frame(txnqty)
           txnqty <- txnqty[order(txnqty[,1]),]
           txnqty <- diff(txnqty)
           txnqty <- na.omit(txnqty)
           
           # Get start and end dates
           starts <- incrPosQtyCum[-which(incrPosQtyCum==0)]
           ends <- abs(decrPosQtyCum[-which(decrPosQtyCum==0)])
           cumsum(txnqty) # let's investigate cumsum(txnqty)
           end_idx <- findInterval(cumsum(txnqty), ends, left.open = TRUE) + 1 # can disregard last element as it relates to open trade
           end_idx
           start_idx <- findInterval(cumsum(txnqty), starts, left.open = TRUE) + 1 # can disregard last element as it relates to open trade
           start_idx
           
           testdf <- data.frame(cbind(txnqty, cumsum(txnqty), start_idx, end_idx))
           testdf$start_ts <- index(starts)[start_idx]
           testdf$end_ts <- index(ends)[end_idx]
           testdf <- testdf[-which(testdf$txnqty == 0),]
           
           # build trades$Start and trades$End in trades list
           # iterating over testdf, for all txns that have an end date
           # and are therefore round turn trades
           for(i in 1:length(which(!is.na(testdf$end_ts)))){
             trades$Start[i] <- which(index(incrPosQtyCum) == testdf$start_ts[i])
             trades$End[i] <- which(index(decrPosQtyCum) == testdf$end_ts[i])
           }
           # now add 1 to idx for missing initdate from incr/decrPosQtyCum - adds consistency with flat.to.reduced and flat.to.flat
           trades$Start <- trades$Start + 1
           trades$End <- trades$End + 1

           # add extra 'trade start' if there's an open trade, so 'includeOpenTrade' logic will work
           if(any(is.na(testdf$end_ts))){
             trades$Start <- c(trades$Start,last(which(index(incrPos) == testdf$start_ts[first(which(is.na(testdf$end_ts)))])))
           }
         }
  ) # end round turn trade separation by tradeDef

  # if the last trade is still open, adjust depending on whether we want open trades or not
  if(last(posPL)[,"Pos.Qty"] != 0)
  {
    if(includeOpenTrade)
      trades$End <- c(trades$End,nrow(posPL))
    else
      trades$Start <- head(trades$Start, -1)
  }
  if(length(trades$Start)!=length(trades$End)){
    trades$Start[(length(trades$Start)+1):length(trades$End)] <- last(trades$Start)
  }
  
  # check for an open trade that starts on the last observation, remove
  last.trade.is.open <- FALSE
  if(last(trades$End)==last(trades$Start)){
    last.trade.is.open <- TRUE
    trades$End <- trades$End[-length(trades$End)]
    trades$Start <- trades$Start[-length(trades$Start)]
  }
  
  # pre-allocate trades list
  N <- length(trades$End)
  trades <- c(trades, list(
    Init.Qty = numeric(N),
    Init.Pos = numeric(N),
    Max.Pos = numeric(N),
    End.Pos = numeric(N),
    Closing.Txn.Qty = numeric(N),
    Num.Txns = integer(N),
    Max.Notional.Cost = numeric(N),
    Net.Trading.PL = numeric(N),
    MAE = numeric(N),
    MFE = numeric(N),
    Pct.Net.Trading.PL = numeric(N),
    Pct.MAE = numeric(N),
    Pct.MFE = numeric(N),
    tick.Net.Trading.PL = numeric(N),
    tick.MAE = numeric(N),
    tick.MFE = numeric(N)))

  # create txn.qty vector for computing Init.Qty and End.Pos
  txn.qty <- diff(posPL$Pos.Qty)

  # calculate information about each round turn 'trade'
  for(i in 1:N)
  {
    timespan <- seq.int(trades$Start[i], trades$End[i])
    trade <- posPL[timespan]
    n <- nrow(trade)
    # calculate cost basis, PosPL, Pct.PL, tick.PL columns
    Pos.Qty <- trade[,"Pos.Qty"]   # avoid repeated subsetting

    # position sizes
    Max.Pos.Qty.loc <- which.max(abs(Pos.Qty))     # find max position quantity location
    trades$Init.Pos[i] <- Pos.Qty[1]
    trades$Max.Pos[i] <- Pos.Qty[Max.Pos.Qty.loc]

    # initiating and ending quantities
    trades$End.Pos[i] <- Pos.Qty[n]
    #trades$Init.Qty[i] <- txn.qty[timespan][1]
    if(tradeDef == "flat.to.flat" || tradeDef == "flat.to.reduced"){
    trades$Init.Qty[i] <- txn.qty[timespan][1]
    trades$Closing.Txn.Qty[i] <- trades$End.Pos[i] - Pos.Qty[n-1]
    if(trades$Closing.Txn.Qty[i] == 0) trades$Closing.Txn.Qty[i] <- Pos.Qty[n] * -1
    } else if(tradeDef == "increased.to.reduced"){
      trades$Init.Qty[i] <- testdf$txnqty[i] * sign(txn.qty[timespan][1])
      trades$Closing.Txn.Qty[i] <- trades$Init.Qty[i] * -1
    }

    Pos.Cost.Basis <- cumsum(trade[,"Txn.Value"])

    switch(tradeDef,
           flat.to.flat = {
             prorata  <- 1
             ts.prop  <- 1
             trade.PL <- sum(trade[,"Net.Trading.PL"])
             Cum.PL   <- cumsum(trade[,"Net.Trading.PL"])
           },
           flat.to.reduced = {
             #prorata  <- abs(trades$Closing.Txn.Qty[i] / trades$Max.Pos[i]) #not precisely correct?
             gettxns <- getTxns(Portfolio, Symbol) # used in computing trade.cost
             if(index(trade[nrow(trade),]) %in% index(gettxns)){
               closeqty <- coredata(gettxns$Txn.Qty[index(trade[nrow(trade),])]) # total qty traded at closure of round-turn/s
             }
             tradecost <- coredata(gettxns$Txn.Price[index(trade[1,])]) # used in computing trade.PL
             if(abs(trades$Closing.Txn.Qty[i] / closeqty) >= 1) { # closing qty less than init.pos, incl full realized.pl
               prorata <- 1
             } else {
               prorata <- as.numeric((abs(trades$Closing.Txn.Qty[i] / closeqty)))
             }
             ts.prop  <- abs(trades$Closing.Txn.Qty[i] / Pos.Qty) 
             if(i==N && includeOpenTrade){ 
               ts.prop[n] <- 1 # all unrealized PL for last observation is counted 
             } else {
                ts.prop[n] <- 0 # no unrealized PL for last observation is counted
             }
             if(i==N && includeOpenTrade && trade[n,"Period.Realized.PL"] !=0 && last.trade.is.open == FALSE){
               trade.PL <- 0
             } else {
               trade.PL <- trade[n,"Period.Realized.PL"]
             }
             fees     <- sum(trade[,'Txn.Fees']) * prorata
             trade.PL <- trade.PL + fees 
             #Cum.PL   <- cumsum(trade[n,'Period.Realized.PL'])*prorata + cumsum(trade[,'Period.Unrealized.PL']*ts.prop) + trade[,'Txn.Fees']
             #Cum.PL   <- cumsum(trade[,'Period.Realized.PL'] + (trade[,'Period.Unrealized.PL']*ts.prop)) + trade[,'Txn.Fees']
             Cum.PL   <- merge(trade[n,'Period.Realized.PL']*prorata, cumsum(trade[,'Period.Unrealized.PL'])*ts.prop, trade[,'Txn.Fees'])
             Cum.PL[is.na(Cum.PL)] <- 0
             Cum.PL <- rowSums(Cum.PL)
             #colnames(Cum.PL) <- 'Cum.PL'
           },
           increased.to.reduced = {
             tradeqty <- as.numeric((coredata(trade[n,'Pos.Qty']) - coredata(trade[n-1,'Pos.Qty']))) # used in computing trade.PL
             gettxns <- getTxns(Portfolio, Symbol) # used in computing trade.cost
             if(index(trade[nrow(trade),]) %in% index(gettxns)){
               closeqty <- coredata(gettxns$Txn.Qty[index(trade[nrow(trade),])]) # total qty traded at closure of round-turn/s
               if(length(closeqty)>1) closeqty<-sum(closeqty) #multiple closing trades share the timestamp, so combine for pro-rata calc
             }
             tradecost <- coredata(gettxns$Txn.Price[index(trade[1,])]) # used in computing trade.PL
             if(abs(trades$Closing.Txn.Qty[i] / closeqty) >= 1) { # closing qty less than init.pos, incl full realized.pl
               prorata <- 1
             } else {
               prorata <- as.numeric((abs(trades$Closing.Txn.Qty[i] / closeqty)))
             }
             # calculate trade size as proportion of total position size (ts.prop)
             ts.prop  <- abs(trades$Closing.Txn.Qty[i] / Pos.Qty) # slightly different implementation compared with flat.to.reduced for trade size proportion
             colnames(ts.prop) <- 'ts.prop'
             if(i==N && includeOpenTrade){ 
               ts.prop[n] <- 1 # all unrealized PL for last observation is counted 
             } else {
                ts.prop[n] <- 0 # no unrealized PL for last observation is counted
             }
             if(i==N && includeOpenTrade && trade[n,"Period.Realized.PL"] !=0 && last.trade.is.open == FALSE){
               trade.PL <- 0
             } else {
               trade.PL <- trade[n,"Period.Realized.PL"]*prorata
             }
             ts.prop[is.infinite(ts.prop)] <- 0 # once a position is closed out to flat, dividing by 0 gives an infinite number so we zero it out as there should be no

             fees     <- as.numeric(trade[1,'Txn.Fees'] * prorata) + as.numeric(trade[n,'Txn.Fees'])
             trade.PL <- trade.PL + fees 
             # remove fees not part of this round turn
             # increased.to.reduced has precisely one opening and closing trade
             trade$Txn.Fees[2:(n-1)] <- 0 
             # scale opening trade fees to correct proportion
             trade$Txn.Fees[1] <- trade[1,'Txn.Fees'] * prorata 
             # for cumulative P&L for increased.to.reduced/acfifo, we have precise
             # numbers for Period.Realized.PL and Txn.Fees, but need to take prorata
             # for unrealized P&L
             #Cum.PL   <- cumsum(trade[n,'Period.Realized.PL'])*prorata + cumsum(trade[,'Period.Unrealized.PL']*ts.prop) + trade[,'Txn.Fees']
             Cum.PL   <- merge(trade[n,'Period.Realized.PL']*prorata, cumsum(trade[,'Period.Unrealized.PL'])*ts.prop, trade[,'Txn.Fees'])
             Cum.PL[is.na(Cum.PL)] <- 0
             Cum.PL <- rowSums(Cum.PL)
             #colnames(Cum.PL) <- 'Cum.PL'
           }
    )

    # scale cost basis based on how much of the Txn.Value should be used for this round turn
    Pos.Cost.Basis <- Pos.Cost.Basis * prorata
    
    # count number of transactions
    trades$Num.Txns[i] <- sum(trade[,"Txn.Value"]!=0)

    # investment
    trades$Max.Notional.Cost[i] <- Pos.Cost.Basis[Max.Pos.Qty.loc]

    # cash P&L
    trades$Net.Trading.PL[i] <- trade.PL
    
    #include unrealized P&L for open position, if necessary
    if(i==N && trades$Net.Trading.PL[i]==0 && includeOpenTrade){ 
      #trades$Net.Trading.PL[i] <- sum(trade[,'Period.Unrealized.PL'])
      trades$Net.Trading.PL[i] <- sum(posPL$Net.Trading.PL) - sum(posPL$Period.Realized.PL)
      #trades$Net.Trading.PL[i] <- sum(posPL$Net.Trading.PL) - sum(trades$Net.Trading.PL) # balancing final inclOpenTrade round turn PL
    }
    
    # cash MAE/MFE
    trades$MAE[i] <- min(0,Cum.PL) 
    trades$MFE[i] <- max(0,Cum.PL) 

    # percentage P&L
    Pct.PL <- Cum.PL/abs(trades$Max.Notional.Cost[i])

    #if(nrow(Pct.PL)>1){trades$Pct.Net.Trading.PL[i] <- Pct.PL[n]}
    #if(nrow(Pct.PL)==1){trades$Pct.Net.Trading.PL[i] <- Pct.PL}
    if(length(Pct.PL)>1){trades$Pct.Net.Trading.PL[i] <- Pct.PL[n]}
    if(length(Pct.PL)==1){trades$Pct.Net.Trading.PL[i] <- Pct.PL}
    trades$Pct.MAE[i] <- min(0,trades$MAE[i]/abs(trades$Max.Notional.Cost[i]))
    trades$Pct.MFE[i] <- max(0,trades$MFE[i]/abs(trades$Max.Notional.Cost[i]))

    # tick P&L
    # Net.Trading.PL/position/tick value = ticks
    Tick.PL <- Cum.PL/abs(trades$Max.Pos[i])/tick_value

    # if(nrow(Tick.PL)>1){trades$tick.Net.Trading.PL[i] <- Tick.PL[n]}
    # if(nrow(Tick.PL)==1){trades$tick.Net.Trading.PL[i] <- Tick.PL}
    if(length(Tick.PL)>1){trades$tick.Net.Trading.PL[i] <- Tick.PL[n]}
    if(length(Tick.PL)==1){trades$tick.Net.Trading.PL[i] <- Tick.PL}
    trades$tick.MAE[i] <- min(0,trades$MAE[i]/tick_value)
    trades$tick.MFE[i] <- max(0,trades$MFE[i]/tick_value)
  }
  trades$Start <- index(posPL)[trades$Start]
  trades$End   <- index(posPL)[trades$End]

  #make into data.frame
  trades<- as.data.frame(trades)
  
  if(includeFlatPeriods){
    # use a list to put things together
    flat.p<-list()
    flat.p$Start <- which(posPL$Pos.Qty==0 & lag(posPL$Pos.Qty)!=0)
    flat.p$End <- which(posPL$Pos.Qty!=0 & lag(posPL$Pos.Qty)==0)
    
    # check for initial flat period, remove as non-informational
    if(first(flat.p$End)<first(flat.p$Start)){
      flat.p$End <- flat.p$End[-1]
    }
    
    # check for a flat period that starts on the last observation, remove
    if(last(flat.p$End)==last(flat.p$Start)){
      flat.p$End <- flat.p$End[-length(flat.p$End)]
      flat.p$Start <- flat.p$Start[-length(flat.p$Start)]
    }
    
    # check for trailing flat period, keep this if it exists
    if(last(flat.p$End) < last(flat.p$Start) && 
       length(flat.p$Start)>length(flat.p$End)){
      # add an artificial end at the end of the series
      flat.p$End <- c(flat.p$End,length(index(posPL)))
    }
    
    # allocate flat periods list
    N <- length(flat.p$End)
    flat.p <- c(flat.p, list(
      Init.Qty = rep(0,N),
      Init.Pos = rep(0,N),
      Max.Pos = rep(0,N),
      End.Pos = rep(0,N),
      Closing.Txn.Qty = rep(0,N),
      Num.Txns = rep(0,N),
      Max.Notional.Cost = rep(0,N),
      Net.Trading.PL = rep(0,N),
      MAE = rep(0,N),
      MFE = rep(0,N),
      Pct.Net.Trading.PL = rep(0,N),
      Pct.MAE = rep(0,N),
      Pct.MFE = rep(0,N),
      tick.Net.Trading.PL = rep(0,N),
      tick.MAE = rep(0,N),
      tick.MFE = rep(0,N)))
    
    flat.p$Start <- index(posPL)[flat.p$Start]
    flat.p$End   <- index(posPL)[flat.p$End]
    
    flat.p <- as.data.frame(flat.p)
    
    #combine with the trades data.frame
    trades <- rbind(trades,flat.p)
  }
  
  #add duration
  trades$duration <- difftime(trades$End, trades$Start, units='secs') #for POSIXct compliance
  
  #add periodicity
  attr(trades, 'trade.periodicity') <- periodicity(posPL)
  
  return(trades)
} # end fn perTradeStats


#' quantiles of per-trade stats
#'
#' The quantiles of your trade statistics get to the heart of quantitatively
#' setting rational stops and possibly even profit taking targets
#' for a trading strategy or system.
#' When applied to theoretical trades from a backtest, they may help to adjust
#' parameters prior to trying the strategy with real money.
#' When applied to real historical trades, they should help in examining what
#' is working and where there is room for improvement in a trading system
#' or strategy.
#'
#' This function will use the \code{\link{quantile}} function to calculate
#' quantiles of per-trade net P&L, MAE, and MFE using the output from
#' \code{\link{perTradeStats}}.  These quantiles are chosen by the \code{probs}
#' parameter and will be calculated for one or all of
#' 'cash','percent',or 'tick', controlled by the \code{scale} argument.
#' Quantiles will be calculated separately for trades that end positive (gains)
#' and trades that end negative (losses), and will be denoted
#' 'pos' and 'neg',respectively.
#'
#' Additionally, this function will return the MAE with respect to
#' the maximum cumulative P&L achieved for each \code{scale} you request.
#' Tomasini&Jaekle recommend plotting MAE or MFE with respect to cumulative P&L
#' and choosing a stop or profit target in the 'stable region'.  The reported
#' max should help the user to locate the stable region, perhaps mechanically.
#' There is room for improvement here, but this should give the user
#' information to work with in addition to the raw quantiles.
#' For example, it may make more sense to use the max of a loess or
#' kernel or other non-linear fit as the target point.
#'
#' @param Portfolio string identifying the portfolio
#' @param Symbol string identifying the symbol to examin trades for. If missing, the first symbol found in the \code{Portfolio} portfolio will be used
#' @param \dots any other passthrough parameters
#' @param scale string specifying 'cash', or 'percent' for percentage of investment, or 'tick'
#' @param probs vector of probabilities for \code{quantile}
#' @author Brian G. Peterson
#' @references Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new approach to system development and portfolio optimisation} (ISBN 978-1-905641-79-6)
#' @seealso \code{\link{tradeStats}}
#' @export
tradeQuantiles <- function(Portfolio, Symbol, ..., scale=c('cash','percent','tick'),probs=c(.5,.75,.9,.95,.99,1))
{
    trades <- perTradeStats(Portfolio, Symbol, ...)

    #order them by increasing MAE and decreasing P&L (to resolve ties)
    trades <- trades[with(trades, order(-Pct.MAE, -Pct.Net.Trading.PL)), ]
    #we could argue that we need three separate sorts, but we'll come back to that if we need to

    trades$Cum.Pct.PL <- cumsum(trades$Pct.Net.Trading.PL) #NOTE: this is adding simple returns, so not perfect, but gets the job done
    trades$Cum.PL <- cumsum(trades$Net.Trading.PL)
    trades$Cum.tick.PL <- cumsum(trades$tick.Net.Trading.PL)
    # example plot
    # plot(-trades$Pct.MAE,trades$Cum.Pct.PL,type='l')
    #TODO: put this into a chart. fn

    post <- trades[trades$Net.Trading.PL>0,]
    negt <- trades[trades$Net.Trading.PL<0,]

    ret<-NULL
    for (sc in scale){
        switch(sc,
                cash = {
                    posq <- quantile(post$Net.Trading.PL,probs=probs)
                    names(posq)<-paste('posPL',names(posq))

                    negq <- -1*quantile(abs(negt$Net.Trading.PL),probs=probs)
                    names(negq)<-paste('negPL',names(negq))

                    posMFEq <-quantile(post$MFE,probs=probs)
                    names(posMFEq) <- paste('posMFE',names(posMFEq))
                    posMAEq <--1*quantile(abs(post$MAE),probs=probs)
                    names(posMAEq) <- paste('posMAE',names(posMAEq))

                    negMFEq <-quantile(negt$MFE,probs=probs)
                    names(negMFEq) <- paste('negMFE',names(negMFEq))
                    negMAEq <--1*quantile(abs(negt$MAE),probs=probs)
                    names(negMAEq) <- paste('negMAE',names(negMAEq))

                    MAEmax <- trades[which(trades$Cum.PL==max(trades$Cum.PL)),]$MAE
                    names(MAEmax)<-'MAE~max(cumPL)'

                    ret<-c(ret,posq,negq,posMFEq,posMAEq,negMFEq,negMAEq,MAEmax)
                },
                percent = {
                    posq <- quantile(post$Pct.Net.Trading.PL,probs=probs)
                    names(posq)<-paste('posPctPL',names(posq))

                    negq <- -1*quantile(abs(negt$Pct.Net.Trading.PL),probs=probs)
                    names(negq)<-paste('negPctPL',names(negq))

                    posMFEq <-quantile(post$Pct.MFE,probs=probs)
                    names(posMFEq) <- paste('posPctMFE',names(posMFEq))
                    posMAEq <--1*quantile(abs(post$Pct.MAE),probs=probs)
                    names(posMAEq) <- paste('posPctMAE',names(posMAEq))

                    negMFEq <-quantile(negt$Pct.MFE,probs=probs)
                    names(negMFEq) <- paste('negPctMFE',names(negMFEq))
                    negMAEq <--1*quantile(abs(negt$Pct.MAE),probs=probs)
                    names(negMAEq) <- paste('negPctMAE',names(negMAEq))

                    MAEmax <- trades[which(trades$Cum.Pct.PL==max(trades$Cum.Pct.PL)),]$Pct.MAE
                    names(MAEmax)<-'%MAE~max(cum%PL)'

                    ret<-c(ret,posq,negq,posMFEq,posMAEq,negMFEq,negMAEq,MAEmax)
                },
                tick = {
                    posq <- quantile(post$tick.Net.Trading.PL,probs=probs)
                    names(posq)<-paste('posTickPL',names(posq))

                    negq <- -1*quantile(abs(negt$tick.Net.Trading.PL),probs=probs)
                    names(negq)<-paste('negTickPL',names(negq))

                    posMFEq <-quantile(post$tick.MFE,probs=probs)
                    names(posMFEq) <- paste('posTickMFE',names(posMFEq))
                    posMAEq <--1*quantile(abs(post$tick.MAE),probs=probs)
                    names(posMAEq) <- paste('posTickMAE',names(posMAEq))

                    negMFEq <-quantile(negt$tick.MFE,probs=probs)
                    names(negMFEq) <- paste('negTickMFE',names(negMFEq))
                    negMAEq <--1*quantile(abs(negt$tick.MAE),probs=probs)
                    names(negMAEq) <- paste('negTickMAE',names(negMAEq))

                    MAEmax <- trades[which(trades$Cum.tick.PL==max(trades$Cum.tick.PL)),]$tick.MAE
                    names(MAEmax)<-'tick.MAE~max(cum.tick.PL)'

                    ret<-c(ret,posq,negq,posMFEq,posMAEq,negMFEq,negMAEq,MAEmax)
                }
        ) #end scale switch
    } #end for loop

    #return a single column for now, could be multiple column if we looped on Symbols
    ret<-t(t(ret))
    colnames(ret)<-paste(Portfolio,Symbol,sep='.')
    ret
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/)
# Copyright (c) 2008-2015 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
