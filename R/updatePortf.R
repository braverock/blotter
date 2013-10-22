#' update Portfilio P&L over a Dates range
#'
#' The \code{updatePortf} function goes through each symbol and calculates the PL for each period prices are available.
#'
#' Note that the portfolio will be marked on every time stamp where prices are available.
#' As such, your \code{Dates} range must reflect timestamps which appear in the price stream.
#' Also note that you probably don't want to mark the portfolio on every tick,
#'
#'
#' @return assigns position information and PL into the environment
#'
#' @param Portfolio string identifying a portfolio
#' @param Symbols character vector identifying symbols to update the portfolio for, default NULL
#' @param Dates xts-style ISO-8601 time range to run updatePortf over, default NULL (will use times from Prices
#' @param Prices optional xts object containing prices and timestamps to mark the book on, default NULL
#' @param \dots any other passthrough parameters
#' @export
updatePortf <- function(Portfolio, Symbols=NULL, Dates=NULL, Prices=NULL, ...)
{ #' @author Peter Carl, Brian Peterson
     pname<-Portfolio
     Portfolio<-.getPortfolio(pname) # TODO add Date handling
     
     # FUNCTION
     if(is.null(Symbols)){
       Symbols = ls(Portfolio$symbols)
     }
     for(symbol in Symbols){
       tmp_instr<-try(getInstrument(symbol), silent=TRUE)
       .updatePosPL(Portfolio=pname, Symbol=as.character(symbol), Dates=Dates, Prices=Prices, ...=...)
     }
     
     # Calculate and store portfolio summary table
     Portfolio<-.getPortfolio(pname) # refresh with an updated object
     if(is.null(Dates)) Dates <- unique(do.call(c,c(lapply(Portfolio$symbols, function(x) index(x[["posPL"]])), use.names=FALSE, recursive=FALSE)))
     
     #Symbols = ls(Portfolio$symbols)
     Attributes = c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Period.Realized.PL', 'Period.Unrealized.PL', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
     summary = NULL
     tmp.attr=NULL
     for(attribute in Attributes) {
       result=NULL
       switch(attribute,
              Net.Value =,
              Gross.Value =,
              Long.Value =,
              Short.Value =,{
                # all these use Pos.Value
                if(is.null(tmp.attr)){
                  table = .getBySymbol(Portfolio = Portfolio, Attribute = "Pos.Value", Dates = Dates, Symbols = Symbols)
                  tmp.attr="Pos.Value"
                }
                switch(attribute,
                       Gross.Value = {  result = xts(rowSums(abs(table), na.rm=TRUE), order.by=index(table))},
                       Long.Value  = { tmat = table
                                       tmat[tmat < 0] <- 0
                                       result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
                       },
                       Short.Value = { tmat = table
                                       tmat[tmat > 0] <- 0
                                       result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
                       },
                       Net.Value   = {	result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))	}
                )
              },
              Period.Realized.PL =,
              Period.Unrealized.PL =,
              Gross.Trading.PL =,
              Txn.Fees =,
              Net.Trading.PL = {
                table = .getBySymbol(Portfolio = Portfolio, Attribute = attribute, Dates = Dates, Symbols = Symbols)
                tmp.attr = NULL
                result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
              }
       )
       
       colnames(result) = attribute
       if(is.null(summary)) {summary=result}
       else {summary=cbind(summary,result)}
     }
     
     # get rid of duplicated indices in the summary data,
     # thanks to Guy Yollin for the bug report and Josh Ulrich for the elegant approach to fixing it
     d <- duplicated(.index(summary)) | duplicated(.index(summary), fromLast=TRUE)
     if(any(d)){
       # extract duplicated rows; get last row for each duplicate
       summary.dups <- summary[d,]
       ds <- duplicated(.index(summary.dups)) & !duplicated(.index(summary.dups), fromLast=TRUE)
       # get the last value
       cLast <- c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value')
       lastCols <- summary.dups[which(ds),cLast]
       # sum values
       cSums <- c('Period.Realized.PL', 'Period.Unrealized.PL', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
       # take cumulative sum; keep last value for each duplicate
       sumCols <- cumsum(summary.dups[,cSums])[which(ds),cSums]
       # subtract previous value from current value, since we used cumsum
       sumColsLag <- lag(sumCols)
       sumColsLag[1,] <- 0
       sumCols <- sumCols - sumColsLag
       slist <- merge(sumCols,lastCols)      # combine aggregated objects
       slist <- slist[,colnames(summary)]    # order columns
       summary <- rbind(summary[!d,], slist) # put it all back together
     }
     
     # if(!is.timeBased(Dates)) Dates = xts:::time.xts(Portfolio$symbols[[1]][["posPL"]][Dates])
     #xts(,do.call(unlist,c(lapply(symbols,index),use.names=FALSE)))
     if(!is.timeBased(Dates)) Dates <- unique(do.call(c,c(lapply(Portfolio$symbols, function(x) index(x[["posPL"]][Dates]) ), use.names=FALSE, recursive=FALSE)))
     startDate = first(Dates)-.00001
     # trim summary slot to not double count, related to bug 831 on R-Forge, and rbind new summary
     if( as.POSIXct(attr(Portfolio,'initDate'))>=startDate || length(Portfolio$summary)==0 ){
       Portfolio$summary<-summary #changes to subset might not return a empty dimnames set of columns
     }else{
       Portfolio$summary<-rbind(Portfolio$summary[paste('::',startDate,sep='')],summary)
     }

     #portfolio is already an environment, it's been updated in place
     #assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
     
     return(pname) #not sure this is a good idea
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/)
# Copyright (c) 2008-2011 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
