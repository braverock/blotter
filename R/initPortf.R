#' Initializes a portfolio object.
#' 
#' Constructs and initializes a portfolio object, which is used to contain transactions, positions, and aggregate level values.
#'
#' Initializes a portfolio object, which is constructed from the following:
#' $symbols: the identifier used for each instrument contained in the portfolio. Use \code{names(Portfolio$symbols)} to get a list of symbols.
#' $symbols$[symbol]$txn: irregular xts object of transactions data 
#' $symbols$[symbol]$posPL: regular xts object of positions P&L calculated from transactions
#' $symbols$[symbol]$posPL.ccy: regular xts object of positions P&L converted to portfolio currency
#' $summary: aggregated portfolio values
#'
#' Each symbol has three associated tables.  The first, txn, is the transactions table, an irregular time series that contains information about trades or other position adjustments with the following columns:
#' \itemize{
#' \item Txn.Qty: the quantity, usually in units of contracts, changing hands. Positive values indicate a "buy" transaction; negative values are used to indicate a "sell."
#' \item Txn.Price: the price at which the transaction was made,
#' \item Txn.Fees: the sum total of transaction fees associated with the trade,
#' \item Txn.Value: the notional value of the transaction,
#' \item Avg.Txn.Cost: a calculated value for the average net price paid (received) per contract bought (received),
#' \item Pos.Qty: the resulting position quantity of contracts, calculated as the sum of the current transaction and the prior position,
#' \item Pos.Avg.Cost: the calculated average cost of the resulting position, and
#' \item Realized.PL: any prot or loss realized in the transaction from closing out a prior position
#' }
#'
#' The second, posPL, is a container used to store calculated P&L values from transactions and close prices within an instrument. The data series is, however, a regular time series. Columns of the table include:
#' \itemize{
#' \item Pos.Qty the quantity of the position held in the symbol,
#' \item Pos.Value the notional value of the position,
#' \item Txn.Value the net value of the transactions occuring,
#' \item Txn.Fees the total fees associated with transactions,
#' \item Realized.PL any net prot or loss realized through transactions,
#' \item Unrealized.PL any prot or loss associated with the remaining or open position, and
#' \item Trading.PL the sum of net realized and unrealized prot and loss.
#' }
#' 
#' The third, posPL.ccy, is the same as the second but translated into the portfolio currency.
#'
#' For each portfolio, the summary slot contains a table that tracks calculated portfolio information through time. The table contains the following columns, held in a regular xts time series:
#' \itemize{
#' \item Long.Value: The sum of the notional value of all positions held long in the portfolio.
#' \item Short.Value: The sum of the notional value of all positions held short in the portfolio.
#' \item Net.Value: The sum of the notional long and notional short value of the portfolio.
#' \item Gross.Value: The sum of the notional long and absolute value of the notional short value of the portfolio.
#' \item Txn.Fees: The sum of brokerage commissions, exchange and other brokerage fees paid by the portfolio during the period.
#' \item Realized.PL: The sum of net realized prots or losses aggregated from the underlying positions in the portfolio. Gross realized prots can be calculated by adding Txn.Fees, the brokerage commission expenses for the period.
#' \item Unrealized.PL: The sum total increase or decrease in unrealized profits or losses on open positions in the portfolio at the end of the period.
#' \item Net.Trading.PL: Net realized prot or loss plus interest income plus change in unrealized prot or loss across all positions in the portfolio.
#' }
#' TODO: add $account: name of the (one) affiliated account
#
#' Outputs
#' Initialized portfolio structure with a start date and initial positions.
#' 
#' @param name A name for the resulting portfolio object
#' @param symbols  A list of instrument identifiers for those instruments contained in the portfolio 
#' @param initPosQty Initial position quantity, default is zero
#' @param initDate A date prior to the first close price given, used to contain initial account equity and initial position
#' @param currency ISO currency identifier used to locate the portfolio currency
#' @param \dots any other passthrough parameters  
#' @author Peter Carl
#' @export
initPortf <- function(name="default", symbols, initPosQty = 0, initDate = '1950-01-01', currency='USD', ...)
{ # @author Peter Carl
    if(exists(paste("portfolio",name,sep='.'), envir=.blotter,inherits=TRUE))
        stop(paste("Portfolio",name,"already exists, use updatePortf() or addPortfInstr() to update it."))
    
    # FUNCTION
    portfolio=list()
	portfolio$symbols=vector("list",length=length(symbols))
    names(portfolio$symbols)=symbols
    if(length(initPosQty)==1)
	initPosQty=rep(initPosQty, length(symbols))
    if(length(initPosQty)!=length(symbols))
	stop("The length of initPosQty is unequal to the number of symbols in the portfolio.")
    for(instrument in symbols){
    	i = match(instrument, symbols)
        portfolio$symbols[[instrument]]$txn = .initTxn(initDate = initDate, initPosQty = initPosQty[i],...=...)
        portfolio$symbols[[instrument]]$posPL = .initPosPL(initDate = initDate, initPosQty = initPosQty[i],...=...)
        portfolio$symbols[[instrument]][[paste('posPL',currency,sep='.')]] = portfolio$symbols[[instrument]]$posPL
    }
	portfolio$summary<-.initSummary(initDate=initDate)
    class(portfolio)<-c("blotter_portfolio", "portfolio")
    attr(portfolio,'currency')<-currency
	attr(portfolio,'initDate')<-initDate
    #return(portfolio)
    assign(paste("portfolio",as.character(name),sep='.'),portfolio,envir=.blotter)
    return(name) # not sure this is a good idea
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
