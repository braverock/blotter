#' Initializes a portfolio object, 
#' 
#' Initializes a portfolio object, which is constructed from is constructed of the following:
#' symbols: the identifier used for each instrument contained in the portfolio.
#'   Use names(Portfolio) to get a list of symbols.
#' $[symbol]$txn: irregular xts object of transactions data 
#' $[symbol]$posPL: regular xts object of positions P&L calculated from 
#'   transactions
#' 
#' TODO: add $account: name of the (one) affiliated account
#
#' Outputs
#' Initialized portfolio structure with a start date and initial positions.
#' 
#' @param name 
#' @param symbols  instrument identifiers of those instruments contained in the portfolio 
#' @param initPosQty initial position, default is zero
#' @param initDate date prior to the first close price given, used to contain initial account equity and initial position
#' @author Peter Carl
#' @export
initPortf <- function(name="default", symbols, initPosQty = 0, initDate = '1950-01-01', currency='USD')
{ # @author Peter Carl
    if(exists(paste("portfolio",name,sep='.'), envir=.blotter,inherits=TRUE))
        stop(paste("Portfolio",name,"already exists, use updatePortf() or addPortfInstr() to update it."))
    
    # FUNCTION
    portfolio=vector("list",length=length(symbols))
    names(portfolio)=symbols
    if(length(initPosQty)==1)
	initPosQty=rep(initPosQty, length(symbols))
    if(length(initPosQty)!=length(symbols))
	stop("The length of initPosQty is unequal to the number of symbols in the portfolio.")
    for(instrument in symbols){
    	i = match(instrument, symbols)
        portfolio[[instrument]]$txn = initTxn(initDate = initDate, initPosQty = initPosQty[i])
        portfolio[[instrument]]$posPL = initPosPL(initDate = initDate, initPosQty = initPosQty[i])
    }
    class(portfolio)<-c("blotter_portfolio", "portfolio")
    attr(portfolio,'currency')<-currency
    #return(portfolio)
    assign(paste("portfolio",as.character(name),sep='.'),portfolio,envir=.blotter)
    return(name) # not sure this is a good idea
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
