`initPortf` <-
function(symbols, initPosQty = 0, initDate = '1950-01-01')
{ # @author Peter Carl

    # DESCRIPTION
    # Initializes a portfolio object, whichis constructed of the following:
    # symbols: the identifier used for each instrument contained in the portfolio.
    #   Use names(Portfolio) to get a list of symbols.
    # $[symbol]$txn: irregular xts object of transactions data 
    # $[symbol]$posPL: regular xts object of positions P&L calculated from 
    #   transactions
    ## @todo: add $account: name of the (one) affiliated account

    # Inputs
    # symbols: instrument identifiers of those instruments contained in the portfolio

    # Outputs
    # Initialized portfolio structure with a start date and initial positions.
    # initDate: date prior to the first close price given, used to contain
    #           initial account equity and initial position
    # initPosQty: initial position, default is zero

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
    return(portfolio)
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
