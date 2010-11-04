#' Gets the transactions and returns 
#' @param Portfolio a string identifying a portfolio object containing transactions
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Date timestamp as of which to get transactions
#' @return xts of transactions made in the Symbol during the time period given
#' @export
getTxns <- function(Portfolio, Symbol, Date)
{ # @author Peter Carl
    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio first"))
    
    TxnData = Portfolio$symbols[[Symbol]]$txn
    Txns = TxnData[Date,c('Txn.Qty', 'Txn.Price', 'Txn.Fees', 'Txn.Value', 'Txn.Avg.Cost', 'Net.Txn.Realized.PL')]
    return(Txns)
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
