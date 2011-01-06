#' Retrieve transactions and their attributes.
#'
#' Find and return the transactions and attribute values for the symbol and date in a specied portfolio.
#' 
#' This function provides easy access to the values of transactions contained in a Portfolio object. See \code{\link{initPortf}} for a detailed description of the structure of a Portfolio object.
#'
#'
#' @param Portfolio a string identifying a portfolio object containing transactions
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Dates specified as an ISO 8601 date or an xts date range, such as "2007-01::2008-04-15"
#' @return xts time series of transactions made in the Symbol during the time period given
#' @seealso \code{\link{initPortf}}
#' @export
getTxns <- function(Portfolio, Symbol, Dates)
{ # @author Peter Carl
    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'), envir=.blotter)
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio first"))
    
    TxnData = Portfolio$symbols[[Symbol]]$txn
    Txns = TxnData[Dates,c('Txn.Qty', 'Txn.Price', 'Txn.Fees', 'Txn.Value', 'Txn.Avg.Cost', 'Net.Txn.Realized.PL')]
    return(Txns)
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
