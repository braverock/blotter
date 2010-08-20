#' Function goes through each symbol and calculates the PL for each day prices are available
#' 
#' Inputs
#' Portfolio: a portfolio object containing transactions
#' Symbol: an instrument identifier for a symbol included in the portfolio
#' Dates: Dates for which to calculate equity account
#' These dates must appear in the price stream
#' 
#' Outputs
#' assigns position information and PL into the environment
#' 
#' @param Portfolio 
#' @param Symbols
#' @param Dates 
#' @param Prices
#' @export
updatePortf <- function(Portfolio, Symbols=NULL, Dates=NULL, Prices=NULL)
{ #' @author Peter Carl
    pname<-Portfolio
    Portfolio<-getPortfolio(pname) # TODO add Date handling

    # FUNCTION
    if(is.null(Symbols)){
        Symbols = names(Portfolio)
    } 
    for(symbol in Symbols){
        tmp_instr<-try(getInstrument(symbol))
        updatePosPL(Portfolio=pname, Symbol=as.character(symbol), Dates=Dates, Prices=Prices)            
    }
    # Calculate and store portfolio summary table
    Portfolio<-getPortfolio(pname) # refresh with an updated object
    Symbols = names(Portfolio)
    Attributes = c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Realized.PL', 'Unrealized.PL', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
    summary = matrix()
    for(attribute in Attributes) {
        table = .getBySymbol(Portfolio = Portfolio, Attribute = attribute, Dates = Dates, Symbols = Symbols)
        switch(attribute,
            Gross.Value = {
                result = xts(rowSums(abs(table), na.rm=TRUE), order.by=index(table))
            },
            Long.Value = {
                tmat = apply(table,MARGIN=c(1,2),FUN=max,0)# comes out a matrix
                result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
            },
            Short.Value = {
                tmat = apply(table,MARGIN=c(1,2),FUN=min,0) # comes out a matrix
                result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
            },
            { result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
            }
        )
        colnames(result) = attribute
        summary = merge(summary, result)
    }
    # TODO: Now store summary in the correct slot in the Portfolio object
    return(pname) #not sure this is a good idea
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
