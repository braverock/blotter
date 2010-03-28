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
