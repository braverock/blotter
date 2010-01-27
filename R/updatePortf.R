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
#' @param Dates 
#' @export
updatePortf <- function(Portfolio, Dates)
{ #' @author Peter Carl
    pname<-Portfolio
    Portfolio<-getPortfolio(pname) # TODO add Date handling

    # FUNCTION
    symbols = names(Portfolio)
    for(symbol in symbols){
        tmp_instr<-try(getInstrument(symbol))
        if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
            message(paste("Instrument",symbol," not found, assuming non-synthetic"))
        } else {
            updatePosPL(pname, symbol, Dates, Cl(get(symbol)))            
        }  
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
