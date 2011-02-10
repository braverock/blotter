#' gets position at Date
#' @param Portfolio a string identifying a portfolio object containing transactions
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Date timestamp as of which to have the most recent position
#' @return Numeric value of the most recent position.
#' @export 
getPosQty <- function(Portfolio, Symbol, Date)
{ # @author Peter Carl
    pname<-Portfolio
    # portfolio retrieval happens in getPos
    #Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
    #if(inherits(Portfolio,"try-error"))
    #    stop(paste("Portfolio",name," not found, use initPortf() to create a new account"))
    
    # DESCRIPTION:
    # Gets the previous position 

    # Inputs
    # Portfolio: 
    # Symbol: 
    # Date: 

    # Outputs
    # 

    # FUNCTION
    PosQty = as.numeric(getPos(pname, Symbol, Date, Columns='Pos.Qty'))
    return(PosQty)
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
