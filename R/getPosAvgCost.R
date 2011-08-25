
#' Retrieves the most recent average cost of the position
#' 
#' @param Portfolio a portfolio object containing transactions
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Date timestamp as of which to have the most recent position
#' @return Numeric value of the average cost of the current position
#' @rdname getPosAvgCost
.getPosAvgCost <- function(Portfolio, Symbol, Date)
{ # @author Peter Carl
    pname<-Portfolio
    # FUNCTION
    PosAvgCost = as.numeric(getPos(pname, Symbol, Date, Columns='Pos.Avg.Cost'))
    return(PosAvgCost)
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
