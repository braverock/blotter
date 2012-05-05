#' Calculates the portfolio weights for positions within a given portfolio.
#' 
#' Portfolio weights may be calculated differently depending on their use.
#' 
#' @return xts timeseries object with weights by date in rows and symbolname in columns
#' @param Portfolio a portfolio object structured with initPortf()
#' @param Symbols an instrument identifier for a symbol included in the portfolio
#' @param Dates dates to return the calculation over formatted as xts range
#' @param denominator string describing the deniminator, see usage
#' @param Account an Account object containing Portfolio summaries
calcPortfWgt <- function(Portfolio, Symbols = NULL, Dates = NULL, 
        denominator = c('Gross.Value', 'Net.Value', 'Long.Value', 'Short.Value'), 
        Account = NULL)
{ # @author Peter Carl

    # FUNCTION

#    pos.value = .getBySymbol(Portfolio = Portfolio, Dates = Dates, Attribute = "Pos.Value", Symbols = Symbols)    
#    portf.value = .calcPortfAttr(Portfolio = Portfolio, Date = Dates, Attribute = denominator[1])
#    weights = apply(pos.value, MARGIN = 2, FUN = function(x,y){return(x/y)}, y=portf.value) 

#    return(weights)
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
