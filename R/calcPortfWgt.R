#' Calculates the portfolio weights for positions within a given portfolio.
#' 
#' Portfolio weights may be calculated differently depending on their use.
#' By default, this function uses denominator of 'Gross.Value', the second most common 
#' option will likely be 'Net.Value'. 
#' For separating long and short weights, 'Long.Value' and 'Short.Value' may be 
#' needed as denominators.
#' 
#' @return xts timeseries object with weights by date in rows and symbolname in columns
#' @param Portfolio a portfolio object structured with initPortf()
#' @param Symbols an instrument identifier for a symbol included in the portfolio
#' @param Dates dates to return the calculation over formatted as xts range
#' @param denominator string describing the deniminator, see Description
#' @param Account an Account object containing Portfolio summaries
#' @export
calcPortfWgt <- function(Portfolio, 
                         Symbols = NULL, 
                         Dates = NULL, 
                         denominator = c('Gross.Value', 'Net.Value', 'Long.Value', 'Short.Value'), 
                         Account)
{ # @author Peter Carl, Brian Peterson

    zerofill <- function (x) 
    { # kind of like PerformanceAnalytics, but not quite
        for (column in 1:NCOL(x)) {
            x[,column] <- ifelse(is.na(x[,column]),0, x[,column])
        }
        return(x)
    }
    
    pname<-Portfolio
    Portfolio<-getPortfolio(pname) # TODO add Date handling
    
    if(is.null(Symbols)) Symbols<-names(Portfolio$symbols)
    
    pos.value = .getBySymbol(Portfolio = Portfolio, Dates = Dates, Attribute = "Pos.Value", Symbols = Symbols)    
    portf.value = .getByPortf(Account=getAccount(Account),Attribute = denominator[1], Dates = Dates)
    weights = zerofill(as.data.frame(lapply(pos.value, FUN = function(x,y){return(x/y)}, y=portf.value))) 

    return(weights)
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
