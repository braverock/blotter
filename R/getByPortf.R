#' get attributes from each portfolio in an account
#' 
#' Retrieves calculated attributes for each portfolio in the account
#' from the portfolio summary table.  Assembles into a portfolio-by-time table,
#' normalized to the Account currency 
#' 
#' 
#' Attribute: typically any of:
#'  'Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees',
#'  'Realized.PL', 'Unrealized.PL', or 'Trading.PL'
#' @param Account an Account object containing Portfolio summaries
#' @param Attribute column name to be assembled for each symbol
#' @param Dates dates to return the calculation over formatted as xts range
#' @return regular xts object of values by portfolio
#' @rdname getByPortf
.getByPortf <- function(Account, Attribute, Dates=NULL)
{ # @author Peter Carl
    
    zerofill <- function (x) 
    { # kind of like PerformanceAnalytics, but not quite
        for (column in 1:NCOL(x)) {
            x[,column] <- ifelse(is.na(x[,column]),0, x[,column])
        }
        return(x)
    }

    table = NULL 
    portfolios=names(Account$portfolios)
    for (portfolio in portfolios) {
        tmp_col= Account$portfolios[[portfolio]][Dates,Attribute,drop=FALSE]
        colnames(tmp_col)<-portfolio
        if(is.null(table)) table = tmp_col
        else table = cbind(table, tmp_col)
    }
    switch(Attribute,
        Long.Value =, 
        Short.Value =, 
        Net.Value =, 
        Gross.Value = {
            if(NROW(table) > 1) 
                table = na.locf(table)
        },
        Txn.Fees =,
        Realized.PL =, 
        Unrealized.PL =, 
		Net.Trading.PL =,		
        Gross.Trading.PL = {
            if(NROW(table) > 1)
                table = zerofill(table)
        }
    )
    return(table)
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
