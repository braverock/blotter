.onLoad <- function(lib, pkg) {
    if(!exists('.blotter'))  
        .blotter <<- new.env()
}


#' Constructs the data container used to store calculated account values such as aggregated P&L, equity, etc.
#' 
#' Inputs
#' portfolios: a list of portfolio object names to attach to the account
#' initDate: date prior to the first close price given, used to contain
#'           initial account equity and initial position 
#' initEq: initial equity or starting capitaal, default is 100,000
#' 
#' Outputs
#' Constructs multi-column xts object used to store aggregated portfolio 
#' calculations
#' 
#' NOTES
#' An Account object is a list of portfolios with portfolio summary information
#' 
#' The Account object is modeled on the CFTC Thirteen-column presentation table.
#' Start with the CFTC six column presentation, which includes:
#' Beg.Eq, Additions, Withdrawals, Net.Perf, End.Eq, Period.ROR
#' No reason to persist Period.ROR, and Beg.Eq = Previous End.Eq,
#' So we're left with four columns.  Note that Period.ROR can be calc'd 
#' several different ways and is best left as a function.
#' 
#' To get to the CFTC thirteen columns add:
#' Gross.Realized, Commission, Net.Realized, Int.Income, Ch.Unrealized,
#' Advisory.Fees, Wealth.Index 
#' Again, no need to add Wealth.Index. Eventually, these additional 
#' columns will be useful.  
#' Gross.Realized will be calculated as (Net) Realized.PL + Txn.Fees
#' 
#' @param name Account name, as string
#' @param portfolios character vector of strigs naming portfolios included in this account
#' @param initDate A date prior to the first close price given, used to contain initial account equity and initial position
#' @param currency ISO currency identifier used to locate the portfolio currency
#' @param initEq initial account equity in the currency of the portfolio, as a floating point number.
#' @param \dots any other passthrough parameters  
#' @export
#' @note
#' TODO Add calcPeriodROR function
#' 
#' TODO Adddd functions addCapital, drawCapital, addFees
#' 
#' initDate and initEq can be used in addCapital to initalize the account?
#' Track cash at this level???
#' Calc gross PL and subtract fees? Or calc net PL and add fees.
initAcct <- function(name='default', portfolios, initDate="1950-01-01", initEq=0, currency='USD', ...)
{ # @author Peter Carl

    if(exists(paste("account",name,sep='.'), envir=.blotter,inherits=TRUE)) 
        stop(paste("Account",name,"already exists, use updateAcct() or create a new account."))
    
    # FUNCTION
    account=list()
    account$portfolios=vector("list",length=length(portfolios))
    names(account$portfolios)=portfolios
    account$summary = xts( as.matrix(t(c(0,0,0,0,0,0,0,0,0,0,initEq))), order.by=as.POSIXct(initDate,...=...), ...=... )
    colnames(account$summary) = c('Additions', 'Withdrawals', 'Realized.PL', 'Unrealized.PL', 'Int.Income', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL', 'Advisory.Fees', 'Net.Performance', 'End.Eq')
    for(portfolio in portfolios){
        account$portfolios[[portfolio]] = .initSummary(initDate=initDate)
    }
    # return(account)
    attr(account,'currency')<-currency
	attr(account,'initEq')<-initEq
	class(account)<-c("portfolio_account","account")
	assign(paste("account",as.character(name),sep='.'),account,envir=.blotter)  
    return(name) # not sure this is a good idea
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
