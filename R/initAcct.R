`initAcct` <-
function(portfolios, initDate="1950-01-01", initEq=100000)
{ # @author Peter Carl

    # DESCRIPTION
    # Constructs the data container used to store calculated account values
    # such as aggregated P&L, equity, etc.

    # Inputs
    # portfolios: a list of portfolio object names to attach to the account
    # initDate: date prior to the first close price given, used to contain
    #           initial account equity and initial position 
    # initEq: initial equity or starting capitaal, default is 100,000

    # Outputs
    # Constructs multi-column xts object used to store aggregated portfolio 
    # calculations

    # NOTES
    # An Account object is a list of portfolios with portfolio summary information

    # The Account object is modeled on the CFTC Thirteen-column presentation table.
    # Start with the CFTC six column presentation, which includes:
    # Beg.Eq, Additions, Withdrawals, Net.Perf, End.Eq, Period.ROR
    # No reason to persist Period.ROR, and Beg.Eq = Previous End.Eq,
    # So we're left with four columns.  Note that Period.ROR can be calc'd 
    # several different ways and is best left as a function.

    # To get to the CFTC thirteen columns add:
    # Gross.Realized, Commission, Net.Realized, Int.Income, Ch.Unrealized,
    # Advisory.Fees, Wealth.Index 
    # Again, no need to add Wealth.Index. Eventually, these additional 
    # columns will be useful.  
    # Gross.Realized will be calculated as (Net) Realized.PL + Txn.Fees

    ## TODOs:
    ## Add calcPeriodROR function
    ## add functions addCapital, drawCapital, addFees
    ## initDate and initEq can be used in addCapital to initalize the account?

    ## Track cash at this level???
    ## Calc gross PL and subtract fees? Or calc net PL and add fees.
    ## use getPortfSummary to populate portfolio tables?

    # FUNCTION
    account=vector("list",length=(length(portfolios)+1))
    names(account)=c("TOTAL",portfolios)
    account[["TOTAL"]] = xts( as.matrix(t(c(0,0,0,0,0,0,0,0,0,initEq))), order.by=as.POSIXct(initDate) )
    colnames(account[["TOTAL"]]) = c('Additions', 'Withdrawals', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', 'Int.Income', 'Trading.PL', 'Advisory.Fees', 'Net.Performance', 'End.Eq')
    for(portfolio in portfolios){
        account[[portfolio]] = xts( as.matrix(t(c(0,0,0,0,0,0,0,0))), order.by=as.POSIXct(initDate) )
        colnames(account[[portfolio]]) = c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees','Realized.PL', 'Unrealized.PL', 'Trading.PL')
    }
    return(account)
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
