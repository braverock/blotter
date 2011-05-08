#' update ending equity for an account
#' 
#' Calculates End.Eq and Net.Performance
#' 
#' Requires that updateAcct has been run and any additional functions
#' have alread appended information into that table (e.g., additions or
#' withdrawals, fees, interest, etc.)
#' 
#' @param Account string identifying account
#' @param Dates Dates from which to calculate equity account
#' @author Peter Carl, Brian G. Peterson
#' @export
updateEndEq <- function(Account, Dates=NULL)
{
	aname<-Account
    Account<-try(get(paste("account",aname,sep='.'), envir=.blotter))
    if(inherits(Account,"try-error"))
        stop(paste("Account",aname," not found, use initAcct() to create a new account"))
    
    if(is.null(Dates)) # if no date is specified, get all available dates
        Dates = time(Account$summary)[-1]
    else
        Dates = time(Account$summary[Dates])

    PrevDate = time(Account$summary[first(Account$summary[Dates,which.i=TRUE])-1,]) # get index of previous end date 
    PrevEndEq = getEndEq(aname, PrevDate)
    Additions = Account$summary[Dates]$Additions
    Withdrawals = Account$summary[Dates]$Withdrawals
    NetPerformance = rowSums(Account$summary[Dates,c('Int.Income','Net.Trading.PL', 'Advisory.Fees')])
	# assign NetPerformance into the account slot 
    Account$summary$Net.Performance[Dates] <- NetPerformance

    # Create a vector of end equity
    EndCapital = PrevEndEq + cumsum(Additions + Withdrawals + NetPerformance) 
    Account$summary$End.Eq[Dates] <- EndCapital
	
    assign(paste("account",aname,sep='.'),Account, envir=.blotter) 
    return(aname) #not sure this is a good idea
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
