#' update ending equity for an account
#' 
#' @param Account 
#' @param Dates 
#' @author Peter Carl
#' @export
updateEndEq <- function(Account, Dates=NULL)
{
	# DESCRIPTION
	# Calculates End.Eq and Net.Performance
	
	# Requires that updateAcct has been run and any additional functions
	# have alread appended information into that table (e.g., additions or
	# withdrawals, fees, interest, etc.)
	
	aname<-Account
    Account<-try(get(paste("account",aname,sep='.'), envir=.blotter))
    if(inherits(Account,"try-error"))
        stop(paste("Account",aname," not found, use initAcct() to create a new account"))
    
    if(is.null(Dates)) # if no date is specified, get all available dates
        Dates = time(Account$summary)
    else
        Dates = time(Account$summary[Dates])

	### TODO Vectorize this
	#     # For each date, calculate realized and unrealized P&L
	#     for(d in 1:length(Dates)){ # d is a date slot counter
	# 	Account = calcEndEq(aname, as.character(Dates[d])) ## WTF?
	#     }

    PrevDate = time(Account$summary[first(Account$summary[Dates,which.i=TRUE])-1,]) # get index of previous end date 
    PrevEndEq = getEndEq(aname, PrevDate)
    Additions = Account$summary[Dates]$Additions
    Withdrawals = Account$summary[Dates]$Withdrawals
    NetPerformance = rowSums(Account$summary[Dates,c('Int.Income','Net.Trading.PL', 'Advisory.Fees')])
	# assign NetPerformance into the account slot
    Account$summary$Net.Performance[Dates] <- NetPerformance

    # Create a vector of end equity without the previous value?
    EndCapital = PrevEndEq + Additions + Withdrawals + NetPerformance
    Account$summary$End.Eq[Dates] <- EndCapital
	
    assign(paste("account",aname,sep='.'),Account, envir=.blotter) 
    return(aname) #not sure this is a good idea
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
