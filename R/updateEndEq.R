#' update ending equity for an account
#' 
#' @param Account 
#' @param Dates 
#' @author Peter Carl
#' @export
updateEndEq <- function(Account, Dates=NULL)
{
    aname<-Account
    Account<-try(get(paste("account",aname,sep='.'), envir=.blotter))
    if(inherits(Account,"try-error"))
        stop(paste("Account",aname," not found, use initAcct() to create a new account"))
    
    if(is.null(Dates)) # if no date is specified, get all available dates
        Dates = time(Account[[1]])
    else
        Dates = time(Account[[1]][Dates])

    # For each date, calculate realized and unrealized P&L
    for(d in 1:length(Dates)){ # d is a date slot counter
	Account = calcEndEq(aname, as.character(Dates[d])) ## WTF?
    }
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
