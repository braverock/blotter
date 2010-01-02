`updateEndEq` <-
function(Account, Dates)
{
    if(is.null(Dates)) # if no date is specified, get all available dates
        Dates = time(Account[[1]])
    else
        Dates = time(Account[[1]][Dates])

    # For each date, calculate realized and unrealized P&L
    for(d in 1:length(Dates)){ # d is a date slot counter
	Account = calcEndEq(Account, as.character(Dates[d])) ## WTF?
    }
    return(Account)
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
