`calcEndEq` <-
function(Account, Date)
{ 
    # DESCRIPTION
    # Calculates End.Eq and Net.Performance

    # Requires that updateAcct has been run and any additional functions
    # have alread appended information into that table (e.g., additions or
    # withdrawals, fees, interest, etc.)
    Dates = time(Account[[1]])
    PrevDate = Dates[grep(Date, Dates)-1]
    PrevEndEq = getEndEq(Account, PrevDate)
    Additions = as.numeric(Account[[1]][Date]$Additions)
    Withdrawals = as.numeric(Account[[1]][Date]$Withdrawals)
    IntIncome = as.numeric(Account[[1]][Date]$Int.Income)
    TradingPL = as.numeric(Account[[1]][Date]$Trading.PL)
    AdvisoryFees = as.numeric(Account[[1]][Date]$Advisory.Fees)
    NetPerformance = IntIncome + TradingPL + AdvisoryFees
    Account[[1]][Date,'Net.Performance'] = as.numeric(NetPerformance) 
    Account[[1]][Date,'End.Eq'] = as.numeric(PrevEndEq + Additions + Withdrawals + NetPerformance)
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
