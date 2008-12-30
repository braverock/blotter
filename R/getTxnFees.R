`getTxnFees` <-
function(Portfolio, Symbol, Date)
{
    TxnData = Portfolio[[Symbol]]$txn
    TxnFees = sum(TxnData[Date,'Txn.Fees'])
    return(TxnFees)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
