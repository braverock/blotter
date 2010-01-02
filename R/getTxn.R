`getTxn` <-
function(Portfolio, Symbol, Date)
{ # @author Peter Carl

    # DESCRIPTION:
    # Gets the value of that period's transactions and returns the sum 

    # Inputs
    # Portfolio: a portfolio object containing transactions
    # Symbol: an instrument identifier for a symbol included in the portfolio
    # Date: timestamp as of which to have the most recent position

    # Outputs
    # Table of transactions made in the Symbol during the time period given
    TxnData = Portfolio[[Symbol]]$txn
    Txns = TxnData[Date,c('Txn.Qty', 'Txn.Price', 'Txn.Fees', 'Txn.Value', 'Txn.Avg.Cost')]
    return(Txns)
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
