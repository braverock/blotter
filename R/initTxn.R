`initTxn` <-
function(initDate="1950-01-01", initPosQty=0)
{ # @author Peter Carl

    # DESCRIPTION
    # Constructs the data container used to store transactions and resulting positions.
    # The data series will be an irregular time series.

    # Inputs
    # initDate: date prior to the first close price given, used to contain
    #           initial account equity and initial position
    # initPosQty: initial position, default is zero

    # Outputs
    # Constructs multi-column xts object used to store transactions

    # FUNCTION
    ## @todo: Add 'Txn.Type' column
    ## @todo: DIVIDEND creates a realized gain
    txn <- xts( as.matrix(t(c(0,0,0,0,0,initPosQty,0,0))), order.by=as.Date(initDate) )
    colnames(txn) <- c('Txn.Qty', 'Txn.Price', 'Txn.Fees', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Realized.PL')
    return(txn)
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
