#' Constructs the data container used to store transactions and resulting positions.
#' 
#' The data series stored here will be an irregular time series.
#' @param initDate date prior to the first close price given, used to contain initial account equity and initial position
#' @param initPosQty initial position, default is zero
#' @param \dots any other passthrough parameters  
#' @return Constructs multi-column xts object used to store transactions
#' @rdname initTxn
.initTxn <- function(initDate="1950-01-01", initPosQty=0, ...)
{ # @author Peter Carl
    ## TODO: Add 'Txn.Type' column
    ## TODO: DIVIDEND Txn.Type creates a realized gain
    txn <- xts( as.matrix(t(c(0,0,0,0,initPosQty,0,0,0,0,0))), order.by=as.POSIXct(initDate, ...=...), ...=... )
    colnames(txn) <- c('Txn.Qty', 'Txn.Price', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Gross.Txn.Realized.PL', 'Txn.Fees', 'Net.Txn.Realized.PL', 'Con.Mult')
    class(txn)<-c("transactions",class(txn))
    return(txn)
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
