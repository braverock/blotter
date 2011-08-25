#' Calculates a per share or per contract cost of the transaction to match the units the price is quoted in
#' @param TxnValue total value of the transaction, including fees
#' @param TxnQty total units (shares) of the transaction
#' @param ConMult multiplier from instrument data
#' @return TxnAvgCost: unit normalized (per share) cost implied by the transaction
#' @rdname calcTxnAvgCost
.calcTxnAvgCost <- function(TxnValue, TxnQty, ConMult=1)
{ # @author Peter Carl
    TxnAvgCost = TxnValue/(TxnQty*ConMult)
    return(TxnAvgCost)
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
