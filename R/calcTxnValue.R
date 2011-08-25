#' Calculates the total value of a transaction or trade
#' @param TxnQty total units (shares) of the transaction
#' @param TxnPrice price at which the transaction was done
#' @param TxnFees fees associated with the transaction, e.g. commissions
#' @param ConMult multiplier from instrument data
#' @return TxnValue: total dollar value of the transaction, including fees
#' @rdname calcTxnValue
.calcTxnValue <- function(TxnQty, TxnPrice, TxnFees, ConMult=1)
{ # @author Peter Carl
    TxnValue = TxnQty * TxnPrice * ConMult - TxnFees
    return(TxnValue)
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
