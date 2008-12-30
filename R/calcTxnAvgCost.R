`calcTxnAvgCost` <-
function(TxnValue, TxnQty)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates a per share or per contract cost of the transaction 
    # to match the units the price is quoted in

    # Inputs
    # TxnValue: total value of the transaction, including fees
    # TxnQty: total units (shares) of the transaction
    # Note that the multiplier is missing for other types of instruments

    # Outputs
    # TxnAvgCost: unit normalized (per share) cost implied by the transaction

    TxnAvgCost = TxnValue/TxnQty
    return(TxnAvgCost)
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
