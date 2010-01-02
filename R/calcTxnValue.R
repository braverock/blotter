`calcTxnValue` <-
function(TxnQty, TxnPrice, TxnFees)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates the total value of a transaction or trade

    # Inputs
    # TxnQty: total units (shares) of the transaction
    # TxnPrice: price at which the transaction was done
    # TxnFees: fees associated with the transaction, e.g. commissions
    # Note that the multiplier is missing for other types of instruments

    # Outputs
    # TxnValue: total dollar value of the transaction, including fees

    TxnValue = TxnQty * TxnPrice - TxnFees
    return(TxnValue)
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
