`calcPosAvgCost` <-
function(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates the average cost of a resulting position from a transaction

    # Inputs
    # PrevPosQty: quantity of the previous position
    # PrevPosAvgCost: average position cost of the previous position
    # TxnValue: total value of the transaction, including fees
    # PosQty: total units (shares) of the resulting position
    # Note that the multiplier is missing for other types of instruments

    # Outputs
    # PosAvgCost: average cost of the resulting position

    if(PosQty == 0)
        PosAvgCost = 0
    else {
        PosAvgCost = (PrevPosQty*PrevPosAvgCost+TxnValue)/PosQty
    }
    return(PosAvgCost)
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
