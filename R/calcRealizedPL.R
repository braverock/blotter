`calcRealizedPL` <-
function(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty)
{ # @author Peter Carl

    # DESCRIPTION
    # Calculates any realized gain or loss resulting from a transaction

    # Inputs
    # TxnQty: total units (shares) of the transaction
    # TxnAvgCost: unit normalized (per share) cost implied by the transaction
    # PrevPosAvgCost: average position cost of the previous position
    # PosQty: total units (shares) of the resulting position
    # PrevPosQty: quantity of the previous position

    # Outputs
    # RealizedPL: value of profit or loss realized in a transaction

    # if the previous position is zero, RealizedPL = 0
    # if previous position is positive and position is larger, RealizedPL =0
    # if previous position is negative and position is smaller, RealizedPL =0
    if(abs(PrevPosQty) < abs(PosQty) | (PrevPosQty = 0))
        RealizedPL = 0

    # if prev position is negative and position is larger, OR
    # if prev position is positive and position is smaller,
    # then calc RealizedPL
    else
        RealizedPL = TxnQty * (PrevPosAvgCost - TxnAvgCost)

    return(RealizedPL)
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
