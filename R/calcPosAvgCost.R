#' Calculates the average cost of a resulting position from a transaction
#' 
#' @return PosAvgCost: average cost of the resulting position
#' @param PrevPosQty quantity of the previous position
#' @param PrevPosAvgCost average position cost of the previous position
#' @param TxnValue total value of the transaction, including fees
#' @param PosQty total units (shares) of the resulting position
#' @param ConMult multiplier from instrument data
calcPosAvgCost <- function(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty, ConMult=1)
{ # @author Peter Carl
    if(PosQty == 0)
        PosAvgCost = 0
    else {
        PosAvgCost = abs((PrevPosQty * PrevPosAvgCost * ConMult + TxnValue)/(PosQty*ConMult))
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
