#include <R.h>
#include <Rinternals.h>

SEXP calcPosAvgCost (SEXP PrevPosQty, SEXP PrevPosAvgCost, SEXP TxnValue, SEXP PosQty, SEXP ConMult)
{   
    /* "vectorized" version of .calcPosAvgCost for use in addTxns
     * Author: Joshua Ulrich
     */
    int i, P=0;
    if(length(TxnValue) != length(PosQty))
        error("TxnValue and PosQty must be the same length");

    double d_PrevPosQty = asReal(PrevPosQty);
    double d_PrevPosAvgCost = asReal(PrevPosAvgCost);
    PROTECT(TxnValue = coerceVector(TxnValue, REALSXP)); P++;
    double *d_TxnValue = REAL(TxnValue);
    PROTECT(PosQty = coerceVector(PosQty, REALSXP)); P++;
    double *d_PosQty = REAL(PosQty);
    double d_ConMult = asReal(ConMult);

    int n = length(PosQty);

    SEXP PosAvgCost;
    PROTECT(PosAvgCost = allocVector(REALSXP, n)); P++;
    double *d_PosAvgCost = REAL(PosAvgCost);

    for(i = 0; i < n; i++) {
        if(d_PosQty[i] == 0.0) {
            d_PosAvgCost[i] = 0.0;
        } else
        if(abs(d_PrevPosQty) > abs(d_PosQty[i])){
            /* position is decreasing, pos avg cost for the open position remains the same */
            d_PosAvgCost[i] = d_PrevPosAvgCost;
        } else {
            d_PosAvgCost[i] = (d_PrevPosQty * d_PrevPosAvgCost * d_ConMult + d_TxnValue[i])/(d_PosQty[i]*d_ConMult);
        }
        d_PrevPosQty = d_PosQty[i];
        d_PrevPosAvgCost = d_PosAvgCost[i];
    }
    UNPROTECT(P);
    return(PosAvgCost);
}

/*############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2011 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: calcPosAvgCost.c 1051 2012-06-15 15:39:44Z braverock $
#
#############################################################################*/
