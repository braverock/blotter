#' initializes position P&L for a portfolio instrument
#' @param initDate date prior to the first close price given, used to contain initial account equity and initial position
#' @param \dots any other passthrough parameters  
#' @param initPosQty initial position, default is zero
#' @param initConMult initial contract multiplier, default is one(1)
#' @param initCcyMult initial currency multiplier, default is one(1)
initPosPL <- function(initDate="1950-01-01", ..., initPosQty=0, initConMult=1, initCcyMult=1) #TODO add other init values to function as well for cost basis
{ # @author Peter Carl

    # DESCRIPTION
    # Constructs the data container used to store calculated P&L values from 
    # transactions and close prices. The data series will be a regular time series.

    # Inputs
    # initDate: 
    #           
    # initPosQty: 

    # Outputs
    # Constructs multi-column xts object used to store derived position information

    # FUNCTION
    posPL <- xts( as.matrix(t(c(initPosQty,initConMult,initCcyMult,0,0,0,0,0,0))), order.by=as.POSIXct(initDate) )
    colnames(posPL) <- c('Pos.Qty', 'Con.Mult', 'Ccy.Mult', 'Pos.Value', 'Txn.Value', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL','Trading.PL')
    class(posPL)<- c("posPL",class(posPL))
    return(posPL)
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
