`initPosPL` <-
function(initDate="1950-01-01", initPosQty=0)
{ # @author Peter Carl

    # DESCRIPTION
    # Constructs the data container used to store calculated P&L values from 
    # transactions and close prices. The data series will be a regular time series.

    # Inputs
    # initDate: date prior to the first close price given, used to contain
    #           initial account equity and initial position
    # initPosQty: initial position, default is zero

    # Outputs
    # Constructs multi-column xts object used to store derived position information

    # FUNCTION
    posPL <- xts( as.matrix(t(c(initPosQty,0,0,0,0,0,0))), order.by=as.Date(initDate) )
    colnames(posPL) <- c('Pos.Qty', 'Pos.Value', 'Txn.Value', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL','Trading.PL')
    return(posPL)
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
