#' Adds transactions to a portfolio.
#' 
#' Fees are indicated as negative values and will be subtracted from the transaction value. TxnFees can either be a fixed amount, or a function of two arguments Qty and Price in which case the function is evaluated to determine the fee amount.
#' 
#' @param Portfolio  a portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol an instrument identifier for a symbol included in the portfolio,e.g., IBM
#' @param TxnDate  transaction date as ISO 8106, e.g., '2008-09-01'
#' @param TxnQty total units (such as shares) transacted.  Positive values indicate a 'buy'; negative values indicate a 'sell'
#' @param TxnPrice  price at which the transaction was done
#' @param TxnFees fees associated with the transaction, e.g. commissions., See Details  
#' @param verbose 
#' @author Peter Carl
#' @export
addTxn <- function(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, TxnFees=0, verbose=TRUE)
{ # @author Peter Carl
    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
    
    # Outputs:
    # Portfolio: hands back the entire portfolio object with the additional
    # transaction in the correct slot: Portfolio[[Symbol]]$txn

    # FUNCTION
    # Compute transaction fees if a function was supplied
    txncost <- ifelse( is.function(TxnFees), TxnFees(TxnQty, TxnPrice), TxnFees)
    # Calculate the value and average cost of the transaction
    TxnValue = calcTxnValue(TxnQty, TxnPrice, txncost)
    TxnAvgCost = calcTxnAvgCost(TxnValue, TxnQty)

    # Calculate the change in position
    PrevPosQty = getPosQty(pname, Symbol, TxnDate)
    PosQty = PrevPosQty + TxnQty

    # Calculate the resulting position's average cost
    PrevPosAvgCost = getPosAvgCost(pname, Symbol, TxnDate)
    PosAvgCost = calcPosAvgCost(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty)

    # Calculate any realized profit or loss (net of fees) from the transaction
    RealizedPL = calcRealizedPL(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty)

    # Store the transaction and calculations
    NewTxn = xts(t(c(TxnQty, TxnPrice, txncost, TxnValue, TxnAvgCost, PosQty, PosAvgCost, RealizedPL)), order.by=as.POSIXct(TxnDate))
    colnames(NewTxn) = c('Txn.Qty', 'Txn.Price', 'Txn.Fees', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Realized.PL')
    Portfolio[[Symbol]]$txn <- rbind(Portfolio[[Symbol]]$txn, NewTxn)

    if(verbose)
        print(paste(TxnDate, Symbol, TxnQty, "@",TxnPrice, sep=" "))
    
    assign(paste("portfolio",pname,sep='.'),Portfolio,envir=.blotter)
}

## example cost function
pennyPerShare <- function(TxnQty, TxnPrice) {
    return(TxnQty * -0.01)
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
