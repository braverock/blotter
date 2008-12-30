`addTxn` <-
function(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, TxnFees=0, verbose=TRUE)
{ # @author Peter Carl

    # DESCRIPTION:
    # Adds transactions to a portfolio.

    # Inputs
    # Portfolio: a portfolio object structured with initPortf()
    # Symbol: an instrument identifier for a symbol included in the portfolio,
    #   e.g., IBM
    # TxnDate: transaction date as ISO 8106, e.g., '2008-09-01'
    # TxnQty: total units (such as shares) transacted.  Positive values indicate
    #   a 'buy'; negative values indicate a 'sell'
    # TxnPrice: price at which the transaction was done
    # TxnFees: fees associated with the transaction, e.g. commissions.  Fees are
    #   indicated as positive values and will be subtracted from the transaction.

    # Outputs:
    # Portfolio: hands back the entire portfolio object with the additional
    # transaction in the correct slot: Portfolio[[Symbol]]$txn

    # FUNCTION
    # Calculate the value and average cost of the transaction
    TxnValue = calcTxnValue(TxnQty, TxnPrice, TxnFees)
    TxnAvgCost = calcTxnAvgCost(TxnValue, TxnQty)

    # Calculate the change in position
    PrevPosQty = getPosQty(Portfolio, Symbol, TxnDate)
    PosQty = PrevPosQty + TxnQty 

    # Calculate the resulting position's average cost
    PrevPosAvgCost = getPosAvgCost(Portfolio, Symbol, TxnDate)
    PosAvgCost = calcPosAvgCost(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty) 

    # Calculate any realized profit or loss (net of fees) from the transaction
    RealizedPL = calcRealizedPL(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty)

    # Store the transaction and calculations
    NewTxn = xts(t(c(TxnQty, TxnPrice, TxnFees, TxnValue, TxnAvgCost, PosQty, PosAvgCost, RealizedPL)), order.by=as.Date(TxnDate))
    colnames(NewTxn) = c('Txn.Qty', 'Txn.Price', 'Txn.Fees', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Realized.PL')
    Portfolio[[Symbol]]$txn <- rbind(Portfolio[[Symbol]]$txn, NewTxn) 

    if(verbose)
        print(paste(Symbol, TxnDate, TxnQty, "@",TxnPrice, sep=" "))
    return(Portfolio)
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
