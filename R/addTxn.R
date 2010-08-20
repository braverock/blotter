#' Adds transactions to a portfolio.
#' 
#' Fees are indicated as negative values and will be subtracted from the transaction value. TxnFees can either be a fixed amount, or a function of two arguments Qty and Price in which case the function is evaluated to determine the fee amount.
#' 
#' @param Portfolio  a portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol an instrument identifier for a symbol included in the portfolio,e.g., IBM
#' @param TxnDate  transaction date as ISO 8106, e.g., '2008-09-01'
#' @param TxnQty total units (such as shares) transacted.  Positive values indicate a 'buy'; negative values indicate a 'sell'
#' @param TxnPrice  price at which the transaction was done
#' @param \dots any other passthrough parameters
#' @param TxnFees fees associated with the transaction, e.g. commissions., See Details
#' @param ConMult 
#' @param verbose TRUE/FALSE
#' @author Peter Carl
#' @export
addTxn <- function(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, ..., TxnFees=0, ConMult=NULL, verbose=TRUE)
{ # @author Peter Carl
	
    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
    
    if(is.null(ConMult) | !hasArg(ConMult)){
        tmp_instr<-try(getInstrument(Symbol))
        if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
            warning(paste("Instrument",Symbol," not found, using contract multiplier of 1"))
            ConMult<-1
        } else {
            ConMult<-tmp_instr$multiplier
        }  
    }
	
	
	
	#If there is no table for the symbol then create a new one
	if (is.null(Portfolio$symbols[[Symbol]])){ 
		addPortfInstr(Portfolio=pname, symbols=Symbol)
		Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
	}

    # Outputs:
    # Portfolio: hands back the entire portfolio object with the additional
    # transaction in the correct slot: Portfolio$symbols[[Symbol]]$txn

    # FUNCTION
    # Compute transaction fees if a function was supplied
    txnfees <- ifelse( is.function(TxnFees), TxnFees(TxnQty, TxnPrice), TxnFees)
    # Calculate the value and average cost of the transaction
    TxnValue = calcTxnValue(TxnQty, TxnPrice, 0, ConMult) # Gross of Fees
    TxnAvgCost = calcTxnAvgCost(TxnValue, TxnQty, ConMult)

    # Calculate the change in position
    PrevPosQty = getPosQty(pname, Symbol, TxnDate)
    PosQty = PrevPosQty + TxnQty

    # Calculate the resulting position's average cost
    PrevPosAvgCost = getPosAvgCost(pname, Symbol, TxnDate)
    PosAvgCost = calcPosAvgCost(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty, ConMult)

    # Calculate any realized profit or loss (net of fees) from the transaction
    GrossTxnRealizedPL = calcRealizedPL(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty, ConMult)
    NetTxnRealizedPL = GrossTxnRealizedPL + txnfees

    # Store the transaction and calculations
    NewTxn = xts(t(c(TxnQty, TxnPrice, TxnValue, TxnAvgCost, PosQty, PosAvgCost, GrossTxnRealizedPL, txnfees, NetTxnRealizedPL, ConMult)), order.by=as.POSIXct(TxnDate))
    #colnames(NewTxns) = c('Txn.Qty', 'Txn.Price', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Gross.Txn.Realized.PL', 'Txn.Fees', 'Net.Txn.Realized.PL', 'Con.Mult')
    Portfolio$symbols[[Symbol]]$txn<-rbind(Portfolio$symbols[[Symbol]]$txn, NewTxn)

    if(verbose)
        print(paste(TxnDate, Symbol, TxnQty, "@",TxnPrice, sep=" "))
        #print(Portfolio$symbols[[Symbol]]$txn)
    
    assign(paste("portfolio",pname,sep='.'),Portfolio,envir=.blotter)
}

## example cost function
pennyPerShare <- function(TxnQty, TxnPrice) {
    return(TxnQty * -0.01)
}

#' @export
addTxns<- function(Portfolio, Symbol, TxnData , verbose=TRUE, ..., ConMult=NULL)
{
    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)

    if(is.null(ConMult) | !hasArg(ConMult)){
        tmp_instr<-try(getInstrument(Symbol))
        if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
            warning(paste("Instrument",Symbol," not found, using contract multiplier of 1"))
            ConMult<-1
        } else {
            ConMult<-tmp_instr$multiplier
        }  
    }    

    for (row in 1:nrow(TxnData)) {
        if(row==1) {
            PrevPosQty     <- getPosQty(pname, Symbol, index(TxnData[row,]))
            PrevPosAvgCost <- getPosAvgCost(pname, Symbol, index(TxnData[row,]))
        }
        #TODO create vectorized versions of all these functions so we don't have to loop
        TxnQty         <- as.numeric(TxnData[row,'Quantity'])
        TxnPrice       <- as.numeric(TxnData[row,'Price'])
        TxnFee         <- 0 #TODO FIXME support transaction fees in addTxns
        #TxnFee         <- ifelse( is.function(TxnFees), TxnFees(TxnQty, TxnPrice), TxnFees)
        TxnValue       <- calcTxnValue(TxnQty, TxnPrice, TxnFee, ConMult)
        TxnAvgCost     <- calcTxnAvgCost(TxnValue, TxnQty, ConMult)
        #PrevPosQty     <- getPosQty(pname, Symbol, index(TxnData[row,]))
        PosQty         <- PrevPosQty+TxnQty
        PosAvgCost     <- calcPosAvgCost(PrevPosQty, PrevPosAvgCost, 0, PosQty, ConMult) # lag this over the data?
        GrossTxnRealizedPL = calcRealizedPL(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty, ConMult)
        NetTxnRealizedPL = GrossTxnRealizedPL - TxnFee
        PrevPosQty     <- PosQty
        PrevPosAvgCost <- PosAvgCost
        
        NewTxn = xts(t(c(TxnQty, 
                         TxnPrice, 
                         TxnValue, 
                         TxnAvgCost, 
                         PosQty, 
                         PosAvgCost, 
                         GrossTxnRealizedPL,
                         TxnFee,
                         NetTxnRealizedPL,
                         ConMult)),
                         order.by=index(TxnData[row,]))

        if(row==1){
            NewTxns <- NewTxn
            colnames(NewTxns) = c('Txn.Qty', 'Txn.Price', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Gross.Txn.Realized.PL', 'Txn.Fees', 'Net.Txn.Realized.PL', 'Con.Mult')
        } else {
            NewTxns<-rbind(NewTxns, NewTxn)
        }
    }
    Portfolio$symbols[[Symbol]]$txn<-rbind(Portfolio$symbols[[Symbol]]$txn,NewTxns) 

    if(verbose) print(NewTxns)
    
    assign(paste("portfolio",pname,sep='.'),Portfolio,envir=.blotter)    
}

addDiv <- function(Portfolio, Symbol, TxnDate, DivPerShare, ..., TxnFees=0, ConMult=NULL, verbose=TRUE)
{ # @author Peter Carl
    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)

    if(is.null(ConMult) | !hasArg(ConMult)){
        tmp_instr<-try(getInstrument(Symbol))
        if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
            warning(paste("Instrument",Symbol," not found, using contract multiplier of 1"))
            ConMult<-1
        } else {
            ConMult<-tmp_instr$multiplier
        }
    }
    # Outputs:
    # Portfolio: hands back the entire portfolio object with the additional
    # transaction in the correct slot: Portfolio$symbols[[Symbol]]$txn

    # FUNCTION
    # Adding a Dividend does not affect position
    TxnQty = 0
    TxnPrice = 0
#     TxnType = "Dividend"
# TODO add TxnTypes to $txn table

    # Get the current position quantity
    PrevPosQty = getPosQty(pname, Symbol, TxnDate)
    PosQty = PrevPosQty # no change to position, but carry it forward
    # Calculate the value and average cost of the transaction
    # The -1 multiplier allows a positive DivPerShare value to create a
    # positive realized gain
    TxnValue = -1 * PrevPosQty * DivPerShare * ConMult # Calc total dividend paid
    TxnAvgCost = DivPerShare

    # No change to the the resulting position's average cost
    PrevPosAvgCost = getPosAvgCost(pname, Symbol, TxnDate)
    PosAvgCost = PrevPosAvgCost # but carry it forward in $txn

    # Calculate any realized profit or loss (net of fees) from the transaction
    GrossTxnRealizedPL = PrevPosQty * DivPerShare * ConMult
    NetTxnRealizedPL = GrossTxnRealizedPL + TxnFees

    # Store the transaction and calculations
    NewTxn = xts(t(c(TxnQty, TxnPrice, TxnValue, TxnAvgCost, PosQty, PosAvgCost, GrossTxnRealizedPL, TxnFees, NetTxnRealizedPL, ConMult)), order.by=as.POSIXct(TxnDate))
    #colnames(NewTxns) = c('Txn.Qty', 'Txn.Price', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Gross.Txn.Realized.PL', 'Txn.Fees', 'Net.Txn.Realized.PL', 'Con.Mult')
    Portfolio$symbols[[Symbol]]$txn<-rbind(Portfolio$symbols[[Symbol]]$txn, NewTxn)

    if(verbose)
        print(paste(TxnDate, Symbol, "Dividend", DivPerShare, "on", PrevPosQty, "shares:", TxnValue, sep=" "))
        #print(Portfolio$symbols[[Symbol]]$txn)

    assign(paste("portfolio",pname,sep='.'),Portfolio,envir=.blotter)
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
