#' Add transactions to a portfolio.
#' 
#' When a trade or adjustment is made to the Portfolio, the addTxn function 
#' calculates the value and average cost of the transaction,  the change in 
#' position, the resulting positions average cost, and any realized profit 
#' or loss (net of fees) from the transaction. Then it stores the transaction 
#' and calculations in the Portfolio object.
#'
#' Fees are indicated as negative values and will be subtracted from the 
#' transaction value. TxnFees can either be a fixed amount, or a function 
#' in which case the function is evaluated to 
#' determine the fee amount.
#' The \code{pennyPerShare} function provides a simple example of a transaction cost
#' function.
#' 
#' Transactions which would cross your position through zero will be split 
#' into two transactions, one to flatten the position, and another to initiate 
#' a new position on the opposite side of the market.  The new (split) 
#' transaction will have it's timestamp inclremented by eps to preserve ordering. 
#' This transaction splitting vastly simplifies realized P&L calculations elsewhere in the code.
#' 
#' @param Portfolio  A portfolio name that points to a portfolio object structured with \code{initPortf()}
#' @param Symbol An instrument identifier for a symbol included in the portfolio, e.g., "IBM"
#' @param TxnDate  Transaction date as ISO 8601, e.g., '2008-09-01' or '2010-01-05 09:54:23.12345'
#' @param TxnQty Total units (such as shares or contracts) transacted.  Positive values indicate a 'buy'; negative values indicate a 'sell'
#' @param TxnPrice  Price at which the transaction was done
#' @param \dots Any other passthrough parameters
#' @param TxnFees Fees associated with the transaction, e.g. commissions., See Details
#' @param ConMult Contract/instrument multiplier for the Symbol if it is not dened in an instrument specication
#' @param verbose If TRUE (default) the function prints the elements of the transaction in a line to the screen, e.g., "2007-01-08 IBM 50 @@ 77.6". Suppress using FALSE.
#' @param eps value to add to force unique indices
#' @note 
#' The addTxn function will eventually also handle other transaction types, 
#' such as adjustments for corporate actions or expire/assign for options. 

#' @seealso \code{\link{addTxns}}, \code{\link{pennyPerShare}}, \code{\link{initPortf}}
#' @author Peter Carl
#' @export
addTxn <- function(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, ..., TxnFees=0, ConMult=NULL, verbose=TRUE, eps=1e-06)
{ # @author Peter Carl

    pname <- Portfolio
    PrevPosQty = getPosQty(pname, Symbol, TxnDate)
    
    if(!is.timeBased(TxnDate) ){
        TxnDate<-as.POSIXct(TxnDate)
    }
    
    # split transactions that would cross through zero
    if(PrevPosQty!=0 && sign(PrevPosQty+TxnQty)!=sign(PrevPosQty) && PrevPosQty!=-TxnQty){
        addTxn(Portfolio=pname, Symbol=Symbol, TxnDate=TxnDate, TxnQty=-PrevPosQty, TxnPrice=TxnPrice, ..., 
                TxnFees = TxnFees, ConMult = ConMult, verbose = verbose, eps=eps)
        TxnDate=TxnDate+2*eps #transactions need unique timestamps, so increment a bit
        TxnQty=TxnQty+PrevPosQty
        PrevPosQty=0
    }
    
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


    # FUNCTION
    # Compute transaction fees if a function was supplied
    if (is.function(TxnFees)) txnfees <- TxnFees(TxnQty, TxnPrice) else txnfees<- as.numeric(TxnFees)
    if(is.null(txnfees) | is.na(txnfees)) txnfees = 0
    if(txnfees>0) warning('Positive Transaction Fees should only be used in the case of broker/exchange rebates for TxnFees ',TxnFees,'. See Documentation.')
    
    # Calculate the value and average cost of the transaction
    TxnValue = .calcTxnValue(TxnQty, TxnPrice, 0, ConMult) # Gross of Fees
    TxnAvgCost = .calcTxnAvgCost(TxnValue, TxnQty, ConMult)

    # Calculate the change in position
    PosQty = PrevPosQty + TxnQty


    # Calculate the resulting position's average cost
    PrevPosAvgCost = .getPosAvgCost(pname, Symbol, TxnDate)
    PosAvgCost = .calcPosAvgCost(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty, ConMult)

	
    # Calculate any realized profit or loss (net of fees) from the transaction
    GrossTxnRealizedPL = TxnQty * ConMult * (PrevPosAvgCost - TxnAvgCost)

	# if the previous position is zero, RealizedPL = 0
	# if previous position is positive and position is larger, RealizedPL =0
	# if previous position is negative and position is smaller, RealizedPL =0
	if(abs(PrevPosQty) < abs(PosQty) | (PrevPosQty = 0))
		GrossTxnRealizedPL = 0
	
	NetTxnRealizedPL = GrossTxnRealizedPL + txnfees

    # Store the transaction and calculations
    NewTxn = xts(t(c(TxnQty, TxnPrice, TxnValue, TxnAvgCost, PosQty, PosAvgCost, GrossTxnRealizedPL, txnfees, NetTxnRealizedPL, ConMult)), order.by=TxnDate)
    #colnames(NewTxns) = c('Txn.Qty', 'Txn.Price', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Gross.Txn.Realized.PL', 'Txn.Fees', 'Net.Txn.Realized.PL', 'Con.Mult')
    Portfolio$symbols[[Symbol]]$txn<-rbind(Portfolio$symbols[[Symbol]]$txn, NewTxn)

    if(verbose)
        print(paste(TxnDate, Symbol, TxnQty, "@",TxnPrice, sep=" "))
        #print(Portfolio$symbols[[Symbol]]$txn)
    
    assign(paste("portfolio",pname,sep='.'),Portfolio,envir=.blotter)
}

#' Example TxnFee cost function
#' @param TxnQty total units (such as shares or contracts) transacted.  Positive values indicate a 'buy'; negative values indicate a 'sell'
#' This is an example intended to demonstrate how a cost function could be used in place of a flat numeric fee.
#' @export
pennyPerShare <- function(TxnQty) {
    return(abs(TxnQty) * -0.01)
}

#' Add multiple transactions to a portfolio
#' @param Portfolio  A portfolio name that points to a portfolio object structured with \code{\link{initPortf}}
#' @param Symbol An instrument identifier for a symbol included in the portfolio, e.g., "IBM"
#' @param TxnData  An xts object containing all required txn fields
#' @param \dots Any other passthrough parameters
#' @param verbose If TRUE (default) the function prints the elements of the transaction in a line to the screen, e.g., "2007-01-08 IBM 50 @@ 77.6". Suppress using FALSE.
#' @param ConMult Contract or instrument multiplier for the Symbol if it is not dened in an instrument specication
#' @seealso \code{\link{addTxn}}, \code{\link{initPortf}}
#' @note
#' TODO figure out if we can fully vectorize this function to make it faster
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
            PrevPosAvgCost <- .getPosAvgCost(pname, Symbol, index(TxnData[row,]))
        }
        #TODO create vectorized versions of all these functions so we don't have to loop
        TxnQty         <- as.numeric(TxnData[row,'Quantity'])
        TxnPrice       <- as.numeric(TxnData[row,'Price'])
        TxnFee         <- 0 #TODO FIXME support transaction fees in addTxns
        #TxnFee         <- ifelse( is.function(TxnFees), TxnFees(TxnQty, TxnPrice), TxnFees)
        TxnValue       <- .calcTxnValue(TxnQty, TxnPrice, TxnFee, ConMult)
        TxnAvgCost     <- .calcTxnAvgCost(TxnValue, TxnQty, ConMult)
        #PrevPosQty     <- getPosQty(pname, Symbol, index(TxnData[row,]))
        PosQty         <- PrevPosQty+TxnQty
        PosAvgCost     <- .calcPosAvgCost(PrevPosQty, PrevPosAvgCost, 0, PosQty, ConMult) # lag this over the data?
		GrossTxnRealizedPL = TxnQty * ConMult * (PrevPosAvgCost - TxnAvgCost)
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

#' Add cash dividend transactions to a portfolio.
#' 
#' Adding a cash dividend does not affect position quantity, like a split would.
#' 
#' @param Portfolio  A portfolio name that points to a portfolio object structured with \code{\link{initPortf}}.
#' @param Symbol An instrument identifier for a symbol included in the portfolio, e.g., IBM.
#' @param TxnDate  Transaction date as ISO 8601, e.g., '2008-09-01' or '2010-01-05 09:54:23.12345'.
#' @param DivPerShare The amount of the cash dividend paid per share or per unit quantity.
#' @param \dots Any other passthrough parameters.
#' @param TxnFees Fees associated with the transaction, e.g. commissions. See Details.
#' @param verbose If TRUE (default) the function prints the elements of the transaction in a line to the screen, e.g., "2007-01-08 IBM 50 @@ 77.6". Suppress using FALSE.
#' @param ConMult Contract or instrument multiplier for the Symbol if it is not dened in an instrument specication.
#' @export
#' @note
#' # TODO add TxnTypes to $txn table
#' 
#' # TODO add AsOfDate 
#' 
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

    # FUNCTION
    # 
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
    PrevPosAvgCost = .getPosAvgCost(pname, Symbol, TxnDate)
    PosAvgCost = PrevPosAvgCost # but carry it forward in $txn

    # Calculate any realized profit or loss (net of fees) from the transaction
    GrossTxnRealizedPL = PrevPosQty * DivPerShare * ConMult
    NetTxnRealizedPL = GrossTxnRealizedPL + TxnFees

    # Store the transaction and calculations
    NewTxn = xts(t(c(TxnQty, TxnPrice, TxnValue, TxnAvgCost, PosQty, PosAvgCost, GrossTxnRealizedPL, TxnFees, NetTxnRealizedPL, ConMult)), order.by=as.POSIXct(TxnDate))
    #colnames(NewTxns) = c('Txn.Qty', 'Txn.Price', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Gross.Txn.Realized.PL', 'Txn.Fees', 'Net.Txn.Realized.PL', 'Con.Mult')
    Portfolio$symbols[[Symbol]]$txn<-rbind(Portfolio$symbols[[Symbol]]$txn, NewTxn)

    if(verbose)
        print(paste(TxnDate, Symbol, "Dividend", DivPerShare, "on", PrevPosQty, "shares:", -TxnValue, sep=" "))
        #print(Portfolio$symbols[[Symbol]]$txn)

    assign(paste("portfolio",pname,sep='.'),Portfolio,envir=.blotter)
}
###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/)
# Copyright (c) 2008-2011 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
