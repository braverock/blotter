#' Extract transactions from a portfolio
#' 
#' This function reverse-engineers \code{\link{addTxn}} calls for all the transactions in \code{Portfolio}.
#' This is the fundamental task required to create a reproducible example, as it would replicate the 
#' state of the $txn slot in the portfolio after each \code{addTxn} call.  
#' While market data, expected results, portfolio and account setup, etc, are also required, 
#' these can usually be deduced or equivalent formulations can be found.  
#' 
#' For transactions, only the exact \code{addTxn} parameters will recreate the $txn slot.  This function creates that reproducibility, since the result (stored in 'x') can be used to regenerate transactions with \code{eval(parse(text=x))}.
#' 
#' @param Portfolio string identifying the portfolio to extract from
#' @return String vector of \code{\link{addTxn}} calls that would replicate the given portfolio.
#' @author Brian G. Peterson, Josh Ulrich
#' @export
extractTxns <- function(Portfolio)
{ # @author Brian G. Peterson, Josh Ulrich
	
	pname <- Portfolio	
	Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
	if(inherits(Portfolio,"try-error"))
		stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio first"))
	out<-NULL
	symbolnames<-names(Portfolio[['symbols']])
	for (Symbol in symbolnames) {
		tmpTxns<-Portfolio$symbols[[Symbol]]$txn[-1,]
		if (nrow(tmpTxns)>=1){
			tmpout<-NULL
			
			for (i in 1:nrow(tmpTxns)){
				tmpout[i]<- paste(  "addTxn(Portfolio ='",pname,
								"', Symbol ='",Symbol,
								"', TxnDate = '",index(tmpTxns)[i],
								"', TxnQty =",tmpTxns[i,'Txn.Qty'],
								", TxnPrice =",tmpTxns[i,'Txn.Price'],
								", TxnFees =",tmpTxns[i,'Txn.Fees'],
								", ConMult =",tmpTxns[i,'Con.Mult'],
								")",
								sep=''
							)
				
			}
									
			if(is.null(out)) out <- tmpout
			else out<- c(out,tmpout)
		}
	}
	return(out)
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
