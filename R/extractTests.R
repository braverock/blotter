
extractTxns <- function(Portfolio)
{ # @author Brian G. Peterson, Josh Ulrich
	
	pname <- Portfolio	
	Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
	if(inherits(Portfolio,"try-error"))
		stop(paste("Portfolio",name," not found, use initPortf() to create a new portfolio first"))
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
# Copyright (c) 2008-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: getTxn.R 378 2010-08-20 18:12:00Z braverock $
#
###############################################################################
