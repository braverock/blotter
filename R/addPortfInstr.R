addPortfInstr <- function(Portfolio,symbols,...,verbose=TRUE) 
{
    pname<-Portfolio
    portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio"))

    msymbols<-match(names(portfolio),symbols)
    symbols<-symbols[which(is.na(msymbols))]
    
    if(missing(initPosQty)) initPosQty=rep(0, length(symbols))
    
    for(instrument in symbols){
        i = match(instrument, symbols)
        portfolio[[instrument]]$txn = initTxn(initDate = initDate, initPosQty = initPosQty[i])
        portfolio[[instrument]]$posPL = initPosPL(initDate = initDate, initPosQty = initPosQty[i])
    }
    
    assign(paste("portfolio",as.character(pname),sep='.'),portfolio,envir=.blotter)
    
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

