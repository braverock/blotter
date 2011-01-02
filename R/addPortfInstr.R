#' add an instrument to a portfolio
#' 
#' thanks to WolfGang Wu for making this function more usable
#' 
#' @param Portfolio portfolio identifier string
#' @param symbols character vector of symbols to add to the portfolio
#' @param \dots any other passthru parameters
#' @export
addPortfInstr <- function(Portfolio,symbols,...) 
{
    pname<-Portfolio
    portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)	
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio"))
	initDate <- attr(portfolio, "initDate")    
	currency <- attr(portfolio, "currency")
    for(instrument in symbols){
        portfolio$symbols[[instrument]]$txn = .initTxn(initDate = initDate, initPosQty = 0)
        portfolio$symbols[[instrument]]$posPL = .initPosPL(initDate = initDate, initPosQty = 0)
		portfolio$symbols[[instrument]][[paste('posPL',currency,sep='.')]] = portfolio$symbols[[instrument]]$posPL		
    }

    assign(paste("portfolio",as.character(pname),sep='.'),portfolio,envir=.blotter)    
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

