PortfReturns <- function (Account, method=c('contribution'),...,Dates=NULL,Portfolios=NULL) 
{ # @author Brian Peterson
	aname<-Account
	if(!grepl("account\\.",aname)) Account<-try(get(paste("account",aname,sep='.'),envir=.blotter))
	else Account<-try(get(aname,envir=.blotter))
	if(inherits(Account,"try-error"))
		stop(paste("Account ",aname," not found, use initAcct() to create a new account"))
	if(!inherits(Account,"account")) stop("Account ",aname," passed is not the name of an account object.")
	
	Portfolios = names(Account$portfolios)
	table=NULL
	for(pname in Portfolios){
		#Portfolio<-
		ptable = .getBySymbol(Portfolio = pname, Attribute = "Net.Trading.PL", Dates = Dates,...)
		
		#TODO check portfolio and account currencies and convert if necessary
		
		if(!is.null(attr(Account,'initEq'))){
			ptable = ptable/as.numeric(attr(Account,'initEq'))
		}
		if(is.null(table)) table=ptable
		else table=cbind(table,ptable)
	}
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
