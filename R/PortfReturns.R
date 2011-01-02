#' Calculate portfolio instrument returns
#' 
#' This function (for now) calculates return on initial equity for each instrument
#' in the portfolio or portfolios that make up an account.  These columns will be additive
#' to return on capital of each portfolio, or of the entire account.
#' 
#' This function exists because of R/Finance community requests by Mark Breman and Thomas Bolton 
#' @export
#' @param Account string name of the account to generate returns for
#' @param method for now, only 'contribution' is supported
#' @param \dots any other passthru parameters (like \code{native} for \code{.getBySymbol}
#' @param Dates xts style ISO 8601 date subset to retrieve, default NULL (all dates)
#' @param Portfolios concatenated string vector for portfolio names to retrieve returns on, default NULL (all portfolios)
#' @note
#' TODO handle portfolio and account in different currencies (not hard, just not done)
#' 
#' TODO explicitly handle portfolio weights
#' 
#' TODO provide additional  methods of calculating returns
#' 
#' TODO support additions and withdrawals to available capital 
PortfReturns <- function (Account, method=c('contribution'),...,Dates=NULL,Portfolios=NULL) 
{ # @author Brian Peterson
	aname<-Account
	if(!grepl("account\\.",aname)) Account<-try(get(paste("account",aname,sep='.'),envir=.blotter))
	else Account<-try(get(aname,envir=.blotter))
	if(inherits(Account,"try-error"))
		stop(paste("Account ",aname," not found, use initAcct() to create a new account"))
	if(!inherits(Account,"account")) stop("Account ",aname," passed is not the name of an account object.")
	
	if(is.null(Portfolios)) Portfolios = names(Account$portfolios)
	
	table=NULL
	for(pname in Portfolios){
		Portfolio <- getPortfolio(pname)
		if(is.null(Dates)) Dates <- paste("::",last(index(Portfolio$summary)),sep='')
		ptable = .getBySymbol(Portfolio = Portfolio, Attribute = "Net.Trading.PL", Dates = Dates,...)
		
		#TODO check portfolio and account currencies and convert if necessary
		
		#TODO handle additions and withdrawals in equity
		
		if(!is.null(attr(Account,'initEq'))){
			initEq<-as.numeric(attr(Account,'initEq'))
			if(initEq==0) stop("Initial equity of zero would produce div by zero NaN,Inf,-Inf returns, please fix in initAcct().")
			ptable = ptable/initEq
		}
		if(is.null(table)) table=ptable
		else table=cbind(table,ptable)
	}
	return(table)
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
