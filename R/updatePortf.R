#' Function goes through each symbol and calculates the PL for each day prices are available
#' 
#' Inputs
#' Portfolio: a portfolio object containing transactions
#' Symbol: an instrument identifier for a symbol included in the portfolio
#' Dates: Dates for which to calculate equity account
#' These dates must appear in the price stream
#' 
#' Outputs
#' assigns position information and PL into the environment
#' 
#' @param Portfolio 
#' @param Symbols
#' @param Dates 
#' @param Prices
#' @export
updatePortf <- function(Portfolio, Symbols=NULL, Dates=NULL, Prices=NULL)
{ #' @author Peter Carl
    pname<-Portfolio
    Portfolio<-getPortfolio(pname) # TODO add Date handling

    # FUNCTION
    if(is.null(Symbols)){
        Symbols = names(Portfolio$symbols)
    } 
    for(symbol in Symbols){
        tmp_instr<-try(getInstrument(symbol))
        updatePosPL(Portfolio=pname, Symbol=as.character(symbol), Dates=Dates, Prices=Prices)            
    }
	
    # Calculate and store portfolio summary table
    Portfolio<-getPortfolio(pname) # refresh with an updated object
    #Symbols = names(Portfolio$symbols)
    Attributes = c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Realized.PL', 'Unrealized.PL', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
    summary = NULL
	tmp.attr=NULL
    for(attribute in Attributes) {
		result=NULL
        switch(attribute,
			Net.Value =,	
            Gross.Value =,
			Long.Value =,
			Short.Value =,{
				# all these use Pos.Value
				if(is.null(tmp.attr)){
					table = .getBySymbol(Portfolio = Portfolio, Attribute = "Pos.Value", Dates = Dates, Symbols = Symbols)
					tmp.attr="Pos.Value"
				}	
				switch(attribute,
						Gross.Value = {	result = xts(rowSums(abs(table), na.rm=TRUE), order.by=index(table))},
						Long.Value  = { tmat = apply(table,MARGIN=c(1,2),FUN=max,0)# comes out a matrix
										result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
						},
						Short.Value = { tmat = apply(table,MARGIN=c(1,2),FUN=min,0) # comes out a matrix
										result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
						},
						Net.Value   = {	result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))	}
				)
            },
			Realized.PL =,
			Unrealized.PL =,
			Gross.Trading.PL =,
			Txn.Fees =,
			Net.Trading.PL = { 
				table = .getBySymbol(Portfolio = Portfolio, Attribute = attribute, Dates = Dates, Symbols = Symbols)
				tmp.attr = NULL
				result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
            }
        )
		
        colnames(result) = attribute
		if(is.null(summary)) {summary=result}
		else {summary=cbind(summary,result)}
    }
	
	if(!is.timeBased(Dates)) Dates = time(Portfolio$summary[Dates])
	startDate = xts:::.parseISO8601(first(Dates))$first.time-1 #does this need to be a smaller delta for millisecond data?
	# trim summary slot to not double count, related to bug 831 on R-Forge, and rbind new summary 
	if(attr(Portfolio,'initDate')>=startDate){
		Portfolio$summary<-summary #changes to subset might not return a empty dimnames set of columns
	}else{
		Portfolio$summary<-rbind(Portfolio$summary[paste('::',startDate,sep='')],summary)
	}
	# assign Portfolio to environment
	assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
	
    return(pname) #not sure this is a good idea
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
