#' Calculates position PL from the position data and corresponding close price data. 
#' 
#' @param Portfolio a portfolio name to a portfolio structured with initPortf()
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Dates xts subset of dates, e.g., "2007-01::2008-04-15". These dates must appear in the price stream
#' @param Prices periodic prices in an xts object with a columnname compatible with \code{getPrice}
#' @param ConMult if necessary, numeric contract multiplier, not needed if instrument is defined. 
#' @param \dots any other passthru parameters
#' @return Regular time series of position information and PL 
#' @author Peter Carl, Brian Peterson
#' @rdname updatePosPL
.updatePosPL <- function(Portfolio, Symbol, Dates=NULL, Prices=NULL, ConMult=NULL, ...)
{ # @author Peter Carl, Brian Peterson
    rmfirst=FALSE
    prices=NULL
    pname<-Portfolio
    Portfolio<-getPortfolio(pname) 
	p.ccy.str<-attr(Portfolio,'currency')
	if(is.null(p.ccy.str)) p.ccy.str<-'NA'
    tmp_instr<-try(getInstrument(Symbol))
    if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
	    warning(paste("Instrument",Symbol," not found, things may break"))
		tmp_instr<-list(currency="USD",multiplier=1)
    }
    dargs <- list(...)
    if(!is.null(dargs$env)) {env <- dargs$env} else env=.GlobalEnv
    if(!is.null(dargs$symbol)) {symbol<-dargs$symbol} else symbol=NULL
    if(!is.null(dargs$prefer)) {prefer<-dargs$prefer} else prefer=NULL
    if(is.null(Prices)){
        prices=getPrice(get(Symbol, pos=env), symbol=symbol, prefer=prefer)[,1]
    } else {
        prices=Prices
    }
    if(.parseISO8601(Dates)$first.time < first(index(prices)) || is.na(.parseISO8601(Dates)$first.time)){
        Dates<-index(prices[paste('/',.parseISO8601(Dates)$last.time,sep='')])
    }
    
    if(is.null(Dates)) {# if no date is specified, get all available dates
            Dates = time(prices)
	} else if(!is.timeBased(Dates)) Dates = time(prices[Dates])
    
    if(ncol(prices)>1) prices=getPrice(Prices,Symbol)
    

	# line up Prices dates with Dates set/index/span passed in.
	startDate = first(xts:::.parseISO8601(Dates))$first.time-1 #does this need to be a smaller/larger delta for millisecond data?
	endDate   = last(xts:::.parseISO8601(Dates))$last.time
	if(is.na(endDate)) endDate<-NULL
	dateRange = paste(startDate,endDate,sep='::')
	
	#subset Prices by dateRange too...
	Prices<-prices[dateRange]
    
    if(nrow(Prices)<1) {
        Prices=xts(cbind(Prices=as.numeric(last(prices[paste('::',endDate,sep='')]))),as.Date(endDate))
        warning('no Prices available for ',Symbol,' in ',dateRange,' : using last available price and marking to ', endDate)
    }
	
	# Prices <- Prices[dateRange][,1] # only take the first column, if there is more than one
	
	colnames(Prices)<-'Prices' # name it so we can refer to it by name later
	
	#	***** Vectorization *****#
	# trim posPL slot to not double count, related to bug 831 on R-Forge 
	Portfolio$symbols[[Symbol]]$posPL<-Portfolio$symbols[[Symbol]]$posPL[paste('::',startDate,sep='')]
	Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]<-Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]][paste('::',startDate,sep='')]
	priorPL<-last(Portfolio$symbols[[Symbol]]$posPL)
	if(nrow(priorPL)==0) {
		cn<-colnames(priorPL)
		priorPL = xts(t(rep(0,ncol(priorPL))),order.by=startDate-1)
		colnames(priorPL)<-cn
	}
	
	Txns <- Portfolio$symbols[[Symbol]]$txn[dateRange]
	# if there are no transactions, get the last one before the current dateRange, we'll discard later
	if(nrow(Txns)==0) {
		Txns <- last(Portfolio$symbols[[Symbol]]$txn[paste('::',startDate,sep='')])
	} 
	
	#	 line up transaction with Dates list
	tmpPL <- merge(Txns, priorPL, Prices) # most Txn columns will get discarded later, as will the rows from 'before' the startDate
	
	#browser()
	
	if(is.na(tmpPL$Prices[1])){
		#first price is NA, it would be nice to fill it in with a previous last valid price
		fprice <- last(prices[paste('::',startDate,sep='')])
		if (length(fprice)==1) tmpPL$Prices[1] <- fprice 
	}
	
	# na.locf any missing prices with last observation (this assumption seems the only rational one for vectorization)
	tmpPL$Prices <- na.locf(tmpPL$Prices)

	# na.locf Pos.Qty,Con.Mult,Pos.Avg.Cost to instantiate $posPL new rows	
	#tmpPL$Pos.Qty.1 <- na.locf(tmpPL$Pos.Qty.1)
	#lagPosQty<-Lag(tmpPL$Pos.Qty.1)
	tmpPL$Pos.Qty <- ifelse(is.na(tmpPL$Pos.Qty) & !is.na(tmpPL$Pos.Qty.1), tmpPL$Pos.Qty.1, tmpPL$Pos.Qty)
	#tmpPL$Pos.Qty <- ifelse(is.na(tmpPL$Pos.Qty) & !is.na(lagPosQty), tmpPL$Pos.Qty.1, tmpPL$Pos.Qty)
	tmpPL$Pos.Qty <- na.locf(tmpPL$Pos.Qty)
    
    
    #TODO check for instrument multiplier rather than doing all this messing around, if possible.
	tmpPL$Con.Mult.1 <- na.locf(tmpPL$Con.Mult.1)
	tmpPL$Con.Mult.1 <- ifelse(is.na(tmpPL$Con.Mult) & !is.na(tmpPL$Con.Mult.1) , tmpPL$Con.Mult.1, tmpPL$Con.Mult)
	tmpPL$Con.Mult <- na.locf(tmpPL$Con.Mult)
    tmpPL$Con.Mult <- na.locf(tmpPL$Con.Mult, fromLast=TRUE) # carry NA's backwards too, might cause problems with options contracts that change multiplier
	tmpPL$Con.Mult <- ifelse(is.na(tmpPL$Con.Mult) ,1, tmpPL$Con.Mult)
	
	tmpPL$Pos.Avg.Cost.1 <- na.locf(tmpPL$Pos.Avg.Cost.1)
	tmpPL$Pos.Avg.Cost <- ifelse(is.na(tmpPL$Pos.Avg.Cost) & !is.na(tmpPL$Pos.Avg.Cost.1) ,tmpPL$Pos.Avg.Cost.1, tmpPL$Pos.Avg.Cost)
	tmpPL$Pos.Avg.Cost <- na.locf(tmpPL$Pos.Avg.Cost)
	
	# zerofill Txn.Value, Txn.Fees
	tmpPL$Txn.Value <- ifelse(is.na(tmpPL$Txn.Value),0, tmpPL$Txn.Value)
	
	tmpPL$Txn.Fees  <- ifelse(is.na(tmpPL$Txn.Fees) ,0, tmpPL$Txn.Fees)
	
	# matrix calc Pos.Qty * Price * Con.Mult to get Pos.Value
	tmpPL$Pos.Value <- tmpPL$Pos.Qty * tmpPL$Con.Mult * tmpPL$Prices
	
	LagValue<-Lag(tmpPL$Pos.Value)
	LagValue<-ifelse(is.na(LagValue),0,LagValue) # needed to avoid a possible NA on the first value that would mess up the Gross.Trading.PL calc
	tmpPL$Gross.Trading.PL <- tmpPL$Pos.Value- LagValue - tmpPL$Txn.Value
	
	
	# alternate matrix calc for Realized&Unrealized PL that is only dependent on Txn PL and Gross.Trading.PL
	tmpPL$Net.Txn.Realized.PL <- ifelse(is.na(tmpPL$Net.Txn.Realized.PL),0,tmpPL$Net.Txn.Realized.PL)
	tmpPL$Gross.Txn.Realized.PL <- ifelse(is.na(tmpPL$Gross.Txn.Realized.PL),0,tmpPL$Gross.Txn.Realized.PL)
	
	#tmpPL$Gross.Trading.PL <- tmpPL$Pos.Value - (tmpPL$Pos.Qty*tmpPL$Pos.Avg.Cost) +  tmpPL$Gross.Txn.Realized.PL
	tmpPL$Period.Realized.PL <- tmpPL$Gross.Txn.Realized.PL
	tmpPL$Period.Unrealized.PL <- round(tmpPL$Gross.Trading.PL - tmpPL$Period.Realized.PL,2)
	
	# matrix calc Net.Trading.PL as Gross.Trading.PL + Txn.Fees
	tmpPL$Net.Trading.PL <- tmpPL$Gross.Trading.PL + tmpPL$Txn.Fees

	# Ccy.Mult for this step is always 1
	tmpPL$Ccy.Mult<-rep(1,nrow(tmpPL))
	
	# reorder,discard  columns for insert into portfolio object
	tmpPL <- tmpPL[,c('Pos.Qty', 'Con.Mult', 'Ccy.Mult', 'Pos.Value', 'Pos.Avg.Cost', 'Txn.Value',  'Period.Realized.PL', 'Period.Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')]

	# rbind to $posPL slot
	tmpPL <- tmpPL[dateRange] #subset to get rid of any prior period Txn or PosPL rows we inserted
	Portfolio$symbols[[Symbol]]$posPL<-rbind(Portfolio$symbols[[Symbol]]$posPL,tmpPL)
		

    
    # now do the currency conversions for the whole date range
    TmpPeriods<-Portfolio$symbols[[Symbol]]$posPL[dateRange]

	CcyMult = NA 
	FXrate = NA
	invert=FALSE
	if(!is.null(attr(Portfolio,'currency'))) {
		if (tmp_instr$currency==p.ccy.str) {
			CcyMult<-1			
		} else {
			port_currency<-try(getInstrument(p.ccy.str))
			if(inherits(port_currency,"try-error") | !is.instrument(port_currency)){
				warning("Currency",p.ccy.str," not found, using currency multiplier of 1")
				CcyMult<-1
			} else { #convert from instr ccy to portfolio ccy
				FXrate.str<-paste(tmp_instr$currency, p.ccy.str, sep='') # currency quote convention is EURUSD which reads as "USD per EUR"
				FXrate<-try(get(FXrate.str), silent=TRUE)
				#TODO FIXME: this uses convention to sort out the rate, we should check $currency and $counter_currency and make sure directionality is correct 
				if(inherits(FXrate,"try-error")){
					FXrate.str<-paste(p.ccy.str, tmp_instr$currency, sep='')
					FXrate<-try(get(FXrate.str), silent=TRUE)
					if(inherits(FXrate,"try-error")){ 
						warning("Exchange Rate",FXrate.str," not found for symbol,',Symbol,' using currency multiplier of 1")
						CcyMult<-1
					} else {
						invert=TRUE
					}
				}
			}		
			
		}
	} else {
		message("no currency set on portfolio, using currency multiplier of 1")
		CcyMult =1
	}
	if(is.na(CcyMult) && !is.na(FXrate)) {
		if(inherits(FXrate,'xts')){
			CcyMult <- FXrate[dateRange]
			CcyMult <- na.locf(merge(CcyMult,index(TmpPeriods)))
			CcyMult <- drop(CcyMult[index(TmpPeriods)])
		} else {
			CcyMult<-as.numeric(FXrate)
		}
	} else {
		CcyMult<-1
	}
	if(isTRUE(invert)){
		# portfolio and instrument have different currencies, and FXrate was in the wrong direction
		CcyMult<-1/CcyMult
	}
	
	
	#multiply the correct columns 
    columns<-c('Pos.Value', 'Txn.Value', 'Pos.Avg.Cost', 'Period.Realized.PL', 'Period.Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
	TmpPeriods[,columns]<-TmpPeriods[,columns]*CcyMult
	TmpPeriods[,'Ccy.Mult']<-CcyMult
		
	#add change in Pos.Value in base currency
	LagValue <- as.numeric(last(Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]$Pos.Value))
	if(length(LagValue)==0) LagValue <- 0
	LagPos.Value <- lag(TmpPeriods$Pos.Value,1)
	LagPos.Value[1] <- LagValue
	CcyMove <- TmpPeriods$Pos.Value - LagPos.Value - TmpPeriods$Txn.Value - TmpPeriods$Period.Unrealized.PL - TmpPeriods$Period.Realized.PL
	TmpPeriods$Gross.Trading.PL <- TmpPeriods$Gross.Trading.PL + CcyMove
	TmpPeriods$Net.Trading.PL <- TmpPeriods$Net.Trading.PL + CcyMove
	TmpPeriods$Period.Unrealized.PL <- TmpPeriods$Period.Unrealized.PL + CcyMove
	
    #stick it in posPL.ccy
    Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]<-rbind(Portfolio$symbols[[Symbol]][[paste('posPL',p.ccy.str,sep='.')]],TmpPeriods)
	
    # assign Portfolio to environment
    assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
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
