#' Calculates position PL from the position data and corresponding close price data. 
#' 
#' @param Portfolio a portfolio name to a portfolio structured with initPortf()
#' @param Symbol an instrument identifier for a symbol included in the portfolio
#' @param Dates xts subset of dates, e.g., "2007-01::2008-04-15". These dates must appear in the price stream
#' @param Prices periodic prices in an xts object with a columnname compatible with \code{getPrice}
#' @param ConMult if necessary, numeric contract multiplier, not needed if instrument is defined. 
#' @param Interval optional character string, containing one of "millisecond" (or "ms"), "microsecond" (or "us"),
#' "second", "minute", "hour", "day", "week", "month", "quarter", or "year".  This can optionally be preceded by
#' a positive integer, or followed by "s".
#' @param \dots any other passthru parameters
#' @return Regular time series of position information and PL 
#' @author Peter Carl, Brian Peterson
#' @rdname updatePosPL
.updatePosPL <- function(Portfolio, Symbol, Dates=NULL, Prices=NULL, ConMult=NULL, Interval=NULL, ...)
{ # @author Peter Carl, Brian Peterson
  rmfirst=FALSE
  prices=NULL
  pname<-Portfolio
  Portfolio<-.getPortfolio(pname) 
	p.ccy.str<-attr(Portfolio,'currency')
	if(is.null(p.ccy.str)) p.ccy.str<-'NA'
    tmp_instr<-try(getInstrument(Symbol), silent=TRUE)
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

    # if no date is specified, get all available dates
    if(is.null(Dates)) {
        Dates = index(prices)
        # Convert to POSIXct w/same TZ as portfolio object
        if(any(indexClass(prices) %in% c("Date","yearmon","yearqtr"))) {
            portfTZ <- indexTZ(Portfolio$symbols[[Symbol]]$txn)
            Dates <- as.POSIXct(as.character(as.Date(Dates)), tz=portfTZ)
        }
    } else if(!is.timeBased(Dates)) {
        Dates<- if(is.na(.parseISO8601(Dates)$first.time) ||
            .parseISO8601(Dates)$first.time < as.POSIXct(first(index(prices)))){
            index(prices[paste('/',.parseISO8601(Dates)$last.time,sep='')])
        } else index(prices[Dates])
    }
    if(!missing(Interval) && !is.null(Interval)) {
        ep_args <- .parse_interval(Interval)
        prices <- prices[endpoints(prices, on=ep_args$on, k=ep_args$k)]
    }
    
    if(ncol(prices)>1) prices=getPrice(Prices,Symbol)
    
	# line up Prices dates with Dates set/index/span passed in.
	startDate = first(Dates)-.00001 #does this need to be a smaller/larger delta for millisecond data?
	endDate   = last(Dates)
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
	
	# Get values frop priorPL into Txns; only keep columns we need from Txns
	# NOTE: There will usually be fewer transactions than price observations,
	# so do as much as possible before merging with potentially large price data
	TxnsCols <- c('Txn.Value','Txn.Fees','Gross.Txn.Realized.PL','Net.Txn.Realized.PL','Pos.Qty','Pos.Avg.Cost','Con.Mult')
	tmpPL <- merge(Txns[,TxnsCols], xts(,index(priorPL)))
	if(is.na(tmpPL[1,'Pos.Qty']))
		tmpPL[1,'Pos.Qty'] <- priorPL[1,'Pos.Qty']
	if(is.na(tmpPL[1,'Con.Mult']))
		tmpPL[1,'Con.Mult'] <- priorPL[1,'Con.Mult']
	if(is.na(tmpPL[1,'Pos.Avg.Cost']))
		tmpPL[1,'Pos.Avg.Cost'] <- priorPL[1,'Pos.Avg.Cost']
	
	# Now merge with prices
	tmpPL <- merge(tmpPL, Prices)
	
	if(is.na(tmpPL[1,'Prices'])){
		#first price is NA, it would be nice to fill it in with a previous last valid price
		fprice <- last(prices[paste('::',startDate,sep='')])
		if (length(fprice)==1) tmpPL[1,'Prices'] <- fprice 
                # if there's no previous valid price, calculate it from the prior position value
                # (can occur if .updatePosPL is called repeatedly with only one date/price)
                if (length(fprice)==0) tmpPL[1,'Prices'] <- priorPL[,'Pos.Value'] / priorPL[,'Pos.Qty']
	}
	# na.locf any missing prices with last observation (this assumption seems the only rational one for vectorization)
	# and na.locf Pos.Qty,Con.Mult,Pos.Avg.Cost to instantiate $posPL new rows
	columns <- c('Prices','Pos.Qty','Con.Mult','Pos.Avg.Cost')
	tmpPL[,columns] <- na.locf(tmpPL[,columns])
	    
	#TODO check for instrument multiplier rather than doing all this messing around, if possible.
	tmpPL[,'Con.Mult'] <- na.locf(tmpPL[,'Con.Mult'], fromLast=TRUE) # carry NA's backwards too, might cause problems with options contracts that change multiplier
	if(any(naConMult <- is.na(tmpPL[,'Con.Mult'])))  # belt + suspenders?
		tmpPL[naConMult,'Con.Mult'] <- 1
	
	# zerofill Txn.Value, Txn.Fees
	tmpPL[is.na(tmpPL[,'Txn.Value']),'Txn.Value'] <- 0
	tmpPL[is.na(tmpPL[,'Txn.Fees']),'Txn.Fees']  <- 0
	
	# matrix calc Pos.Qty * Price * Con.Mult to get Pos.Value
	tmpPL <- merge(tmpPL, Pos.Value=drop(tmpPL[,'Pos.Qty'] * tmpPL[,'Con.Mult'] * tmpPL[,'Prices']))
	
	LagValue <- lag(tmpPL[,'Pos.Value'])
	LagValue[is.na(LagValue)] <- 0  # needed to avoid a possible NA on the first value that would mess up the Gross.Trading.PL calc
	tmpPL <- merge(tmpPL, Gross.Trading.PL=drop(tmpPL[,'Pos.Value']- LagValue - tmpPL[,'Txn.Value']))
	
	# alternate matrix calc for Realized&Unrealized PL that is only dependent on Txn PL and Gross.Trading.PL
	tmpPL[is.na(tmpPL[,'Net.Txn.Realized.PL']),'Net.Txn.Realized.PL'] <- 0
	tmpPL[is.na(tmpPL[,'Gross.Txn.Realized.PL']),'Gross.Txn.Realized.PL'] <- 0
	
	# matrix calc Period.*.PL, Net.Trading.PL as Gross.Trading.PL + Txn.Fees
	tmpPL <- merge(tmpPL,
		Period.Realized.PL = drop(tmpPL[,'Gross.Txn.Realized.PL']),  # believe it or not, merging is faster than renaming
		Period.Unrealized.PL = drop(round(tmpPL[,'Gross.Trading.PL'] - tmpPL[,'Gross.Txn.Realized.PL'], 2)),
		Net.Trading.PL = drop(tmpPL[,'Gross.Trading.PL'] + tmpPL[,'Txn.Fees']),
		Ccy.Mult = 1)  # Ccy.Mult for this step is always 1
	
	# Ccy.Mult for this step is always 1
	tmpPL[,'Ccy.Mult'] <- 1
	
	# reorder,discard  columns for insert into portfolio object
	tmpPL <- tmpPL[,c('Pos.Qty', 'Con.Mult', 'Ccy.Mult', 'Pos.Value', 'Pos.Avg.Cost', 'Txn.Value',  'Period.Realized.PL', 'Period.Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')]

	# rbind to $posPL slot
	tmpPL <- tmpPL[dateRange] #subset to get rid of any prior period Txn or PosPL rows we inserted
	Portfolio[['symbols']][[Symbol]][['posPL']]<-rbind(Portfolio[['symbols']][[Symbol]][['posPL']],tmpPL)
		

    
  # now do the currency conversions for the whole date range
  TmpPeriods<-Portfolio$symbols[[Symbol]]$posPL[dateRange]
  
	CcyMult = NA 
	FXrate = NA
	invert=FALSE
	if(!is.null(attr(Portfolio,'currency'))) {
		if (tmp_instr$currency==p.ccy.str) {
			CcyMult<-1			
		} else {
			port_currency<-try(getInstrument(p.ccy.str), silent=TRUE)
			if(inherits(port_currency,"try-error") | !is.instrument(port_currency)){
				warning("Currency",p.ccy.str," not found, using currency multiplier of 1")
				CcyMult<-1
			} else { #convert from instr ccy to portfolio ccy
				FXrate.str<-paste(tmp_instr$currency, p.ccy.str, sep='') # currency quote convention is EURUSD which reads as "USD per EUR" or "EUR quoted in USD"
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
            if(ncol(FXrate)>1) CcyMult <- getPrice(FXrate[dateRange],...)
			else CcyMult <- FXrate[dateRange]
			CcyMult <- na.locf(merge(CcyMult,index(TmpPeriods)))
			CcyMult <- CcyMult[index(TmpPeriods)]
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
	
	if (length(CcyMult)==1 && CcyMult==1){
	  Portfolio[['symbols']][[Symbol]][[paste('posPL',p.ccy.str,sep='.')]] <- Portfolio[['symbols']][[Symbol]][['posPL']]
	} else {
	  #multiply the correct columns 
	  columns<-c('Pos.Value', 'Txn.Value', 'Pos.Avg.Cost', 'Period.Realized.PL', 'Period.Unrealized.PL','Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')

      # Find all the closed trades, and the price at close for converting:
      conv0 <- Txns[Txns[,"Gross.Txn.Realized.PL"] != 0, "Txn.Avg.Cost"]

      # This next merge calculates the dates at which to hit CLOSED open trades
      # with a multiplier (CcyMult) ...  But there is a potential problem: Should
      # it be Txn.Avg.Cost later (?).  We need to recheck this code when multiple
      # positions are added.
      conv0 <- merge(conv0, converter = drop(CcyMult), join = "inner")

      # For all trades opened and closed up to the current date, find the PnL in
      # USD at the CLOSE xxxUSD conversion of the bar on which the position was closed.
      if (Txns[1,1] != 0) { # We need at least one transaction, otherwise there will be an error
        if (NROW(conv0) > 0) {
          TmpPeriods <- merge(TmpPeriods, conv0[, "converter"])
        } else {
          TmpPeriods <- merge(TmpPeriods, converter = rep(NA, NROW(TmpPeriods)))
        }
        # This matters for daily bar simulations where Open for entry and Close
        # for exit are used. Check for duplicate dates for trades; these occur
        # for trades opened and closed on the same bar.
        onebartrade_indexes <- which(duplicated(index(TmpPeriods))) # for each i in duplicated, append the previous value.
        if (length(onebartrade_indexes) > 0) {
          for (i in onebartrade_indexes) {
            TmpPeriods[onebartrade_indexes, "converter"] <- TmpPeriods[onebartrade_indexes - 1, "converter"]
          }
        }

        # Insert ability to roll from the end for an open trade.
        openpos <- TmpPeriods[, "Pos.Value"] != 0
        openpos_indexes <- which(openpos == TRUE)
        last_trade_index <- openpos_indexes[length(openpos_indexes)]
        N <- NROW(TmpPeriods)
        if (last_trade_index == N) {
          # A trade is still open, so we must calculate the PnL, then convert at close of final bar...
          TmpPeriods[N, "converter"] <- drop(last(CcyMult))
        }

        TmpPeriods[, "converter"] <- na.locf(TmpPeriods[, "converter"], fromLast = TRUE)
        TmpPeriods[, "Net.Trading.PL"] <- TmpPeriods[, "Net.Trading.PL"] * TmpPeriods[, "converter"]
        TmpPeriods$converter <- NULL
      }
	  
	  #stick it in posPL.ccy
	  Portfolio[['symbols']][[Symbol]][[paste('posPL',p.ccy.str,sep='.')]]<-rbind(Portfolio[['symbols']][[Symbol]][[paste('posPL',p.ccy.str,sep='.')]],TmpPeriods)
	}
  
  #portfolio is already an environment, it's been updated in place
  #assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
}

.parse_interval <- function(interval) {

    # taken/modified from xts:::last.xts
    ip <- gsub("^([[:digit:]]*)([[:alpha:]]+)", "\\1 \\2", interval)
    ip <- strsplit(ip, " ", fixed = TRUE)[[1]]
    if (length(ip) > 2 || length(ip) < 1) 
        stop(paste("incorrectly specified", sQuote("interval")))

    rpu <- ip[length(ip)]
    rpf <- ifelse(length(ip) > 1, as.numeric(ip[1]), 1)
    
    dt.list <- c("milliseconds", "ms", "microseconds", "us", "secs",
      "mins", "hours", "days", "weeks", "months", "quarters", "years")
    dt.ind <- pmatch(rpu, dt.list)
    if(is.na(dt.ind))
        stop("could not uniquely match '", rpu, "' in '", paste0(dt.list,collapse=",'", "'"))
    dt <- dt.list[dt.ind]

    list(on=dt, k=rpf)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2015 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
