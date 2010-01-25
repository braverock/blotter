buildSpread<- function(spread_id, ..., Dates = NULL, onelot=FALSE) {
    #TODO this currently assumes a two-instrument spread, need to 
    # make it work for arbitrary length ratio spreads
    # this will be done by using the ratio multiplier and currency to 
    # create a factor for each element, and then calculating the price from that
    spread_instr<-try(getInstrument(spread_id))
    if(inherits(spread_instr,"try-error") | !is.instrument(spread_instr)){
        stop(paste("Instrument",spread_instr," not found, please create it first."))
    } 
    if(!inherits(spread_instr,"spread")) stop (paste("Instrument", spread_id, " is not a spread, please use the spread_id of a spread."))
    if(length(spread_instr$memberlist$members)>2) stop("Only two instrument spreads are supported at this time, patches welcome.")
    spread_currency<-spread_instr$currency
    stopifnot(is.currency(spread_currency))

    primary_instr<-getInstrument(spread_instr$memberlist$members[1])
    if(inherits(primary_instr,"try-error") | !is.instrument(primary_instr)){
        stop(paste("Instrument",primary_instr," not found, please create it first."))
    } else {
        primary_currency<-primary_instr$currency
        stopifnot(is.currency(primary_currency))
        primary_mult<-primary_instr$multiplier
        primary_ratio<-spread_instr$memberlist$memberratios[1]
        primary_prices<-get(spread_instr$memberlist$members[1])
    }
    secondary_instr<-getInstrument(spread_instr$memberlist[1])
    if(inherits(secondary_instr,"try-error") | !is.instrument(secondary_instr)){
        stop(paste("Instrument", secondary_instr, " not found, please create it first."))
    } else {
        if(!all.equal(primary_currency,secondary_currency)){
            secondary_currency<-secondary_instr$currency
            stopifnot(is.currency(secondary_currency))
            exchange_rate<-try(get( paste(primary_currency,secondary_currency,sep='')))
            if(inherits(exchange_rate,"try-error")){
                exchange_rate<-try(get( paste(secondary_currency,primary_currency,sep='')))
                if(inherits(exchange_rate,"try-error")){
                    stop(paste("Exchange Rate", paste(primary_currency, secondary_currency, sep=''), "not found."))    
                } else {
                    exchange_rate <- 1/exchange_rate
                }   
            }
        } else {
            #currencies of both instruments are the same
            exchange_rate=1
        }
        secondary_mult<-secondary_instr$multiplier
        secondary_ratio<-spread_instr$memberlist$memberratios[2]
        secondary_prices<-get(spread_instr$memberlist$members[2])
    }
    
    spreadlevel<- (primary_prices*primary_mult*primary_ratio)-(secondary_prices*secondary_mult*secondary_ratio*exchange_rate)
    if(onelot) spreadlevel = spreadlevel/primary_ratio
    if(!all.equal(spread_currency,primary_currency)){
        #convert to the currency of the spread
        spr_exch_rate <- try(get(paste(spread_currency,primary_currency,sep='')))
        if(inherits(spr_exch_rate,"try-error")){
            stop(paste("Exchange Rate", paste(spread_currency, primary_currency,sep=''),"not found."))    
        } else {
            spreadlevel<-spreadlevel*exchange_rate
        }
    }
    return(spreadlevel)
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
