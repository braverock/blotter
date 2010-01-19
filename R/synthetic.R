buildSpread<- function(primary_id, ..., Dates = NULL, onelot=FALSE) {
    tmp_instr<-try(getInstrument(primary_id))
    if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
        stop(paste("Instrument",tmp_instr," not found, please create it first."))
    } 
    if(!inherits(tmp_instr,"spread")) stop (paste("Instrument",primary_id," is not a spread, please use the primary_id of a spread."))

    primary_instr<-getInstrument(tmp_instr$memberlist$members[1])
    if(inherits(primary_instr,"try-error") | !is.instrument(primary_instr)){
        stop(paste("Instrument",primary_instr," not found, please create it first."))
    } else {
        primary_currency<-primary_instr$currency
        stopifnot(is.currency(primary_currency))
        primary_mult<-primary_instr$multiplier
        primary_ratio<-tmp_instr$memberlist$memberratios[1]
    }
    secondary_instr<-getInstrument(tmp_instr$memberlist[1])
    if(inherits(secondary_instr,"try-error") | !is.instrument(secondary_instr)){
        stop(paste("Instrument",secondary_instr," not found, please create it first."))
    } else {
        secondary_currency<-secondary_instr$currency
        stopifnot(is.currency(secondary_currency))
        exchange_rate<-try(get(paste(primary_currency,secondary_currency,sep='')))
        if(inherits(exchange_rate,"try-error")){
            exchange_rate<-try(get(paste(secondary_currency,primary_currency,sep='')))
            if(inherits(exchange_rate,"try-error")){
                stop(paste("Exchange Rate",paste(primary_currency,secondary_currency,sep=''),"not found."))    
            } else {
                exchange_rate <- 1/exchange_rate
            }   
        }
        secondary_mult<-secondary_instr$multiplier
        secondary_ratio<-tmp_instr$memberlist$memberratios[2]
    }
    
    spreadlevel<- (primary*primary_mult*primary_ratio)-(secondary*secondary_mult*secondary_ratio*exchange_rate)
    if(onelot) spreadlevel = spreadlevel/primary_ratio
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
