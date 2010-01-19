buildSpread(primary_id, ..., ) {
    tmp_instr<-try(getInstrument(primary_id)
    if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
        stop(paste("Instrument",Symbol," not found, please create it first.))
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
