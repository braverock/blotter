`getEndEq` <-
function(Account, Date)
{ # @author Peter Carl

    # DESCRIPTION:
    # Retrieves the most recent value of the capital account

    # Inputs
    # Date: most recent date of last calculated equity
    # AcctData: location of the ACCOUNT data

    # Outputs
    # Numeric value of the equity account
     
    # FUNCTION
    toDate = paste('::', Date, sep="")
    EndEq = as.numeric(tail(Account[[1]][toDate,], n=1)[,"End.Eq"])
    return(EndEq)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
