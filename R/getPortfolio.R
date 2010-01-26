getPortfolio <- function(Portfolio, Dates=NULL) #should symbol subsets be supported too?  probably not.
{ # @author Brian Peterson
    pname<-Portfolio
    Portfolio<-get(paste("portfolio",pname,sep='.'),envir=.blotter)
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio"))
    
    if(!is.null(Dates)){
        message("date subsettingnot yet supported")
        #TODO add date subsetting in getPortfolio
    }
    
    return(Portfolio)
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
