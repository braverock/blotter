#' 
#' @param Portfolio 
#' @param Dates 
#' @returnType 
#' @export
getPortfolio <- function(Portfolio, Dates=NULL) #should symbol subsets be supported too?  probably not.
{ # @author Brian Peterson
    pname<-Portfolio
    if(!grepl("portfolio\\.",pname)) Portfolio<-try(get(paste("portfolio",pname,sep='.'),envir=.blotter))
    else Portfolio<-try(get(pname,envir=.blotter))
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio"))
    if(!inherits(Portfolio,"portfolio")) stop("Portfolio",pname,"passed is not the name of a portfolio object.")
    
    if(!is.null(Dates)){
        message("date subsetting not yet supported")
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
