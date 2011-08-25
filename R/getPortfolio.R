#' get a portfolio object
#' @param Portfolio string identifying portfolio
#' @param Dates dates subset, not yet supported
#' @export
getPortfolio <- function(Portfolio, Dates=NULL) #should symbol subsets be supported too?  probably not.
{ # @author Brian Peterson
    pname<-Portfolio
    if(!grepl("portfolio\\.",pname)) Portfolio<-suppressWarnings(try(get(paste("portfolio",pname,sep='.'),envir=.blotter),silent=TRUE))
    else Portfolio<-suppressWarnings(try(get(pname,envir=.blotter),silent=TRUE))
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio"))
    if(!inherits(Portfolio,"portfolio")) stop("Portfolio",pname,"passed is not the name of a portfolio object.")
    
    if(!is.null(Dates)){
        message("date subsetting not yet supported")
        #TODO add date subsetting in getPortfolio
    }
    
    return(Portfolio)
}


#' generic is.function for portfolio, will take either a string or an object
#' 
#' If \code{x} is a string, \code{\link{getPortfolio}} will be called with 
#' string \code{x} and tested.  Otherwise, the object passed will be tested.
#' 
#' @param x an object or string to be tested as a portfolio
#' @param \dots any other passthru parameters
#' @seealso \code{\link{getPortfolio}}
#' @export
is.portfolio <- function(x,...) 
{ # @author Brian Peterson
    if(inherits(x,'portfolio')) return(TRUE)
    else if(is.character(x)){
        if(!grepl("portfolio\\.",x)) res <- suppressWarnings(try(get(paste("portfolio",x,sep='.'),envir=.blotter),silent=TRUE))
        else res <- suppressWarnings(try(get(x,envir=.blotter),silent=TRUE))
        #res<-suppressWarnings(try(getPortfolio(x))) #causes spurious error if you're checking whether portfolio exists
        if(!inherits(res,"portfolio")) {
            message("Portfolio",x,"needs to be created first.")
            return(FALSE)
        } else {
            return(TRUE)
        }
    } else return(FALSE)    
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
