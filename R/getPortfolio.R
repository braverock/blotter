#' get a portfolio object
#' 
#' Get a portfolio object conssting of either a nested list (\code{getPortfolio}) 
#' or a pointer to the portfolio in the \code{.blotter} environment (\code{.getPortfolio})
#' 
#' Portfolios in blotter are stored as a set of nested, hashed, environments.
#' 
#' The \code{getPortfolio} function returns a nested list.  If you are unsure, use this function.
#' 
#' The \code{.getPortfolio} function returns a pointer to the actual environment.  
#' Environments in R are passed by reference, and are not copied by the \code{<-} 
#' assignment operator.  Any changes made to the environment returned by 
#' \code{.getPortfolio} are global.  You have been warned.
#'  
#' @param Portfolio string identifying portfolio
#' @param Dates dates subset, not yet supported
#' @param envir the environment to retrieve the portfolio from, defaults to .blotter
#' 
#' @seealso \code{\link{initPortf}}, \code{\link{updatePortf}}
#' @export getPortfolio
#' @export .getPortfolio
getPortfolio <- function(Portfolio, Dates=NULL, envir=.blotter) 
{ 
  pname<-Portfolio
  oport<- .getPortfolio(Portfolio, envir=envir)
  if(is.environment(oport))
      port <- as.list.environment(oport)
  else
      port <- oport
  port$symbols <- lapply(oport$symbols, function(s) if(is.environment(s)) as.list.environment(s) else s)

  class(port) <- class(oport)
  attr(port, "currency") <- attr(oport, "currency")
  attr(port, "initDate") <- attr(oport, "initDate")
  
  if(!is.null(Dates)){
    message("date subsetting not yet supported")
    #TODO add date subsetting in getPortfolio
  }
  
  return(port)
}  

#' @rdname getPortfolio
.getPortfolio <- function(Portfolio, envir=.blotter) 
{ # @author Brian Peterson
    pname<-Portfolio
    if(!grepl("portfolio\\.",pname)) Portfolio<-suppressWarnings(try(get(paste("portfolio",pname,sep='.'),envir=envir),silent=TRUE))
    else Portfolio<-suppressWarnings(try(get(pname,envir=envir),silent=TRUE))
    if(inherits(Portfolio,"try-error"))
        stop(paste("Portfolio",pname," not found, use initPortf() to create a new portfolio"))
    if(!inherits(Portfolio,"portfolio")) stop("Portfolio",pname,"passed is not the name of a portfolio object.")  
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
