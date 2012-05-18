#' Get an account object from the .blotter environment
#' 
#' Retrieves an account object from the \code{.blotter} environment.  Useful for local examination or charting, or storing interim results for later reference.
#'
#' @param Account string identifier for the account
#' @return Account object
#' @export
getAccount <- function(Account) #should symbol subsets be supported too?  probably not.
{ # @author Brian Peterson
    aname<-Account
    if(!grepl("account\\.",aname)) Account<-try(get(paste("account",aname,sep='.'),envir=.blotter))
    else Account<-try(get(aname,envir=.blotter))
    if(inherits(Account,"try-error"))
        stop(paste("Account ",aname," not found, use initAcct() to create a new account"))
    if(!inherits(Account,"account")) stop("Account ",aname," passed is not the name of an account object.")
    
#     if(!is.null(Dates)){
#         message("date subsetting not yet supported")
#         #TODO add date subsetting in getAccount
#     }
    
    return(Account)
}

#' generic is.function for account, will take either a string or an object
#' 
#' If \code{x} is a string, \code{\link{getAccount}} will be called with 
#' string \code{x} and tested.  Otherwise, the object passed will be tested.
#' 
#' @param x an object or string to be tested as a account
#' @param \dots any other passthru parameters
#' @seealso \code{\link{getAccount}}
#' @export
is.account <- function(x,...) 
{ # @author Brian Peterson
    if(inherits(x,'account')) return(TRUE)
    else if(is.character(x)){
        if(!grepl("account\\.",x)) res <- suppressWarnings(try(get(paste("account",x,sep='.'),envir=.blotter),silent=TRUE))
        else res <- suppressWarnings(try(get(x,envir=.blotter),silent=TRUE))
        #res<-suppressWarnings(try(getaccount(x))) #causes spurious error if you're checking whether account exists
        if(!inherits(res,"account")) {
            message("account ",x," needs to be created first.")
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
