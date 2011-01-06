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
