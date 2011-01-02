#' get an account object from the environment for examination or manipulation
#' @param Account string identifier for the account
#' @param Dates 
#' @return Account object
#' @export
getAccount <- function(Account, Dates=NULL) #should symbol subsets be supported too?  probably not.
{ # @author Brian Peterson
    aname<-Account
    if(!grepl("account\\.",aname)) Account<-try(get(paste("account",aname,sep='.'),envir=.blotter))
    else Account<-try(get(aname,envir=.blotter))
    if(inherits(Account,"try-error"))
        stop(paste("Account ",aname," not found, use initAcct() to create a new account"))
    if(!inherits(Account,"account")) stop("Account ",aname," passed is not the name of an account object.")
    
    if(!is.null(Dates)){
        message("date subsetting not yet supported")
        #TODO add date subsetting in getAccount
    }
    
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
