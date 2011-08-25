#' initialize the summary table used in portfolio and account lists
#' @param initDate date prior to the first close price given, used to contain initial account equity and initial position
#' @param \dots any other passthrough parameters  
#' @rdname initSummary
.initSummary <- function(initDate="1950-01-01",...)
{ # @author Brian Peterson
    summary <- xts( as.matrix(t(rep(0,9))), order.by=as.POSIXct(initDate,...=...), ...=... )
    colnames(summary) <- c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Realized.PL', 'Unrealized.PL', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL')
    class(summary)<-c("portfolio_summary",class(summary))
    return(summary)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2011 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: initTxn.R 306 2010-03-26 21:19:51Z peter_carl $
#
###############################################################################
