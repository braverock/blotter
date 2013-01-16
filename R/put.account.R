#' put a account object in .blotter env
#' @param account.st string identifying account
#' @param account account object
#' @param envir the environment to save the account object in, defaults to .blotter
#' @export

put.account <- function(account.st, account, envir=.blotter)
{
    blotter.account.st <- paste('account', account.st, sep='.')

    assign(blotter.account.st, account, envir=envir)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2011 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: getPortfolio.R 742 2011-08-25 21:12:43Z braverock $
#
###############################################################################
