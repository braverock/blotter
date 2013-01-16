#' put a portfolio object in .blotter env
#' @param portfolio.st string identifying portfolio
#' @param portfolio portfolio object
#' @param envir the environment to save the portfolio object in, defaults to .blotter
#' @export

put.portfolio <- function(portfolio.st, portfolio, envir=.blotter)
{
    blotter.portfolio.st <- paste('portfolio', portfolio.st, sep='.')

    assign(blotter.portfolio.st, portfolio, envir=envir)
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
