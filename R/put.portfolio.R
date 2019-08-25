#' put a portfolio object in .blotter env
#' @param portfolio.st string identifying portfolio
#' @param portfolio portfolio object
#' @param envir the environment to save the portfolio object in, defaults to .blotter
#' @export

put.portfolio <- function(portfolio.st, portfolio, envir=.blotter)
{
    blotter.portfolio.st <- paste('portfolio', portfolio.st, sep='.')
    if (is.list(portfolio)) {
        # convert list to environment, and attach attributes
        p.arg <- portfolio
        p.attr <- attributes(p.arg)
        p.attr <- p.attr[!(names(p.attr) %in% "names")]
        portfolio <- as.environment(p.arg)
        portfolio$symbols <- list2env(portfolio$symbols,hash=TRUE)
        for(instrument in ls(portfolio$symbols)) portfolio$symbols[[instrument]] <- list2env(portfolio$symbols[[instrument]])
        attributes(portfolio) <- p.attr
    }
    assign(blotter.portfolio.st, portfolio, envir=envir)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2015 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
