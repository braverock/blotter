calcPortfAttr <- function(Portfolio, Attribute, Dates=NULL, Symbols = NULL)
{
    if(!inherits(Portfolio,"portfolio")) stop("Portfolio passed is not a portfolio object.")
    symbols = names(Portfolio$symbols)
    if(is.null(Dates)|is.na(Dates)) # if no date is specified, get all available dates
        Dates = time(Portfolio$symbols[[1]]$posPL)
#    else Dates = time(Portfolio$symbols[[1]]$posPL[Dates])

    switch(Attribute,
        Gross.Trading.PL = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Gross.Trading.PL', Dates = Dates, Symbols = Symbols)
#            result = xts(apply(table, FUN='sum', MARGIN=1), Dates)
            result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Gross.Trading.PL'
        },
        Txn.Fees = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Txn.Fees', Dates = Dates, Symbols = Symbols)
            result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Txn.Fees'
        },
        Net.Trading.PL = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Net.Trading.PL', Dates = Dates, Symbols = Symbols)
#            result = xts(apply(table, FUN='sum', MARGIN=1), Dates)
            result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Net.Trading.PL'
        },
        Realized.PL = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Realized.PL', Dates = Dates, Symbols = Symbols)
            result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Realized.PL'
        },
        Unrealized.PL = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Unrealized.PL', Dates = Dates, Symbols = Symbols)
            result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Unrealized.PL'
        },
        Net.Value = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Pos.Value', Dates = Dates, Symbols = Symbols)
            result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Net.Value'
        },
        Gross.Value = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Pos.Value', Dates = Dates, Symbols = Symbols)
            result = xts(rowSums(abs(table), na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Gross.Value'
        },
        Long.Value = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Pos.Value', Dates = Dates, Symbols = Symbols)
            tmat = apply(table,MARGIN=c(1,2),FUN=max,0)# comes out a matrix
            result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Long.Value'
        },
        Short.Value = {
            table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Pos.Value', Dates = Dates, Symbols = Symbols)
            tmat = apply(table,MARGIN=c(1,2),FUN=min,0) # comes out a matrix
            result = xts(rowSums(tmat, na.rm=TRUE), order.by=index(table))
            colnames(result) = 'Short.Value'
        }
    )
    return(result)
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
