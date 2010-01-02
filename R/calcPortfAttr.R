`calcPortfAttr` <-
function(Portfolio, Attribute, Date=NULL, Symbols = NULL)
{
    symbols = names(Portfolio)
    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Portfolio[[1]]$posPL)
    else
        Date = time(Portfolio[[1]]$posPL[Date])
    table = xts(NULL, order.by=Date) ## Reference time index

    switch(Attribute,
        Trading.PL = {
            table = getBySymbol(Portfolio = Portfolio, Attribute = 'Trading.PL', Date = Date, Symbols = Symbols)
            result = xts(apply(table, FUN='sum', MARGIN=1), Date)
            colnames(result) = 'Trading.PL'
        },
        Txn.Fees = {
            table = getBySymbol(Portfolio = Portfolio, Attribute = 'Txn.Fees', Date = Date, Symbols = Symbols)
            result = xts(rowSums(table), Date)
            colnames(result) = 'Txn.Fees'
        },
        Realized.PL = {
            table = getBySymbol(Portfolio = Portfolio, Attribute = 'Realized.PL', Date = Date, Symbols = Symbols)
            result = xts(rowSums(table), Date)
            colnames(result) = 'Realized.PL'
        },
        Unrealized.PL = {
            table = getBySymbol(Portfolio = Portfolio, Attribute = 'Unrealized.PL', Date = Date, Symbols = Symbols)
            result = xts(rowSums(table), Date)
            colnames(result) = 'Unrealized.PL'
        },
        Net.Value = {
            table = getBySymbol(Portfolio = Portfolio, Attribute = 'Pos.Value', Date = Date, Symbols = Symbols)
            result = xts(rowSums(table), Date)
            colnames(result) = 'Net.Value'
        },
        Gross.Value = {
            table = getBySymbol(Portfolio = Portfolio, Attribute = 'Pos.Value', Date = Date, Symbols = Symbols)
            result = xts(rowSums(abs(table)), Date)
            colnames(result) = 'Gross.Value'
        },
        Long.Value = {
            table = getBySymbol(Portfolio = Portfolio, Attribute = 'Pos.Value', Date = Date, Symbols = Symbols)
            table = apply(table,MARGIN=c(1,2),FUN=max,0)
            result = xts(rowSums(table), Date)
            colnames(result) = 'Long.Value'
        },
        Short.Value = {
            table = getBySymbol(Portfolio = Portfolio, Attribute = 'Pos.Value', Date = Date, Symbols = Symbols)
            table = apply(table,MARGIN=c(1,2),FUN=min,0) # comes out a matrix
            result = xts(rowSums(table), Date)
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
