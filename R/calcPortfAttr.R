`calcPortfAttr` <-
function(Portfolio, Attribute, Date=NULL)
{
    symbols = names(Portfolio)
    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Portfolio[[1]]$posPL)
    else
        Date = time(Portfolio[[1]]$posPL[Date])
    table = xts(NULL, order.by=Date) ## Reference time index

    switch(Attribute,
        Trading.PL = {
            table = getBySymbol(Portfolio, 'Trading.PL', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1))))
            colnames(result) = 'Trading.PL'
        },
        Txn.Fees = {
            table = getBySymbol(Portfolio, 'Txn.Fees', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Txn.Fees'
        },
        Realized.PL = {
            table = getBySymbol(Portfolio, 'Realized.PL', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Realized.PL'
        },
        Unrealized.PL = {
            table = getBySymbol(Portfolio, 'Unrealized.PL', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Unrealized.PL'
        },
        Net.Value = {
            table = getBySymbol(Portfolio, 'Pos.Value', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Net.Value'
        },
        Gross.Value = {
            table = getBySymbol(Portfolio, 'Pos.Value', Date)
            result = as.xts(t(t(apply(abs(table), FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Gross.Value'
        },
        Long.Value = {
            table = getBySymbol(Portfolio, 'Pos.Value', Date)
            table = apply(table,MARGIN=c(1,2),FUN=max,0)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Long.Value'
        },
        Short.Value = {
            table = getBySymbol(Portfolio, 'Pos.Value', Date)
            table = apply(table,MARGIN=c(1,2),FUN=min,0)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Short.Value'
        }
    )
    return(result)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
