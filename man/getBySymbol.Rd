\name{.getBySymbol}
\alias{.getBySymbol}
\title{Retrieves calculated attributes for each position in the portfolio...}
\usage{.getBySymbol(Portfolio, Attribute, Dates, Symbols, native=FALSE)}
\description{Retrieves calculated attributes for each position in the portfolio}
\details{Retrieves calculated attributes for each position in the portfolio
from the posPL table.  Assembles into a symbol-by-time table useful
for graphing or calculations

items typically include things like
'Pos.Qty', 'Pos.Value', 'Txn.Value', 'Realized.PL', 'Unrealized.PL',or 'Trading.PL'}
\value{regular xts object of values by symbol}
\arguments{\item{Portfolio}{a portfolio object containing transactions}
\item{Attribute}{column name to be assembled for each symbol}
\item{Dates}{}
\item{Symbols}{}
\item{native}{}}
