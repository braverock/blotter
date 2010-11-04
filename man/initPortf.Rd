\name{initPortf}
\alias{initPortf}
\title{Initializes a portfolio object,...}
\usage{initPortf(name="default", symbols, initPosQty=0, initDate="1950-01-01",
    currency="USD")}
\description{Initializes a portfolio object,}
\details{Initializes a portfolio object, which is constructed from is constructed of the following:
$symbols: the identifier used for each instrument contained in the portfolio. Use names(Portfolio$symbols) to get a list of symbols.
$[symbol]$txn: irregular xts object of transactions data 
$[symbol]$posPL: regular xts object of positions P&L calculated from transactions
$[symbol]$posPL.ccy: regular xts object of positions P&L converted to portfolio currency
$summary: summary slot

TODO: add $account: name of the (one) affiliated account
Outputs
Initialized portfolio structure with a start date and initial positions.}
\author{Peter Carl}
\arguments{\item{name}{}
\item{symbols}{instrument identifiers of those instruments contained in the portfolio}
\item{initPosQty}{initial position, default is zero}
\item{initDate}{date prior to the first close price given, used to contain initial account equity and initial position}
\item{currency}{ISO currency idenifier used to locate the portfolio currency}}
