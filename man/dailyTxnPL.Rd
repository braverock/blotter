\name{dailyTxnPL}
\title{generate daily Transaction Realized or Equity Curve P&L by instrument...}
\usage{dailyTxnPL(Portfolios, Symbols, drop.time=TRUE)}
\description{generate daily Transaction Realized or Equity Curve P&L by instrument}
\details{designed to collate information for high frequency portfolios}
\alias{dailyEqPL}
\author{Brian G. Peterson}
\arguments{\item{Portfolios}{portfolio string}
\item{Symbols}{character vector of symbol strings}
\item{drop.time}{remove time component of POSIX datestamp (if any), default TRUE}}
