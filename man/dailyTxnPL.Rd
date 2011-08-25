\name{dailyTxnPL}
\alias{dailyEqPL}
\alias{dailyTxnPL}
\title{generate daily Transaction Realized or Equity Curve P&L by instrument}
\usage{
  dailyTxnPL(Portfolios, Symbols, drop.time = TRUE)

  dailyEqPL(Portfolios, Symbols, drop.time = TRUE)
}
\arguments{
  \item{Portfolios}{portfolio string}

  \item{Symbols}{character vector of symbol strings}

  \item{drop.time}{remove time component of POSIX datestamp
  (if any), default TRUE}
}
\description{
  designed to collate information for high frequency
  portfolios
}
\author{
  Brian G. Peterson
}
\seealso{
  tradeStats
}

