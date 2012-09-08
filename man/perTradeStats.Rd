\name{perTradeStats}
\alias{perTradeStats}
\title{calculate flat to flat per-trade statistics}
\usage{
  perTradeStats(Portfolio, Symbol, ...)
}
\arguments{
  \item{Portfolio}{string identifying the portfolio to
  chart}

  \item{Symbol}{string identifying the symbol to chart. If
  missing, the first symbol found in the \code{Portfolio}
  portfolio will be used}

  \item{\dots}{any other passthrough parameters}
}
\description{
  One 'trade' is defined as the entire time the symbol is
  not flat. It may contain many transactions.  From the
  initial transaction that moves the position away from
  zero to the last transaction that flattens the position
  is all one 'trade' for the purposes of this function.
}
\details{
  This is sometimes referred to as 'flat to flat' analysis.

  Note that a trade that is open at the end of the measured
  period will be marked to the timestamp of the end of the
  series. If that trade is later closed, the stats for it
  will likely change.
}
\author{
  Brian G. Peterson, Jan Hume
}
\references{
  Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new
  approach to system development and portfolio
  optimisation} (ISBN 978-1-905641-79-6)
}

