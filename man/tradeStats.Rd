\name{tradeStats}
\alias{dailyStats}
\alias{tradeStats}
\title{calculate statistics on transactions and P&L for a symbol or symbols in a portfolio or portfolios}
\usage{
  tradeStats(Portfolios, Symbols)

  dailyStats(Portfolios, use = c("Equity", "Txns"))
}
\arguments{
  \item{Portfolios}{portfolio string}

  \item{Symbols}{character vector of symbol strings,
  default NULL}

  \item{use}{for dailyStats, determines whether numbers are
  calculated from trades or equity curve}
}
\description{
  This function calculates trade-level statistics on a
  symbol or symbols within a portfolio or portfolios.
}
\details{
  Every book on trading, broker report on an analytical
  trading system, or blog post seems to have a slightly
  different idea of what trade statistics are necessary,
  and how they should be displayed.  We choose not to make
  value judgments of this type, aiming rather for
  inclusiveness with post-processing for display.

  The output of this function is a \code{\link{data.frame}}
  with named columns for each statistic. Each row is a
  single portfolio+symbol combination. Values are returned
  in full precision. It is likely that the output of this
  function will have more than you wish to display in all
  conditions, but it should be suitable for reshaping for
  display. Building summary reports from this data.frame
  may be easily accomplished using something like
  \code{textplot} or \code{\link{data.frame}}, with
  rounding, fancy formatting, etc. as your needs dictate.

  If you have additional trade statistics you want added
  here, please share. We find it unlikely that any
  transaction-level statistics that can be calculated
  independently of strategy rules could be considered
  proprietary.

  Special Thanks for contributions to this function from:
  \describe{ \item{Josh Ulrich}{ for adding
  multiple-portfolio support, fixing bugs, and improving
  readability of the code } \item{Klemen Koselj}{ for
  median stats, num trades, and win/loss ratios }
  \item{Mark Knecht}{ for suggesting Profit Factor and
  largest winner/largest loser } }

  WARNING: we're not sure this function is stable/complete
  yet.  If you're using it, please give us feedback!
}
\note{
  TODO document each statistic included in this function,
  with equations

  TODO add more stats, potentially PerformanceAnalytics:
  skewness, kurtosis, upside/downside semidieviation,
  Sharpe/Sortino

  mean absolute deviation stats

  more Tharpe/Kestner/Tradestation stats, e.g. K-factor
  RINA Index Percent time in the market Buy and hold return

  Josh has suggested adding \%-return based stats too
}
\author{
  Lance Levenson
}

