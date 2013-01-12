\name{tradeStats}
\alias{dailyStats}
\alias{tradeStats}
\title{calculate statistics on transactions and P&L for a symbol or symbols in a portfolio or portfolios}
\usage{
  tradeStats(Portfolios, Symbols,
    use = c("txns", "trades"))

  dailyStats(Portfolios, use = c("equity", "txns"))
}
\arguments{
  \item{Portfolios}{portfolio string}

  \item{Symbols}{character vector of symbol strings,
  default NULL}

  \item{use}{for determines whether numbers are calculated
  from transactions or round-trip trades (for tradeStats)
  or equity curve (for dailyStats)}
}
\value{
  a \code{data.frame} containing:

  \describe{ \item{Portfolio}{ name of the portfolio}
  \item{Symbol}{ symbol name } \item{Num.Txns}{ number of
  transactions produced by \code{\link{addTxn}} }
  \item{Num.Trades}{ number of \emph{flat to flat} trades
  performed } \item{Net.Trading.PL}{ } \item{Avg.Trade.PL}{
  mean trading P&L per trade } \item{Med.Trade.PL}{ median
  trading P&L per trade} \item{Largest.Winner}{ largest
  winning trade } \item{Largest.Loser}{ largest losing
  trade } \item{Gross.Profits}{ gross (pre-fee) trade
  profits } \item{Gross.Losses}{ gross trade losses }
  \item{Std.Dev.Trade.PL}{ standard deviation of trade P&L
  } \item{Percent.Positive}{ percent of trades that end
  positive } \item{Percent.Negative}{ percent of trades
  that end negative } \item{Profit.Factor}{ absolute value
  ration of gross profits over gross losses }
  \item{Avg.Win.Trade}{ mean P&L of profitabloe trades }
  \item{Med.Win.Trade}{ median P&L of profitable trades }
  \item{Avg.Losing.Trade}{ mean P&L of losing trades }
  \item{Med.Losing.Trade}{ median P&L of losing trades }
  \item{Avg.Daily.PL}{mean daily realized P&L on days there
  were transactions, see \code{\link{dailyStats}} for all
  days } \item{Med.Daily.PL}{ median daily P&L }
  \item{Std.Dev.Daily.PL}{ standard deviation of daily P&L
  } \item{Max.Drawdown}{ max drawdown }
  \item{Avg.WinLoss.Ratio}{ ratio of mean winning over mean
  losing trade } \item{Med.WinLoss.Ratio}{ ratio of median
  winning trade over mean losing trade } \item{Max.Equity}{
  maximum account equity } \item{Min.Equity}{ minimum
  account equity } }
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
  Lance Levenson, Brian Peterson
}
\seealso{
  \code{\link{chart.ME}} for a chart of MAE and MFE derived
  from trades, and \code{\link{perTradeStats}} for detailed
  statistics on a per-trade basis
}

