\name{tradeQuantiles}
\alias{tradeQuantiles}
\title{quantiles of per-trade stats}
\usage{
  tradeQuantiles(Portfolio, Symbol, ...,
    scale = c("cash", "percent", "tick"),
    probs = c(0.5, 0.75, 0.9, 0.95, 0.99, 1))
}
\arguments{
  \item{Portfolio}{string identifying the portfolio}

  \item{Symbol}{string identifying the symbol to examin
  trades for. If missing, the first symbol found in the
  \code{Portfolio} portfolio will be used}

  \item{\dots}{any other passthrough parameters}

  \item{scale}{string specifying 'cash', or 'percent' for
  percentage of investment, or 'tick'}

  \item{probs}{vector of probabilities for \code{quantile}}
}
\description{
  quantiles of per-trade stats
}
\author{
  Brian G. Peterson
}
\references{
  Tomasini, E. and Jaekle, U. \emph{Trading Systems - A new
  approach to system development and portfolio
  optimisation} (ISBN 978-1-905641-79-6)
}

