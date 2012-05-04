\name{chart.Spread}
\alias{chart.Spread}
\title{Charts the transaction series, positions, and P&L of a spread against prices}
\usage{
  chart.Spread(Account, Portfolio, Spread = NULL,
    Symbols = NULL, Dates = NULL, ...)
}
\arguments{
  \item{Account}{string identifying the account}

  \item{Portfolio}{string identifying the portfolio to
  chart}

  \item{Symbols}{string identifying the underlying symbols
  to chart for positions}

  \item{Spread}{identifier of a spread instrument}

  \item{Dates}{date range, currently not used}

  \item{\dots}{any other passthru parameters (typically
  parameters to \code{chart_Series})}
}
\description{
  Charts the transaction series, positions, and P&L of a
  spread against prices
}

