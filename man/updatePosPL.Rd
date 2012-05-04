\name{.updatePosPL}
\alias{.updatePosPL}
\title{Calculates position PL from the position data and corresponding close price data.}
\usage{
  .updatePosPL(Portfolio, Symbol, Dates = NULL,
    Prices = NULL, ConMult = NULL, ...)
}
\arguments{
  \item{Portfolio}{a portfolio name to a portfolio
  structured with initPortf()}

  \item{Symbol}{an instrument identifier for a symbol
  included in the portfolio}

  \item{Dates}{xts subset of dates, e.g.,
  "2007-01::2008-04-15". These dates must appear in the
  price stream}

  \item{Prices}{periodic prices in an xts object with a
  columnname compatible with \code{getPrice}}

  \item{ConMult}{if necessary, numeric contract multiplier,
  not needed if instrument is defined.}

  \item{\dots}{any other passthru parameters}
}
\value{
  Regular time series of position information and PL
}
\description{
  Calculates position PL from the position data and
  corresponding close price data.
}
\author{
  Peter Carl, Brian Peterson
}

