\name{chart.Posn}
\alias{chart.Posn}
\title{Chart trades against market data, position through time, and cumulative P\&L...}
\usage{chart.Posn(Portfolio, Symbol, Dates, ...)}
\description{Chart trades against market data, position through time, and cumulative P\&L}
\details{Produces a three-panel chart of time series charts that contains prices and transactions in the top panel, the resulting position in the second, and a cumulative profit-loss line chart in the third.}
\note{Expect changes to this function, since the underlying charts are experimental functions in quantmod.}
\arguments{\item{Portfolio}{string identifying the portfolio to chart}
\item{Symbol}{string identifying the symbol to chart}
\item{Dates}{xts ISO 8601 style subsetting}
\item{\dots}{any other passthru parameters to \code{\link[quantmod]{chart_Series}}}}
