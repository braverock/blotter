\name{chart.Reconcile}
\alias{chart.Reconcile}
\title{Chart trades against market data, position through time, and cumulative P\&L...}
\usage{chart.Reconcile(theoPort, actualPort, Symbol, Dates, ...)}
\description{Chart trades against market data, position through time, and cumulative P\&L}
\details{Produces a three-panel chart of time series charts that contains prices and transactions in the top panel, the resulting position in the second, and a cumulative profit-loss line chart in the third.}
\seealso{\code{\link{chart.Posn}}}
\note{Expect changes to this function, since the underlying charts are experimental functions in quantmod.}
\arguments{\item{theoPort}{string identifying the theoretical portfolio to chart}
\item{actualPort}{string identifying the actual portfolio to chart}
\item{Symbol}{string identifying the symbol to chart}
\item{Dates}{currently not used}
\item{\dots}{any other passthru parameters to \code{\link[quantmod]{chart_Series}}}}
