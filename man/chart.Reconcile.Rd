\name{chart.Reconcile}
\alias{chart.Reconcile}
\title{Chart trades against market data, position through time, and cumulative P\&L...}
\usage{chart.Reconcile(theoPort, actualPort, Symbol, Dates, ...)}
\description{Chart trades against market data, position through time, and cumulative P\&L}
\details{Produces a four-panel chart of time series charts that contains prices and transactions in the top panel, the resulting position in the second, a cumulative profit-loss line chart in the third.  
The theoretical trades, positions, and P&L are plotted first, in the 'light' versions of the colors, and then the actual values are overplotted in the main color.  
If they agree completely, the theoretical values will not be visible.  Differences will make themselves visible by misalignment of the symbols or lines. 
The fourth panel is the difference in P&L between the theoretical and actual values, and could be considered 'slippage', which could be positive or negative.  
It is calculated by subtracting the theoretical P&L from the actual P&L.}
\seealso{\code{\link{chart.Posn}}}
\note{Expect changes to this function, since the underlying charts are experimental functions in quantmod.}
\arguments{\item{theoPort}{string identifying the theoretical portfolio to chart}
\item{actualPort}{string identifying the actual portfolio to chart}
\item{Symbol}{string identifying the symbol to chart}
\item{Dates}{currently not used}
\item{\dots}{any other passthru parameters to \code{\link[quantmod]{chart_Series}}}}
