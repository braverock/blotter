\name{getTxns}
\alias{getTxns}
\title{Retrieve transactions and their attributes.}
\usage{getTxns(Portfolio, Symbol, Dates)}
\description{Retrieve transactions and their attributes.}
\details{Find and return the transactions and attribute values for the symbol and date in a specied portfolio.

This function provides easy access to the values of transactions contained in a Portfolio object. See \code{\link{initPortf}} for a detailed description of the structure of a Portfolio object.}
\value{xts time series of transactions made in the Symbol during the time period given}
\seealso{\code{\link{initPortf}}}
\arguments{\item{Portfolio}{a string identifying a portfolio object containing transactions}
\item{Symbol}{an instrument identifier for a symbol included in the portfolio}
\item{Dates}{specified as an ISO 8601 date or an xts date range, such as "2007-01::2008-04-15"}}
