\name{getTxns}
\alias{getTxns}
\title{Retrieves the transactions and their attributes.}
\usage{getTxns(Portfolio, Symbol, Date)}
\description{Retrieves the transactions and their attributes.}
\details{Find and return the transactions and attribute values for the symbol and date in a specied portfolio.

This function provides easy access to the values of transactions contained in a Portfolio object. See initPortf for a detailed description of the structure of a Portfolio object.}
\value{xts of transactions made in the Symbol during the time period given}
\arguments{\item{Portfolio}{a string identifying a portfolio object containing transactions}
\item{Symbol}{an instrument identifier for a symbol included in the portfolio}
\item{Date}{timestamp as of which to get transactions}}
