\name{updatePortf}
\alias{updatePortf}
\title{Function goes through each symbol and calculates the PL for each period prices are available...}
\usage{updatePortf(Portfolio, Symbols, Dates, Prices, ...)}
\description{Function goes through each symbol and calculates the PL for each period prices are available}
\details{Inputs
Portfolio: a portfolio object containing transactions
Symbol: an instrument identifier for a symbol included in the portfolio
Dates: Dates for which to calculate equity account
These dates must appear in the price stream

Outputs
assigns position information and PL into the environment}
\arguments{\item{Portfolio}{}
\item{Symbols}{}
\item{Dates}{}
\item{Prices}{}
\item{dots}{any other passthrough parameters}}
