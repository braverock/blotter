\name{getPos}
\alias{getPos}
\title{Retrieves all information about the position as of a date...}
\usage{getPos(Portfolio, Symbol, Date)}
\description{Retrieves all information about the position as of a date}
\details{NOTE This could get much more complicated from here, particularly when it's conditional on symbol, etc.}
\value{All data elements related to position in a row of an xts object}
\arguments{\item{Portfolio}{string identifying a portfolio object containing transactions}
\item{Symbol}{an instrument identifier for a symbol included in the portfolio}
\item{Date}{timestamp as of which to have the most recent position}}
