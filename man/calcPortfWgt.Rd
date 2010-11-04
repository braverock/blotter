\name{calcPortfWgt}
\alias{calcPortfWgt}
\title{Calculates the portfolio weights for positions within a given portfolio.}
\usage{calcPortfWgt(Portfolio, Symbols, Dates, denominator=c("Gross.Value",
    "Net.Value", "Long.Value", "Short.Value"), Account)}
\description{Calculates the portfolio weights for positions within a given portfolio.}
\details{Portfolio weights may be calculated differently depending on their use.}
\value{xts timeseries object with weights by date in rows and symbolname in columns}
\arguments{\item{Portfolio}{a portfolio object structured with initPortf()}
\item{Symbols}{an instrument identifier for a symbol included in the portfolio}
\item{Dates}{dates to return the calculation over formatted as xts range}
\item{denominator}{}
\item{Account}{}}
