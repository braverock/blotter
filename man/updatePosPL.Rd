\name{updatePosPL}
\alias{updatePosPL}
\alias{updatePortf}
\title{ calculate position profit or loss }
\description{
  Calculates position PL from the position data and corresponding close price data.
}
\usage{
updatePosPL(Portfolio, Symbol, StartDate, EndDate, Prices = Cl(get(Symbol)))
updatePortf(Portfolio, StartDate, EndDate)
}
\arguments{
  \item{Portfolio}{ A portfolio object structured with \code{\link{initPortf}}. }
  \item{Symbol}{ An instrument identifier for a symbol included in the portfolio. }
  \item{Dates}{ xts date subset or ISO 8601 Dates from which to calculate profit or loss. }
  \item{Prices}{ Close prices in an xts object with a columnname containing a substring "Close". }
}
\details{
  The \code{updatePortf} function goes through each symbol in a portfolio and calculates the PL for each day prices are available.

  \kbd{StartDate} and \kbd{EndDate} must appear in the price stream.
}
\value{
  Returns the revised portfolio object specified in the input.
}
\author{ Peter Carl }
\note{ 
}
\seealso{ \code{\link{initPortf}}, \code{\link{updateAcct}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
