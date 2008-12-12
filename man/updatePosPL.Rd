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
  \item{StartDate}{ ISO 8601 Date from which to calculate profit or loss. }
  \item{EndDate}{ ISO 8601 Date to which to calculate profit or loss. }
  \item{Prices}{ Close prices in an xts object with a columnname containing a substring "Close". }
}
\details{
  The 'updatePortf' function goes through each symbol in a portfolio and calculates the PL for each day prices are available.

  StartDate and EndDate must appear in the price stream.
}
\value{
  Returns the revised portfolio object specified in the input.
}
\references{ ~put references to the literature/web site here ~ }
\author{ Peter Carl }
\note{ 
}
\seealso{ \code{\link{initPortf}}, \code{\link{updateAcct}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
