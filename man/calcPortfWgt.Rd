\name{calcPortfWgt}
\alias{calcPortfWgt}
\title{ calculate portfolio weights for positions }
\description{
  Calculates the relative weights of each position within the portfolio.
}
\usage{
calcPortfWgt(Portfolio, Symbols = NULL, Dates = NULL, denominator = c("Gross.Value", "Net.Value", "Long.Value", "Short.Value"), Account = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Portfolio}{ The Portfolio object for which to calculate the weights. }
  \item{Symbols}{ A subset of symbols to calculate.  Default is NULL, for which it returns weights for all symbols. }
  \item{Dates}{ The ISO 8601 date to calculate the attribute for.  If no date is specified (NULL by default), the function calculates for all available dates }
  \item{denominator}{ the aggregate measure of the portfolio to use as the denominator }
  \item{Account}{ Not implemented. }
}
\details{
  Portfolio weights can be calculated several different ways, and this function accommodates several methods.  The denominator may be specified as:
  \itemize{
    \item{Long.Value}{ The sum of the notional value of all positions held long in the portfolio.}
    \item{Short.Value}{The sum of the notional value of all positions held short in the portfolio.} 
    \item{Net.Value}{The sum of the notional long and notional short value of the portfolio.}
    \item{Gross.Value}{Default. The sum of the notional long and absolute value of the notional short value of the portfolio.}
  }
}
\value{
  Time series object (xts) with weights by date in rows and symbolname in columns.
}
\author{ Peter Carl }
\note{ 
As currently implemented, Portfolio objects do not contain cash proceeds of sales or outflows from purchases.  These weights, then, are approximations relative only to the aggregate positions.
}
\seealso{ \code{\link{calcPortfAttr}}, \code{\link{getBySymbol}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
