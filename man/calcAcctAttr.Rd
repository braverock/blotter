\name{calcAcctAttr}
\alias{calcAcctAttr}
\title{ Calculate account attributes }
\description{
  Calculates the net attributes of the portfolios contained within an account
}
\usage{
calcAcctAttr(Account, Attribute, Date = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Account}{ An Account object containing Portfolio summaries }
  \item{Attribute}{ The value to be aggregated from each portfolio, any of: 'Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', or 'Trading.PL' }
  \item{Date}{ Date range to return.  If NULL (default), the function returns all available dates }
}
\details{
  The attributes for the account are defined as:
  \itemize{
    \item{Long.Value}{ The aggregated value of the positions held long in each portfolio.} 
    \item{Short.Value}{ The aggregated value of the positions held short in each portfolio.}
    \item{Net.Value}{ The net value of the positions held in each portfolio, calculated as long value less short value.}
    \item{Gross.Value}{ The gross value of the positions held in each portfolio, calculated as longs plus absolute value of shorts.}
    \item{Txn.Fees}{ The aggregated value of transaction fees for each portfolio.}
    \item{Realized.PL}{ The aggregated value of the realized profit and loss for each portfolio.}
    \item{Unrealized.PL}{ The aggregated value of the unrealized profit and loss for each portfolio.}
    \item{Trading.PL}{ The sum of the realized and unrealized profit and loss for each portfolio.}
  }
}
\value{
  An xts time series with the calculated attribute values
}
\author{ Peter Carl }
\seealso{ \code{\link{getByPortf}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
