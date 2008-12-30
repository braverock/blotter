\name{calcPortfAttr}
\alias{calcPortfAttr}
\alias{calcPortfSummary}
\title{ Calculate summary information for a Portfolio }
\description{
    Calculates the attributes of a portfolio from its positions.
}
\usage{
calcPortfAttr(Portfolio, Attribute, Date = NULL)
calcPortfSummary(Portfolio, Date = NULL)
}
\arguments{
  \item{Portfolio}{ The Portfolio object for which to calculate the attribute. }
  \item{Attribute}{ The attribute of the Portfolio object to calculate. See detail below.}
  \item{Date}{ The ISO 8601 date to calculate the attribute for.  If no date is specified (NULL by default), the function calculates for all available dates }
}
\details{
Attribute can be any of the following:
  \item{Long.Value}{ The sum of the notional value of all positions held long in the portfolio.}
  \item{Short.Value}{The sum of the notional value of all positions held short in the portfolio.} 
  \item{Net.Value}{The sum of the notional long and notional short value of the portfolio.}
  \item{Gross.Value}{The sum of the notional long and absolute value of the notional short value of the portfolio.}
  \item{Txn.Fees}{The sum of brokerage commissions, exchange and other brokerage fees paid by the portfolio during the period.}
  \item{Realized.PL}{The sum of net realized profits or losses aggregated from the underlying positions in the portfolio.  Gross realized profits can be calculated by adding 'Txn.Fees', the brokerage commission expenses for the period.}
  \item{Unrealized.PL}{The sum total increase or decrease in unrealized profits or losses on open positions in the portfolio at the end of the period.}
  \item{Trading.PL}{Net realized profit or loss plus interest income plus change in unrealized profit or loss across all positions in the portfolio.}

The function \command{calcPortfSummary} returns all of the columns in an xts object.
}
\value{
  An xts time series with the calculated attribute values
}
\author{ Peter Carl }
\seealso{ \code{\link{getBySymbol}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
