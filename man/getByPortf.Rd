\name{getByPortf}
\alias{getByPortf}
\title{ Create a portfolio-by-time table of a portfolio summary attribute }
\description{
    Retrieves calculated attributes for each portfolio in the account from the portfolio summary table.  Assembles into a portfolio-by-time table useful for graphing or calculations.
}
\usage{
getByPortf(Account, Attribute, Date = NULL)
}
\arguments{
  \item{Account}{ An Account object containing Portfolio summaries }
  \item{Attribute}{ Column name to be assembled for each symbol, any of: 'Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees', 'Realized.PL', 'Unrealized.PL', or 'Trading.PL' }
  \item{Date}{ \pkg{xts} date range to return.  If no date is specified, the function returns all available dates }
}
\details{
    Retrieves and formats values calculated by the function \code{\link{updateAcct}}.  The resulting xts object is formatted with the portfolio name as the columnname. 
}
\value{
  An xts time series object containing the requested value for each portfolio in columns. 
}
\author{ Peter Carl }
\seealso{ \code{\link{initAcct}}, \code{\link{updateAcct}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
