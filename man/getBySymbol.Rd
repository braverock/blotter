\name{getBySymbol}
\alias{getBySymbol}
\title{ Create a symbol-by-time table of a portfolio attribute }
\description{
Retrieves calculated attributes for each position in the portfolio from the \kbd{posPL} table.  Assembles the results into a symbol-by-time table useful for graphing or calculations.
}
\usage{
getBySymbol(Portfolio, Attribute, Date = NULL)
}
\arguments{
  \item{Portfolio}{ A Portfolio object containing transactions }
  \item{Attribute}{ Column name to be assembled for each symbol in the Portfolio, any of: 'Pos.Qty', 'Pos.Value', 'Txn.Value', 'Realized.PL', 'Unrealized.PL', or 'Trading.PL' }
  \item{Date}{ \pkg{xts} date range to return.  If no date is specified, the function returns all available dates }
}
\details{
    Retrieves and formats values calculated by the function \code{\link{updatePosPL}}.  The resulting xts object is formatted with the symbol as the columnname. 
}
\value{
  An xts time series object containing the requested value for  each symbol in columns. 
}
\author{ Peter Carl }

\seealso{ \code{\link{initPortf}}, \code{\link{updatePosPL}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
