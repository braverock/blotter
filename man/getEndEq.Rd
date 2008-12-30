\name{getEndEq}
\alias{getEndEq}
\title{ Retrieves the most recent value of the capital account }
\description{
  Retrieves the most recent value of \emph{Ending Equity} from the Account object.  If the date does not exactly match a date in the Account object, it will return the closest prior value.
}
\usage{
getEndEq(Account, Date)
}
\arguments{
  \item{Account}{ An Account object containing Portfolio summaries }
  \item{Date}{ an ISO 8601 date, such as '2008-01-05' }
}
\details{
    Retrieves the value from the \code{TOTAL} slot of the Account object. Use \code{[Account object name]$TOTAL} to review the xts table of calculated data.
}
\value{
    Numeric value of the equity account for the date given
}
\author{ Peter Carl }
\seealso{ \code{\link{initAcct}}, \code{\link{updateAcct}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
