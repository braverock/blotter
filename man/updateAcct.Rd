\name{updateAcct}
\alias{updateAcct}
\alias{updateEndEq}

\title{ calculate equity account attributes }
\description{
  Calculates the equity account information from the underlying portfolio profits and losses.
}
\usage{
updateAcct(Account, Dates)
updateEndEq(Account, Dates)
}
\arguments{
  \item{Account}{ The account object to update. }
  \item{Dates}{ The ISO 8601 dates to update.  If no date is specified, the function uses all available dates from the first portfolio in the account. }
}
\details{
For each date, the updateAcct function calculates realized and unrealized P&L based on the performance of each underlying Portfolio.  To do that, it first appends the portfolio summary data to the portfolio slot, then aggregates the portfolio information into the TOTAL slot.  The updateAcct function does not calculate the final capital amount (ending equity) for the period -- only the profit or loss for the period.

The updateEndEq function calculates the final capital amount (ending equity) for the period.

These functions are separate so that other updates may be made to the Account object before the final equity is calculated, for example, additions and withdrawals, management and incentive fees, interest income, etc.  As a result, these will likely be run sequentially with intermediate functions for adding other cash transactions.
}
\value{
  Returns the Account object specified in the inputs with modifications.
}
\author{ Peter Carl }
\note{  
To see the names of the underlying portfolios, use 'names(Account)[-1]'.  Each of those named slots is used to store calculated portfolio information from the portfolio objects.

The first slot of the account object, labeled 'TOTAL', is used to store aggregated account information.  See \code{\link{initAccount}} for more information.

}
\seealso{ \code{\link{initAcct}}, \code{\link{initPortf}}, \code{\link{updatePortf}} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
