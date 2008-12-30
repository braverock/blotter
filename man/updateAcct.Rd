\name{updateAcct}
\alias{updateAcct}
\alias{updateEndEq}
\alias{calcEndEq}

\title{ Update equity account attributes and ending account value from underlying portfolios }
\description{
  Calculates the equity account information and component portfolio attributes from the underlying portfolio profits and losses.
}
\usage{
updateAcct(Account, Dates)
updateEndEq(Account, Dates)
calcEndEq(Account, Date)
}
\arguments{
  \item{Account}{ The Account object to update. }
  \item{Dates}{ The ISO 8601 dates to update.  If no date is specified, the function uses all available dates from the first portfolio in the account. }
  \item{Date}{ The ISO 8601 date to update.  No default. }
}
\details{
For each date, the \code{updateAcct} function calculates realized and unrealized P&L based on the performance of each underlying Portfolio, using \code{\link{calcPortfSummary}} (which in turn uses \code{\link{calcPortfAttr}}).  To do that, it first appends the Portfolio summary data to the \kbd{[portfolio name]} slot, then aggregates the portfolio information into the \kbd{TOTAL} slot.  The \code{updateAcct} function does not calculate the final capital amount (ending equity) for the period -- only the Account trading profit or loss for the period.

The \code{updateEndEq} function calculates the final capital amount (ending equity) for the period.  It's a wrapper for \code{calcEndEq} that handles a list of dates.  The \code{calcEndEq} function calculates both \kbd{End.Eq} and \kbd{Net.Performance}.

These functions are separate so that other updates may be made to the Account object before the final equity is calculated, for example, additions and withdrawals, management and incentive fees, interest income, etc.  As a result, these will likely be run sequentially with intermediate functions for adding other cash transactions.

The functions \code{updateEndEq} and \code{calcEndEq} require that \code{updateAcct} has been run and any additional functions have alread appended information into that table (e.g., additions or withdrawals, fees, interest, etc.).  

}
\value{
    \item{\code{updateAcct}}{returns the Account object specified in the inputs with modifications}
    \item{\code{updateEndEq}}{returns the Account object specified in the inputs with modifications}
    \item{\code{calcEndEq}}{returns the Account object specified in the inputs with modifications}

}
\author{ Peter Carl }
\note{  
To see the names of the underlying portfolios, use 'names(Account)[-1]'.  Each of those named slots is used to store calculated portfolio information from the portfolio objects.

The first slot of the account object, labeled 'TOTAL', is used to store aggregated account information.  See \code{\link{initAcct}} for more information.

}
\seealso{ \code{\link{initAcct}}, \code{\link{initPortf}}, \code{\link{updatePortf}} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
