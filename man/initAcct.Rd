\name{initAcct}
\alias{initAcct}
\title{ create a capital account }
\description{
Initializes an account object, which contains portfolios and tracks capital invested.
}
\usage{
initAcct(portfolios, initDate = "1950-01-01", initEq = 1e+05)
}
\arguments{
  \item{portfolios}{ a list of portfolio objects to include in the account }
  \item{initDate}{ date (ISO8601) prior to the first trade date, used to contain initial equity }
  \item{initEq}{ initial capital (equity) in the account, default is zero }
}
\details{
  The account is a cash account where investments, withdrawals, management fees, and other capital account changes are made.  
}
\value{
  Creates an Account object, which is a list of portfolios that contains two xts objects within each symbol.
  \item{TOTAL }{a slot containing account-level data in a regular xts time series}
  \item{[portfolio] }{a named slot containing portfolio-level data for the underlying portfolio object in a regular xts time series}
}
\references{ 
  Coglianese, Michael. "Reading the bottom line," Futures Magazine, 1 August 1992. 
}
\author{ Peter Carl }
\note{ 
An Account object is a list of portfolios with aggregated portfolio summary information from all of the portfolios affiliated with the account.

The Account object is modeled on the information contained within the CFTC's thirteen-column presentation table.  To support that presentation, the account object tracks the following items at the account level:
  \item{Additions}{The sum of all new capital additions made at the beginning of the period into this account or initial deposits to a newly opened account. This does not include profits on existing money.}
  \item{Withdrawals}{The total of capital withdrawals made at the beginning of the period from this account, such as a partial withdrawal for whatever reason or the total ending equity from a terminating account.}
  \item{Txn.Fees}{The sum of brokerage commissions, exchange and other brokerage fees paid by the account during the period.}
  \item{Realized.PL}{The sum of net realized profits or losses aggregated from the underlying portfolios.  Gross realized profits can be calculated by adding 'Txn.Fees', the brokerage commission expenses for the period.}
  \item{Unrealized.PL}{The sum total increase or decrease in unrealized profits or losses on open positions in all of the portfolios in the account at the end of the period.}
  \item{Int.Income}{Includes all interest earned on investible capital, usually from debt instruments such as Treasury bills.}
  \item{Trading.PL}{Net realized profit or loss plus interest income plus change in unrealized profit or loss across all portfolios in the account. }
  \item{Advisory.Fees}{Management and incentive fees charged or accrued during the period by the advisor for services rendered.}
  \item{Net.Performance}{ Trading profit or loss minus charges made as advisory fees. }
  \item{End.Eq}{Capital available for trading as of the end of the period, such as the end of the day or of the month. The following period's beginning equity is equal to the prior period's ending equity.  Period rate of return can be calculated from this column.}

For each portfolio, the account object tracks calculated portfolio information.  That information is calculated by the function \code{\link{calcPortfSummary}}.  For each portfolio held in the account, a slot named for the portfolio contains the following columns, held in a regular xts time series:
\itemize{
  \item{Long.Value}{ The sum of the notional value of all positions held long in the portfolio.}
  \item{Short.Value}{The sum of the notional value of all positions held short in the portfolio.} 
  \item{Net.Value}{The sum of the notional long and notional short value of the portfolio.}
  \item{Gross.Value}{The sum of the notional long and absolute value of the notional short value of the portfolio.}
  \item{Txn.Fees}{The sum of brokerage commissions, exchange and other brokerage fees paid by the portfolio during the period.}
  \item{Realized.PL}{The sum of net realized profits or losses aggregated from the underlying positions in the portfolio.  Gross realized profits can be calculated by adding 'Txn.Fees', the brokerage commission expenses for the period.}
  \item{Unrealized.PL}{The sum total increase or decrease in unrealized profits or losses on open positions in the portfolio at the end of the period.}
  \item{Trading.PL}{Net realized profit or loss plus interest income plus change in unrealized profit or loss across all positions in the portfolio.}
  }
TODO: Eventually, an Account object should track cash transactions much like a Portfolio object.
}
\seealso{ \code{\link{calcPortfSummary}}, \code{\link{initPortf}} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }