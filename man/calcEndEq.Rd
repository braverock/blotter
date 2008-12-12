\name{calcEndEq}
\alias{calcEndEq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcEndEq(Account, Date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Account}{ ~~Describe \code{Account} here~~ }
  \item{Date}{ ~~Describe \code{Date} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Account, Date)
{ 
    # DESCRIPTION
    # Calculates End.Eq and Net.Performance

    # Requires that updateAcct has been run and any additional functions
    # have alread appended information into that table (e.g., additions or
    # withdrawals, fees, interest, etc.)
    Dates = time(Account[[1]])
    PrevDate = Dates[grep(Date, Dates)-1]
    PrevEndEq = getEndEq(Account, PrevDate)
    Additions = as.numeric(Account[[1]][Date]$Additions)
    Withdrawals = as.numeric(Account[[1]][Date]$Withdrawals)
    IntIncome = as.numeric(Account[[1]][Date]$Int.Income)
    TradingPL = as.numeric(Account[[1]][Date]$Trading.PL)
    AdvisoryFees = as.numeric(Account[[1]][Date]$Advisory.Fees)
    NetPerformance = IntIncome + TradingPL + AdvisoryFees
    Account[[1]][Date,'Net.Performance'] = as.numeric(NetPerformance) 
    Account[[1]][Date,'End.Eq'] = as.numeric(PrevEndEq + Additions + Withdrawals + NetPerformance)
    return(Account)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
