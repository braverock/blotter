\name{calcPortfSummary}
\alias{calcPortfSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcPortfSummary(Portfolio, Date = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Portfolio}{ ~~Describe \code{Portfolio} here~~ }
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
function(Portfolio, Date=NULL)
{ # @ author Peter Carl

    # DESCRIPTION
    # Create portfolio summary with the following columns
    # Long.Value, Short.Value, Net.Value, Trading.PL

    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Portfolio[[1]]$posPL)
    else
        Date = time(Portfolio[[1]]$posPL[Date])
    TradingPL = calcPortfAttr(Portfolio, 'Trading.PL', Date)
    RealizedPL = calcPortfAttr(Portfolio, 'Realized.PL', Date)
    UnrealizedPL = calcPortfAttr(Portfolio, 'Unrealized.PL', Date)
    # UnrealizedPL = TradingPL - RealizedPL
    TxnFees = calcPortfAttr(Portfolio, 'Txn.Fees', Date)
    NetValue = calcPortfAttr(Portfolio, 'Net.Value', Date)
    GrossValue = calcPortfAttr(Portfolio, 'Gross.Value', Date)
    LongValue = calcPortfAttr(Portfolio, 'Long.Value', Date)
    ShortValue = calcPortfAttr(Portfolio, 'Short.Value', Date)

    summary=merge(LongValue, ShortValue, NetValue, GrossValue, TxnFees, RealizedPL, UnrealizedPL, TradingPL)
#     colnames(summary)=c('Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees','Realized.PL', 'Unrealized.PL', 'Trading.PL')
# Needed?
    return(summary)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
