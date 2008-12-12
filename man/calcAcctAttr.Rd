\name{calcAcctAttr}
\alias{calcAcctAttr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcAcctAttr(Account, Attribute, Date = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Account}{ ~~Describe \code{Account} here~~ }
  \item{Attribute}{ ~~Describe \code{Attribute} here~~ }
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
function(Account, Attribute, Date=NULL)
{ # @author Peter Carl

    # DESCRIPTION
    # 

    # Inputs
    # Account: an Account object containing Portfolio summaries
    # Attribute: column name to be assembled for each symbol, any of:
    # 'Long.Value', 'Short.Value', 'Net.Value', 'Gross.Value', 'Txn.Fees',
    # 'Realized.PL', 'Unrealized.PL', or 'Trading.PL'

    portfolios = names(Account)[-1]
    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Account[[2]])
    else
        Date = time(Account[[2]][Date])
    table = xts(NULL, order.by=Date) ## Reference time index
    table = getByPortfolio(Account, Attribute, Date)
    result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1))))
    colnames(result) = Attribute
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
