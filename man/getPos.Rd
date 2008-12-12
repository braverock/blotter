\name{getPos}
\alias{getPos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPos(Portfolio, Symbol, Date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Portfolio}{ ~~Describe \code{Portfolio} here~~ }
  \item{Symbol}{ ~~Describe \code{Symbol} here~~ }
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
function(Portfolio, Symbol, Date)
{ # @author Peter Carl

    # DESCRIPTION:
    # Retrieves all information about the position as of a date

    # Inputs
    # Portfolio: a portfolio object containing transactions
    # Symbol: an instrument identifier for a symbol included in the portfolio
    # Date: timestamp as of which to have the most recent position

    # Outputs
    # All data elements related to position in a row of an xts object
    # This should get much more complicated from here, particularly when it's conditional on symbol, etc.
    
    # FUNCTION
    PosData = Portfolio[[Symbol]]$txn
    toDate = paste('::', Date, sep="")
    # It may not make sense to return realized P&L with the position information, so only position and 
    # position average cost are returned.
    Pos = tail(PosData[toDate,c('Pos.Qty','Pos.Avg.Cost')], n=1)
    return(Pos)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
