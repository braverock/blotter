\name{getBySymbol}
\alias{getBySymbol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getBySymbol(Portfolio, Attribute, Date = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Portfolio}{ ~~Describe \code{Portfolio} here~~ }
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
function(Portfolio, Attribute, Date=NULL)
{ # @author Peter Carl

    # DESCRIPTION:
    # Retrieves calculated attributes for each position in the portfolio
    # from the posPL table.  Assembles into a symbol-by-time table useful
    # for graphing or calculations

    # Inputs
    # Portfolio: a portfolio object containing transactions
    # Item: column name to be assembled for each symbol, any of:
    #   'Pos.Qty', 'Pos.Value', 'Txn.Value', 'Realized.PL', 'Unrealized.PL',
    #    or 'Trading.PL'

    # Outputs
    # regular xts object of values by symbol

    # FUNCTION
    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Portfolio[[1]]$posPL)
    else
        Date = time(Portfolio[[1]]$posPL[Date])
    table = xts(NULL, order.by=Date) 
      ## Need a reference time index
    symbols=names(Portfolio)
    for (i in 1:length(symbols)) 
        table = merge(table, Portfolio[[i]]$posPL[Date,Attribute,drop=FALSE])
    colnames(table) = symbols
    return(table)

## TODO: append summary information in last columns based on Attribute requested
# e.g., 'Pos.Value' would append Net.Value, Gross.Value, Long.Value, Short.Value
# using calcPortfAttr(p,'Net.Value')
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
