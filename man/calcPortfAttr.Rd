\name{calcPortfAttr}
\alias{calcPortfAttr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcPortfAttr(Portfolio, Attribute, Date = NULL)
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
{
    symbols = names(Portfolio)
    if(is.null(Date)) # if no date is specified, get all available dates
        Date = time(Portfolio[[1]]$posPL)
    else
        Date = time(Portfolio[[1]]$posPL[Date])
    table = xts(NULL, order.by=Date) ## Reference time index

    switch(Attribute,
        Trading.PL = {
            table = getBySymbol(Portfolio, 'Trading.PL', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1))))
            colnames(result) = 'Trading.PL'
        },
        Txn.Fees = {
            table = getBySymbol(Portfolio, 'Txn.Fees', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Txn.Fees'
        },
        Realized.PL = {
            table = getBySymbol(Portfolio, 'Realized.PL', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Realized.PL'
        },
        Unrealized.PL = {
            table = getBySymbol(Portfolio, 'Unrealized.PL', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Unrealized.PL'
        },
        Net.Value = {
            table = getBySymbol(Portfolio, 'Pos.Value', Date)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Net.Value'
        },
        Gross.Value = {
            table = getBySymbol(Portfolio, 'Pos.Value', Date)
            result = as.xts(t(t(apply(abs(table), FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Gross.Value'
        },
        Long.Value = {
            table = getBySymbol(Portfolio, 'Pos.Value', Date)
            table = apply(table,MARGIN=c(1,2),FUN=max,0)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Long.Value'
        },
        Short.Value = {
            table = getBySymbol(Portfolio, 'Pos.Value', Date)
            table = apply(table,MARGIN=c(1,2),FUN=min,0)
            result = as.xts(t(t(apply(table, FUN='sum', MARGIN=1)))) ## WTF?
            colnames(result) = 'Short.Value'
        }
    )
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
