calcPortfWgt <- function(Portfolio, Symbols = NULL, Dates = NULL, denominator = c('Gross.Value', 'Net.Value', 'Long.Value', 'Short.Value'), Account = NULL)
{ # @author Peter Carl

    # DESCRIPTION
    # Calculates the portfolio weights for positions within a given portfolio.
    # Portfolio weights may be calculated differently depending on their use.

    # Inputs
    # Portfolio: a portfolio object structured with initPortf()
    # Symbol: an instrument identifier for a symbol included in the portfolio,
    #   e.g., IBM
    # Dates: dates to return the calculation over formatted as xts range

    # Outputs
    # Timeseries object with weights by date in rows and symbolname in columns

    # FUNCTION

    pos.value = getBySymbol(Portfolio = Portfolio, Date = Dates, Attribute = "Pos.Value", Symbols = Symbols)
    portf.value = calcPortfAttr(Portfolio = Portfolio, Date = Dates, Attribute = denominator[1])
    weights = apply(pos.value, MARGIN = 2, FUN = function(x,y){return(x/y)}, y=portf.value) 

    return(weights)
}