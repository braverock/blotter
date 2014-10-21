#' Calculate account returns
#' 
#' Similar to the \code{PortfReturns} function, but gives returns for the 
#' entire account and takes into account external cashflows. External cashflows
#' are defined as contributions to or withdrawals from the account. Allows 
#' selecting between time-weighted returns and linked modified Dietz approach. 
#' If time-weighted method is selected, returns at time \eqn{t} are computed 
#' using: \deqn{r_{t}=\frac{V_{t}}{V_{t-1}+C_{t}}-1}
#' where \eqn{V_{t}} - account value at time \eqn{t}, \eqn{C_{t}} - cashflow at
#' time \eqn{t}. The implicit assumption made here is that the cash flow is 
#' available for the portfolio manager to invest from the beginning of the day. 
#' These returns then can be chain linked with geometric compounding (for 
#' instance using \code{Return.cumulative} function from the 
#' \code{PerformanceAnalytics} package) to yield cumulative multi-period 
#' returns:
#' \deqn{1+r=\prod_{t=1}^{T}(1+r_{t})=\prod_{t=1}^{T}\frac{V_{t}}{V_{t-1}+C_{t}}}
#' In the case if there were no cashflows, the result reduces to simple 
#' one-period returns. Time-weighted returns has also an interpretation in
#' terms of unit value pricing.
#' If Modified Dietz method is selected, monthly returns are computed taking
#' into account cashflows within each month:
#' \deqn{r = \frac{V_{t}-V_{t-1}-C}{V_{t-1}+\sum_{t}C_{t}\times W_{t}}}
#' where \eqn{C} - total external cash flows within a month, 
#' \eqn{C_{t}} - external cashflow at time \eqn{t}, 
#' \deqn{W_{t}=\frac{TD-D_{t}}{TD}} - weighting ratio to be applied to external 
#' cashflow on day \eqn{t},
#' \eqn{TD} - total number of days within the month,
#' \eqn{D_{t}} - number of days since the beginning of the month including 
#' weekends and public holidays.
#' Finally monthly Modified Dietz returns can also be linked geometrically.
#' 
#' @aliases AcctReturns
#' @param Account string name of the account to generate returns for
#' @param \dots any other passthru parameters (like \code{native} for 
#' \code{.getBySymbol}
#' @param Dates xts style ISO 8601 date subset to retrieve, default NULL 
#' (all dates)
#' @param Portfolios concatenated string vector for portfolio names to retrieve
#' returns on, default NULL (all portfolios)
#' @param method Used to select between time-weighted and linked modified Dietz
#' returns. May be any of: \itemize{\item timeweighted \item dietz} By default
#' time-weighted is selected
#' @return returns xts with account returns
#' @author Brian Peterson, Andrii Babii
#' @seealso PortfReturns
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E. 
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 
#' 2009. Chapter 5 \cr Bacon, C. \emph{Practical Portfolio Performance 
#' Measurement and Attribution}. Wiley. 2004. Chapter 2 \cr
#' @keywords portfolio returns
#' @note
#' TODO handle portfolio and account in different currencies (not hard, just not done)
#' 
#' TODO explicitly handle portfolio weights
#' 
#' TODO support additions and withdrawals to available capital
#' @export
AcctReturns <- 
function(Account, Dates = NULL, Portfolios = NULL, method = c("timeweighted", "dietz"), ...)
{ # @author Brian Peterson, Andrii Babii
	aname <- Account
	if(!grepl("account\\.", aname)){
	  Account <- try(get(paste("account", aname, sep = '.'), envir = .blotter))
	}  else{
    Account <- try(get(aname, envir = .blotter))
	}
	if(inherits(Account, "try-error")){
	  stop("Account ", aname, " not found, use initAcct() to create a new account")
	}
	if(!inherits(Account, "account")){
    stop("Account ", aname, " passed is not the name of an account object.")
	}
	if(is.null(Portfolios)){
	  Portfolios = names(Account$portfolios)
	}
  
	# Get xts with net trading P&L for all portfolios associated with account
	table = NULL
	for(pname in Portfolios){
		Portfolio <- getPortfolio(pname)
		if(is.null(Dates)){
		  Dates <- paste("::", last(index(Portfolio$summary)), sep = '')
		}
		ptable = .getBySymbol(Portfolio = Portfolio, Attribute = "Net.Trading.PL", 
                          Dates = Dates)
		if(is.null(table)){
		  table=ptable
		}
		else{
		  table=cbind(table,ptable)
		}
	}
  if(!is.null(attr(Account, 'initEq'))){
	  initEq <- as.numeric(attr(Account, 'initEq'))
		if(initEq == 0){
      stop("Initial equity of zero would produce div by zero NaN, Inf, -Inf 
             returns, please fix in initAcct().")
		}
    
	  #TODO check portfolio and account currencies and convert if necessary
    
    CF = Account$summary$Additions - Account$summary$Withdrawals # Cashflows
    V = initEq + reclass(rowSums(table), table)                  # Account values
    method = method[1]
    
    if (method == "timeweighted"){
      # Time-weighted returns
      returns = V  / (lag(V) + CF) - 1
    }
    
    if (method == "dietz"){
      # Linked modified Dietz
      C = apply.monthly(CF, sum)   # total monthly cashflow
      V = apply.monthly(V, first)  # monthly account values
      cfweighted <- function(CF){
        TD = ndays(CF)             # total number of days within the period
        # number of days since the beginning of the period
        D = round(as.vector((index(CF) - index(CF)[1])/3600/24)) 
        W = (TD - D) / TD          # weights
        cashfl = sum(CF * W)       # weighted sum of cashflows within the period
        return(cashfl)
      }
      cashfl = apply.monthly(CF, cfweighted)
      returns = (V - lag(V) - C) / (lag(V) + cashfl) # Modified Dietz
    }
	}
	return(returns)
}
