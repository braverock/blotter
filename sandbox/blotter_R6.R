# R6 repo: https://github.com/wch/R6

# Current blotter structure:
# .blotter
#   $account       (.blotter$account.`name`)
#     $portfolios
#       $`name`
#     $summary
#     $Additions
#     $Withdrawals
#     $Interest
#   $portfolio     (.blotter$portfolio.`name`)
#     $summary
#     $symbols
#       $posPL
#       $posPL.ccy
#       $txn

# Proposed blotter structure (comments welcome):
# > Account (R6 'Account' object)
#   $ addTxn / addTxns (addAcctTxn)
#   $ addPortfolio
#   $ Returns (AcctReturns)
#   $ PortfReturns (PortfReturns)
#   $ getPortfolioAttribute (getByPortf)
#   $ getEquity (getEndEq)
#   $ updateAcct (updateAcct)
#   $ updateEndEq (updateEndEq)
#   -> Portfolios (list|env of R6 'Portfolio' objects)
#     $ addInstrument (addPortfInstr)
#     $ calcPortfWgt (calcPortfWgt)
#     $ getSymbolAttribute (getBySymbol)
#     $ updatePortf
#     -> Positions (list|env of R6 'Position' objects)
#       $ addTxn / addTxns (addTxn / addTxns)
#       $ getPos
#       $ getPosQty
#       $ getTxn
#       $ tradeStats
#       $ perTradeStats
#       $ chart.Posn
#       $ chart.ME
#       -> Instrument
#       -> Transaction
#         - trades, splits, dividends, expirations, assignments, etc.
#       -> PnL
#       -> Position (R6 'Position' object)
#
#
# I'm not sure of the best way to implement the "list" of portfolios in an
# account, and the "list" of instruments in a portfolio.
#
# Should users be able to add portfolios via
#   Account$`name` <- Portfolio$new()
# or should we make that a method in the Account object?
#   Account$add_portfolio()
#
# If we choose either one, we need to ensure that the "name" member of the
# new Portfolio object matches the "name" of the Account$portfolios "list".

#acct$portfolios$MyPort$positions$SPY$addTxn()

# Q: Can we add stuff to the public environment internally?
# A: No, because self is locked
Simple <- R6Class("Simple",
  public = list(
    x = 1,
    add = function(name, value) {
      assign(name, value, self)
    }
  )
)
s <- Simple$new()
s$add("y",2)  # fails
# but maybe we can lock the binding of the portfolios in the environment?
Simple <- R6Class("Simple",
  public = list(
    initialize = function() {
      self$portfolios <- new.env()
      lockBinding("portfolios",self)
    },
    x = 1,
    portfolios = NULL,
    add = function(name, value) {
      unlockBinding("portfolios",self)
      assign(name, value, self$portfolios)
      lockBinding("portfolios",self)
    }
  )
)
s <- Simple$new()
s$add("y",2)  # works!
# but this also works unless the environment is locked...
rm(y, envir=s$portfolios)


library(R6)
library(blotter)

Account <- R6Class(classname = "Account",
  public = list(
    portfolios = NULL,
    initialize = function(portfolios, date, equity, currency) {
      # create the portfolios environment, then lock its binding
      # still need to lock portfolios itself though, but there's no
      # way to unlock environments in base R
      self$portfolios <- new.env()
      lockBinding("portfolios", self)

      if(!missing(currency)) private$currency. <- currency
    },
    deposit = function(date, amount) {
      private$deposits. <- rbind(private$deposits., xts(amount, date))
      #invisible(self)  # if you want to allow chained calls
    },
    withdrawal = function(date, amount) {
      private$withdrawals. <- rbind(private$withdrawals., xts(amount, date))
      #invisible(self)  # if you want to allow chained calls
    },
    interest = function(date, amount) {
      private$interest. <- rbind(private$interest., xts(amount, date))
      #invisible(self)  # if you want to allow chained calls
    },
    addPortfolio <- function(portfolio) {
        if(inherits(portfolio, "Portfolio")) {
          # unlock/relock portfolios environment here too, once it can be unlocked
          unlockBinding("portfolios", self)
          assign(portfolio$name, value, portfolios)
          lockBinding("portfolios", self)
          #invisible(self)  # if you want to allow chained calls
        } else {
          stop(deparse(substitute(portfolio)), " is not a Portfolio object", call.=FALSE)
        }
    }
  ),
  private = list(
    currency. = NA_character_,
    deposits. = NULL,
    interest. = NULL,
    withdrawals. = NULL
  ),
  active = list(
    equity = function(value) {
      if(missing(value)) {
        stop("equity calculation not yet implemented")
      } else {
        stop("equity cannot be directly updated")
      }
    }
  )
)

Portfolio <- R6Class(classname = "Portfolio",
  public = list(
    positions = NULL,
    initialize = function(positions, date, currency) {
      # create the positions environment, then lock its binding
      # still need to lock positions itself though, but there's no
      # way to unlock environments in base R
      self$positions <- new.env()
      lockBinding("positions", self)

      if(!missing(currency)) private$currency. <- currency
    },
    addPosition <- function(position) {
        if(inherits(positions, "Position")) {
          # unlock/relock positions environment here too, once it can be unlocked
          unlockBinding("positions", self)
          assign(position$symbol, value, positions)
          lockBinding("positions", self)
          #invisible(self)  # if you want to allow chained calls
        } else {
          stop(deparse(substitute(position)), " is not a Position object", call.=FALSE)
        }
    }
  ),
  private = list(
    currency. = NA_character_
  ),
  active = list(
#    equity = function(value) {
#      if(missing(value)) {
#        stop("equity calculation not yet implemented")
#      } else {
#        stop("equity cannot be directly updated")
#      }
#    }
  )
)

# I'm not sure "Position" is the correct name for this thing, but I was trying
# to create the most basic component of the blotter structure.
Position <- R6Class(classname = "Position",
  public = list(
    #initialize = function(instrument, transactions) {
    initialize = function(symbol, transactions, currency) {
      if(!missing(symbol)) private$symbol. <- symbol
      if(!missing(currency)) private$currency. <- currency
    },
    # addTxn {{{
    addTxn = function(date, quantity, price, fees=0, ..., ConMult=1, verbose=TRUE) {
      null.txn <- is.null(private$txn.)
      #PrevPosQty <- getPos(date, 'Pos.Qty')  # returns position 'as-of' date
      if(null.txn)
        PrevPosQty <- 0
      else {
        PrevPosQty <- private$txn.[paste0("/",date), 'Pos.Qty']
        PrevPosQty <- PrevPosQty[nrow(PrevPosQty),]
      }

      # split transactions that would cross through zero
      if(PrevPosQty != 0 &&
         sign(PrevPosQty + quantity) != sign(PrevPosQty) &&
         PrevPosQty != -quantity) {
        # calculate fees pro-rata by quantity
        txnFeeQty <- fees/abs(quantity) 
        self$addTxn(date, -PrevPosQty, price, txnFeeQty*abs(PrevPosQty), ...)
        # transactions need unique timestamps
        date <- date + sqrt(.Machine$double.eps)
        quantity <- quantity + PrevPosQty
        PrevPosQty <- 0
        fees <- txnFeeQty * abs(quantity + PrevPosQty)
      }
      
      # Coerce the transaction fees to a function if a string was supplied
      if(is.character(fees)) {
        tmp <- try(match.fun(fees), silent=TRUE)
        if(!inherits(tmp, "try-error"))
          fees <- tmp
      }
      # Compute transaction fees if a function was supplied
      if(is.function(fees))
        txnfees <- fees(quantity, price)
      else
        txnfees <- as.numeric(fees)

      if(is.null(txnfees) || is.na(txnfees))
        txnfees <- 0
      if(txnfees > 0)
        warning('Positive transaction fees should only be used in the case of broker/exchange rebates for TxnFees ',TxnFees,'. See Documentation.')
    
      # Calculate the value and average cost of the transaction
      TxnValue <- quantity * price * ConMult # Gross of Fees
      TxnAvgCost <- TxnValue / (quantity * ConMult)

      # Calculate the change in position
      PosQty <- PrevPosQty + quantity

      # Calculate the resulting position's average cost
      #PrevPosAvgCost <- getPos(date, 'Pos.Avg.Cost')  # returns position 'as-of' date
      if(null.txn)
        PrevPosAvgCost <- 0
      else {
        PrevPosAvgCost <- private$txn.[paste0("/",date), 'Pos.Avg.Cost']
        PrevPosAvgCost <- PrevPosAvgCost[nrow(PrevPosAvgCost),]
      }
      PosAvgCost <- blotter:::.calcPosAvgCost(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty, ConMult)

      # Calculate any realized profit or loss (net of fees) from the transaction
      GrossTxnRealizedPL <- quantity * ConMult * (PrevPosAvgCost - TxnAvgCost)

      # if the previous position is zero, RealizedPL = 0
      # if previous position is positive and position is larger, RealizedPL =0
      # if previous position is negative and position is smaller, RealizedPL =0
      if(abs(PrevPosQty) < abs(PosQty) || PrevPosQty == 0)
        GrossTxnRealizedPL = 0
	
      NetTxnRealizedPL <- GrossTxnRealizedPL + txnfees

      # Store the transaction and calculations
      txn <- c(quantity, price, TxnValue, TxnAvgCost, PosQty, PosAvgCost,
               GrossTxnRealizedPL, txnfees, NetTxnRealizedPL, ConMult)
      txnCols <- c("Txn.Qty", "Txn.Price", "Txn.Value", "Txn.Avg.Cost", 
        "Pos.Qty", "Pos.Avg.Cost", "Gross.Txn.Realized.PL", "Txn.Fees", 
        "Net.Txn.Realized.PL", "Con.Mult")
      private$txn. <- rbind(private$txn., xts(t(txn), date, dimnames=list(NULL, txnCols)))

      if(verbose)
        print(paste(format(date, "%Y-%m-%d %H:%M:%S"), private$symbol., quantity, "@", price, sep=" "))

      invisible(self)
    } #}}}
  ),
  private = list(
    symbol. = NA_character_,
    currency. = NA_character_,
    txn. = NULL
  ),
  # use active bindings for type-checking; is there a better way?
  active = list(
    symbol = function(value) {
      if(missing(value)) {
        private$symbol.
      } else {
        stop("symbol is read-only")
      }
    },
    currency = function(value) {
      if(missing(value)) {
        private$currency.
      } else {
        if(is.character(value) && length(value)==1)
          private$currency. <- value
        else
          stop(deparse(substitute(value)), " is an invalid currency name")
      }
    },
    transactions = function(value) {
      if(missing(value)) {
        if(!is.null(private$txn.))
        private$txn.[,c("Txn.Qty", "Txn.Price", "Txn.Fees",
                "Txn.Value", "Txn.Avg.Cost", "Net.Txn.Realized.PL")]
      } else {
        stop("use addTxn to add a transaction")
      }
    },
    txns = function(value) {
      if(missing(value)) {
        if(!is.null(private$txn.))
        private$txn.[,c("Txn.Qty", "Txn.Price", "Txn.Fees",
                "Txn.Value", "Txn.Avg.Cost", "Net.Txn.Realized.PL")]
      } else {
        stop("use addTxn to add a transaction")
      }
    },
    txn = function(value) {
      if(missing(value)) {
        private$txn.
      } else {
        stop("txn is read-only")
      }
    }
  )
)
pos <- Position$new('foo')
pos$addTxn(Sys.Date()-1L, 10, 90, 0)
pos$addTxn(Sys.Date(), -10, 100, 0)
pos$txns



if(FALSE){
  # blotter amzn demo provides an example of the basic functionality
  data("amzn")
  currency("USD")
  stock("amzn",currency="USD",multiplier=1)
  # Initialize the Portfolio
  initPortf("amzn_port",symbols="amzn",initDate="2010-01-14")
  initAcct("amzn_acct",portfolios="amzn_port",initDate="2010-01-14", initEq=10000)
  # look at the transactions data
  amzn.trades
  # Add the transactions to the portfolio
  blotter:::addTxns("amzn_port","amzn",TxnData=amzn.trades,verbose=TRUE)
  # update the portfolio stats
  updatePortf("amzn_port",Dates="2010-01-14")
  # update the account P&L
  updateAcct("amzn_acct",Dates="2010-01-14")
  # and look at it
  chart.Posn("amzn_port","amzn",Dates="2010-01-14")
}


