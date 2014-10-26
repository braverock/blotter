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
#   -> Portfolios (list|env of R6 'Portfolio' objects)
#     -> Positions (list|env of R6 'Position' objects)
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

library(R6)
library(blotter)

# I'm not sure "Position" is the correct name for this thing, but I was trying
# to create the most basic component of the blotter structure.
Position <- R6Class(classname = "Position",
  public = list(
    #initialize = function(instrument, transactions) {
    initialize = function(symbol, transactions, currency) {
      if(!missing(symbol)) private$.symbol <- symbol
      if(!missing(currency)) private$.currency <- currency
    },
    # addTxn {{{
    addTxn = function(date, quantity, price, fees=0, ..., ConMult=1, verbose=TRUE) {
      null.txn <- is.null(private$.txn)
      #PrevPosQty <- getPos(date, 'Pos.Qty')  # returns position 'as-of' date
      if(null.txn)
        PrevPosQty <- 0
      else {
        PrevPosQty <- private$.txn[paste0("/",date), 'Pos.Qty']
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
        PrevPosAvgCost <- private$.txn[paste0("/",date), 'Pos.Avg.Cost']
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
      private$.txn <- rbind(private$.txn, xts(t(txn), date, dimnames=list(NULL, txnCols)))

      if(verbose)
        print(paste(format(date, "%Y-%m-%d %H:%M:%S"), private$.symbol, quantity, "@", price, sep=" "))

      invisible(self)
    } #}}}
  ),
  private = list(
    .symbol = NA_character_,
    .currency = NA_character_,
    .txn = NULL
  ),
  # use active bindings for type-checking; is there a better way?
  active = list(
    symbol = function(value) {
      if(missing(value)) {
        private$.symbol
      } else {
        stop("symbol is read-only")
      }
    },
    currency = function(value) {
      if(missing(value)) {
        private$.currency
      } else {
        if(is.character(value) && length(value)==1)
          private$.currency <- value
        else
          stop(deparse(substitute(value)), " is an invalid currency name")
      }
    },
    transactions = function(value) {
      if(missing(value)) {
        if(!is.null(private$.txn))
        private$.txn[,c("Txn.Qty", "Txn.Price", "Txn.Fees",
                "Txn.Value", "Txn.Avg.Cost", "Net.Txn.Realized.PL")]
      } else {
        stop("use addTxn to add a transaction")
      }
    },
    txns = function(value) {
      if(missing(value)) {
        if(!is.null(private$.txn))
        private$.txn[,c("Txn.Qty", "Txn.Price", "Txn.Fees",
                "Txn.Value", "Txn.Avg.Cost", "Net.Txn.Realized.PL")]
      } else {
        stop("use addTxn to add a transaction")
      }
    },
    txn = function(value) {
      if(missing(value)) {
        private$.txn
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



# other stuff; not sure it's useful
if(FALSE){
Portfolio <- R6Class(classname = "Portfolio",
  public = list(
    initialize = function(name, positions, currency) {
      if(!missing(name))
        private$.name <- name
      if(!missing(positions)) {
        if(is.list(positions))
          for(p in positions)
            assign(p$symbol, p, private$.positions)
        else
          assign(positions$symbol, positions, private$.positions)
      }
    },
    update = function(symbols, dates, prices, interval) { }
  ),
  private = list(
    .positions = new.env(hash=TRUE),
    .name = "default"
  ),
  active = list(
    name = function(value) {
      if(missing(value)) {
        private$.name
      } else {
        if(is.character(value) && length(value)==1)
          private$.name <- value
        else
          stop(deparse(substitute(value)), " is an invalid portfolio name")
      }
    },
    positions = function(value) {
      value_name <- deparse(substitute(value))
      if(missing(value)) {
        #if(value_name!="")
          # return a copy of the .positions environment as a list, because
          # we do not want users to be able to change positions manually
        #  as.list(get(value_name, private$.positions, inherits=FALSE))
        as.list(private$.positions)
        #else
        #  NULL
      } else {
        if(inherits(value, "Position")) {
          assign(value_name, value, private$.positions)
        } else {
          stop(value_name, " is not a Position object", call.=FALSE)
        }
      }
    }
  )
)
pos <- Position$new('foo')
pos$addTxn(Sys.Date()-1L, 10, 90, 0)
pos$addTxn(Sys.Date(), -10, 100, 0)
pos$txns
p <- Portfolio$new("hello_world", pos, "USD")
p$positions

# prototype
Positions <- R6Class(classname = "Positions",
  public = list(
    add = function(position) {
      if(missing(position))
        invisible()
      if(inherits(position, "Position")) {
        assign(position$symbol, position, private$.positions)
      } else {
        position_name <- deparse(substitute(position))
        stop(position_name, " is not a Position object", call.=FALSE)
      }
    }
  ),
  private = list(
    .positions = new.env(hash=TRUE)
  )
)
}

