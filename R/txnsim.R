#' Monte Carlo analysis of transactions
#'
#' Running simulations with similar properties as the backtest or production
#' portfolio may allow the analyst to evaluate the distribution of returns
#' possible with similat trading approaches and evaluate skill versus luck or
#' overfitting.
#'
#' @details
#'
#' If \code{update=TRUE} (the default), the user may wish to pass \code{Interval}
#' in dots to mark the portfolio at a different frequency than the market data,
#' especially for intraday market data.
#'
#' @param Portfolio string identifying a portfolio
#' @param n number of simulations, default = 100
#' @param replacement sample with or without replacement, default TRUE
#' @param tradeDef string to determine which definition of 'trade' to use. See \code{\link{tradeStats}}
#' @param update boolean indicating whether to call \code{\link{updatePortf}} on the simulated portfolios, default TRUE
#' @param \dots any other passthrough parameters
#'
#' @return a list object of class 'txnsim' containing:
#' \itemize{
#'   \item{\code{backtest.trades}:}{list by symbol containing trade start, quantity, duration from the original backtest}
#'   \item{\code{replicates}:}{a list by symbol containing all the resampled start,quantity, duration time series replicates}
#'   \item{\code{transactions}:}{a list by symbol for each replicate of the Txn object passed to \code{\link{addTxns}}}
#'   \item{\code{initeq}:}{a numeric variable containing the initEq of the portfolio, for starting portfolio value}
#'   \item{\code{seed}:}{ the value of \code{.Random.seed} for replication, if required}
#'   \item{\code{call}:}{an object of type \code{call} that contains the \code{txnsim} call}
#' }
#'
#' Note that this object and its slots may change in the future.
#' Slots \code{replicates},\code{transactions}, and \code{call} are likely
#' to exist in all future versions of this function, but other slots may be added
#' and removed as \code{S3method}'s are developed.
#'
#' @author Jasen Mackie, Brian G. Peterson
#' @references
#' Burns, Patrick. 2006. Random Portfolios for Evaluating Trading Strategies. http://papers.ssrn.com/sol3/papers.cfm?abstract_id=881735
#' @seealso \code{\link{mcsim}}, \code{\link{updatePortf}}
#' @examples
#' \dontrun{
#'
#'   n <- 10
#'
#'   ex.txnsim <- function(Portfolio,n=10,replacement=FALSE, tradeDef='increased.to.reduced') {
#'     #out <- txnsim(Portfolio,n,replacement)
#'     out <- txnsim(Portfolio,n,replacement, tradeDef = tradeDef)
#'     #out <- txnsim(Portfolio,n,replacement, tradeDef = "increased.to.reduced")
#'     for (i in 1:n){
#'       p<-paste('txnsim',Portfolio,i,sep='.')
#'       symbols<-names(getPortfolio(p)$symbols)
#'       for(symbol in symbols) {
#'         dev.new()
#'         chart.Posn(p,symbol)
#'       }
#'     }
#'     out
#'   } # end ex.txnsim
#'
#'   demo('longtrend',ask=FALSE)
#'   lt.nr <- ex.txnsim('longtrend',n, replacement = FALSE)
#'   lt.wr <- ex.txnsim('longtrend',n, replacement = TRUE)
#'
#'   require('quantstrat')
#'   demo('rsi',ask=FALSE)
#'   rsi.nr <- ex.txnsim('RSI',n, replacement = FALSE)
#'   rsi.wr <- ex.txnsim('RSI',n, replacement = TRUE)
#'
#' } #end dontrun
#'
#' @export
txnsim <- function(Portfolio,
                   n = 10,
                   replacement = TRUE,
                   tradeDef = "flat.to.flat",
                   update = TRUE,
                   ...) {
  # store the random seed for replication, if needed
  seed <- .GlobalEnv$.Random.seed
  
  # First get strategy start dates, duration and quantity
  # get portfolio, account and symbols objects
  p <- getPortfolio(Portfolio)
  symbols <- names(p$symbols)
  initDate <- attr(p, "initDate")
  currency <- attr(p, "currency")
  initEq   <- attr(p, "initEq")
  
  
  txnstruct <- function(i) {
    pt <- perTradeStats(Portfolio, symbols[i], tradeDef = tradeDef)
    start <- pt$Start
    end <- pt$End
    
    #TO BE DELETED - just keeping around for some code tips
    # appidx <- append(start, end, after = length(start))
    # idx <- order(as.Date(appidx, format = "%Y-%m-%d"))
    # newindex <- appidx[idx]
    # duration <- append(difftime(newindex[-1], newindex[-length(newindex)], units = "days"), 0) # add zero for first row of duration vector
    # newindex <- newindex[-which(duration == 0)] # remove all zero duration trades from duration
    # duration <- duration[duration != 0] # remove all zero duration trades from newindex
    # qty <- vector(length = length(newindex)) # create a qty vector with length of newindex
    # testindex <- newindex %in% start # create a TRUE or FALSE vector for newindex in start
    # qty[testindex == 1] <- pt$Max.Pos[which(start == newindex[newindex %in% start])]
    # txnsimdf <- data.frame(newindex, duration, qty)
    
    # get duration of non-flat periods
    # duration <- end - start # duration for non-flat periods
    duration <- difftime(end, start, units = "secs")
    #stratduration <- end[length(end)] - start[1] # this is for info
    stratduration <-
      difftime(end[length(end)], start[1], units = "secs") # this is for info
    
    # get duration of flat periods
    txns <- cumsum(getTxns(Portfolio, "IBM"))
    lagtxns <- lag(txns)
    startzero <- txns[which(txns$Txn.Qty == 0)]
    endzero <- lagtxns[which(lagtxns$Txn.Qty == 0)]
    #zeroduration <- index(endzero) - index(startzero)
    zeroduration <-
      difftime(index(endzero), index(startzero), units = "secs")
    
    # build dataframe of start dates and durations
    startdf <- cbind.data.frame(start, duration)
    # build dataframe of end dates and durations
    enddf <-
      cbind.data.frame(index(startzero)[-1], zeroduration[-1])
    names(enddf) <- c("start", "duration")
    
    #start building txnsimdf - WOOHOO (still needs Qty though which is gonna be tricky...if we take it from 'txns', which is surely preferred over Max.Pos from 'pt')
    txnsimdf <- rbind.data.frame(startdf, enddf)
    require(data.table)
    txnsimdf <- data.table(txnsimdf, key = "start")
    
    qty <- vector(length = length(txnsimdf$start))
    idx_start <- txnsimdf$start %in% start
    qty[idx_start == 1] <-
      pt$Init.Qty[index(which(idx_start == TRUE))]
    
    txnsimdf <- cbind.data.frame(txnsimdf, qty)
    
    names(txnsimdf) <- c("start", "duration", "quantity")
    #names(txnsimdf) <- c("start", "duration") # taken out quantity for now
    txnsimdf
  }
  
  # create a list of perTradeStats outputs per symbol
  backtest.trades <- lapply(1:length(symbols), txnstruct)
  names(backtest.trades) <- symbols
  
  ################################################################
  
  if (tradeDef == "flat.to.flat") {
    ### first set up functions for the lapply
    ## no replacement fns:
    # index expression for the replicate call, without replacement
    idxexpr.nr <- function(i, ...) {
      sample(nrow(backtest.trades[[i]]))
    }
    
    # inner function to build the replicate df
    repsgen.nr <- function(j, i, idx) {
      # build a vector of start times
      start <- first(backtest.trades[[i]]$start) +
        #start <- as.Date(first(backtest.trades[[i]]$start)) +
        cumsum(as.numeric(backtest.trades[[i]]$duration[idx[[j]]]))
      #cumsum(seconds_to_period(as.numeric(backtest.trades[[i]]$duration[idx[[j]]])))
      # add the fist start time back in
      start <- c(first(as.Date(backtest.trades[[i]]$start)), start)
      # take off the last end time, since we won't put in a closing trade
      start <- start[-length(start)]
      x <- data.frame(
        start = start,
        duration = backtest.trades[[i]]$duration[idx[[j]]],
        quantity = backtest.trades[[i]]$quantity[idx[[j]]]
      )
    } # end inner lapply function
    
    # outer function over the symbols
    symsample.nr <- function(i) {
      idx <- replicate(n, idxexpr.nr(i), simplify = FALSE)
      symreps <- lapply(1:length(idx), repsgen.nr, i, idx)
    }
    
    ## with replacement fns
    
    # index expression for the replicate call, with replacement
    idxexpr.wr <- function(i) {
      fudgefactor <- 1.1 # fudgefactor is added to size for sampling
      targetdur   <- sum(backtest.trades[[i]]$duration)
      avgdur      <- as.numeric(mean(backtest.trades[[i]]$duration))
      
      dur <- 0 # initialize duration counter
      tdf <- data.frame() #initialize output data.frame
      nsamples <- round(nrow(backtest.trades[[i]]) * fudgefactor, 0)
      while (dur < targetdur) {
        s <- sample(1:nrow(backtest.trades[[i]]), nsamples, replace = TRUE)
        sdf <-
          data.frame(duration = backtest.trades[[i]]$duration[s],
                     quantity = backtest.trades[[i]]$quantity[s])
        if (is.null(tdf$duration)) {
          tdf <- sdf
        } else {
          tdf <- rbind(tdf, sdf)
        }
        dur <- sum(tdf$duration)
        nsamples <-
          round(((targetdur - dur) / avgdur) * fudgefactor, 0)
        nsamples <- ifelse(nsamples == 0, 1, nsamples)
        # print(nsamples) # for debugging
        dur
      }
      # could truncate data frame here to correct total duration
      
      # the row which takes our duration over the target
      xsrow <-
        last(which(cumsum(as.numeric(tdf$duration)) < (targetdur))) + 1
      if (xsrow == nrow(tdf)) {
        # the last row sampled takes us over targetdur
        adjxsrow <- sum(tdf$duration) - targetdur
        tdf$duration[xsrow] <- tdf$duration[xsrow] - adjxsrow
      } else if (xsrow < nrow(tdf)) {
        # the last iteration of the while loop added more than one row
        # which took our duration over the target
        tdf <- tdf[-seq.int(xsrow + 1, nrow(tdf), 1), ]
        adjxsrow <- sum(tdf$duration) - targetdur
        tdf$duration[xsrow] <- tdf$duration[xsrow] - adjxsrow
      }
      # build a vector of start times
      start <-
        first(backtest.trades[[i]]$start) + cumsum(as.numeric(tdf$duration))
      # add the fist start time back in
      start <- c(first(backtest.trades[[i]]$start), start)
      # take off the last end time, since we won't put in a closing trade
      start <- start[-length(start)]
      # add start column to tdf
      tdf$start <- start
      # rearrange columns for consistency
      tdf <- tdf[, c("start", "duration", "quantity")]
      #return the data frame
      tdf
    } # end idexpr.wr
    
    # outer function over the symbols
    symsample.wr <- function(i) {
      symreps <- replicate(n, idxexpr.wr(i), simplify = FALSE)
    }
    
    # now create the replication series
    if (isTRUE(replacement)) {
      reps <- lapply(1:length(symbols), symsample.wr)
    } else {
      reps <- lapply(1:length(symbols), symsample.nr)
    }
    names(reps) <- symbols
    
  } # end flat.to.flat
  
  if (tradeDef == "flat.to.reduced" |
      tradeDef == "increased.to.reduced") {
    ### first set up functions for the lapply
    ## no replacement fns:
    # index expression for the replicate call, without replacement
    idxexpr.nr <- function(i, ...) {
      sample(nrow(backtest.trades[[i]]))
    }
    
    # inner function to build the replicate df
    repsgen.nr <- function(j, i, idx) {
      # build a vector of start times
      start <- first(backtest.trades[[i]]$start) +
        #start <- as.Date(first(backtest.trades[[i]]$start)) +
        cumsum(as.numeric(backtest.trades[[i]]$duration[idx[[j]]]))
      # add the fist start time back in
      start <- c(first(backtest.trades[[i]]$start), start)
      # take off the last end time, since we won't put in a closing trade
      start <- start[-length(start)]
      x <- data.frame(
        start = start,
        duration = backtest.trades[[i]]$duration[idx[[j]]],
        quantity = backtest.trades[[i]]$quantity[idx[[j]]]
      )
    } # end inner lapply function
    
    # outer function over the symbols
    symsample.nr <- function(i) {
      idx <- replicate(n, idxexpr.nr(i), simplify = FALSE)
      symreps <- lapply(1:length(idx), repsgen.nr, i, idx)
    }
    
    ## with replacement fns
    
    # index expression for the replicate call, with replacement
    idxexpr.wr <- function(i) {
      fudgefactor <- 1.1 # fudgefactor is added to size for sampling
      targetdur   <- sum(backtest.trades[[i]]$duration)
      avgdur      <- as.numeric(mean(backtest.trades[[i]]$duration))
      
      dur <- 0 # initialize duration counter
      tdf <- data.frame() #initialize output data.frame
      nsamples <- round(nrow(backtest.trades[[i]]) * fudgefactor, 0)
      while (dur < targetdur) {
        s <- sample(1:nrow(backtest.trades[[i]]), nsamples, replace = TRUE)
        sdf <-
          data.frame(duration = backtest.trades[[i]]$duration[s],
                     quantity = backtest.trades[[i]]$quantity[s])
        if (is.null(tdf$duration)) {
          tdf <- sdf
        } else {
          tdf <- rbind(tdf, sdf)
        }
        dur <- sum(tdf$duration)
        nsamples <-
          round(((targetdur - dur) / avgdur) * fudgefactor, 0)
        nsamples <- ifelse(nsamples == 0, 1, nsamples)
        # print(nsamples) # for debugging
        dur
      }
      # could truncate data frame here to correct total duration
      
      # the row which takes our duration over the target
      xsrow <-
        last(which(cumsum(as.numeric(tdf$duration)) < (targetdur))) + 1
      if (xsrow == nrow(tdf)) {
        # the last row sampled takes us over targetdur
        adjxsrow <- sum(tdf$duration) - targetdur
        tdf$duration[xsrow] <- tdf$duration[xsrow] - adjxsrow
      } else if (xsrow < nrow(tdf)) {
        # the last iteration of the while loop added more than one row
        # which took our duration over the target
        tdf <- tdf[-seq.int(xsrow + 1, nrow(tdf), 1), ]
        adjxsrow <- sum(tdf$duration) - targetdur
        tdf$duration[xsrow] <- tdf$duration[xsrow] - adjxsrow
      }
      # build a vector of start times
      # but first compute actual duration
      #actualdur <- last(backtest.trades[[i]]$start) + last(backtest.trades[[i]]$duration) - first(backtest.trades[[i]]$start)
      actualdur <-
        difftime(
          last(backtest.trades[[i]]$start) + last(backtest.trades[[i]]$duration),
          first(backtest.trades[[i]]$start),
          units = "secs"
        )
      num_overlaps <-
        ceiling(as.numeric(targetdur) / as.numeric(actualdur))
      tl <- list()
      xsrow2_count <- 0
      for (j in 1:num_overlaps) {
        if (j < num_overlaps) {
          xsrow2 <-
            last(which(cumsum(as.numeric(
              tdf$duration
            )) < (actualdur) * j)) + 1
        } else if (i == num_overlaps) {
          xsrow2 <- length(tdf$duration)
        }
        if (xsrow2 == nrow(tdf)) {
          # the last row sampled takes us over targetdur
          tl[[j]] <- tdf[(sum(sapply(tl, nrow)) + 1):xsrow2, ]
          # adjxsrow2 <- sum(tl[[i]]$duration) - actualdur
          # tl[[i]][xsrow2-xsrow2_count,1] <- tl[[i]]$duration[xsrow2-xsrow2_count] - adjxsrow2
        } else if (xsrow2 < nrow(tdf)) {
          # the last iteration of the while loop added more than one row
          # which took our duration over the target
          #tdf <- tdf[-seq.int(xsrow2 + 1, nrow(tdf), 1),]
          if (xsrow2_count == 0) {
            tl[[j]] <- tdf[-seq.int(xsrow2 + 1, nrow(tdf), 1), ]
          } else if (xsrow2_count > 0) {
            tl[[j]] <- tdf[(sum(sapply(tl, nrow)) + 1):xsrow2, ]
          }
          adjxsrow2 <- sum(tl[[j]]$duration) - actualdur
          #tdf$duration[xsrow2] <- tdf$duration[xsrow2] - adjxsrow2
          tl[[j]][xsrow2 - xsrow2_count, 1] <-
            tl[[j]]$duration[xsrow2 - xsrow2_count] - adjxsrow2
          xsrow2_count = xsrow2
        }
      }
      start <- list()
      tdf <- list()
      for (k in 1:num_overlaps) {
        start[[k]] <-
          first(backtest.trades[[i]]$start) + cumsum(as.numeric(tl[[k]]$duration))
        # add the fist start time back in
        start[[k]] <-
          c(first(backtest.trades[[i]]$start), start[[k]])
        # take off the last end time, since we won't put in a closing trade
        start[[k]] <- start[[k]][-length(start[[k]])]
        tdf[[k]] <- cbind(start[[k]], tl[[k]])
        #colnames(tdf[[k]][1]) <- 'start'
      }
      # add start column to tdf
      #tdf$start <- start
      
      # rearrange columns for consistency
      #tdf <- tdf[,c("start", "duration", "quantity")]
      #return the data frame
      tdf
    } # end idexpr.wr
    
    # outer function over the symbols
    symsample.wr <- function(i) {
      symreps <- replicate(n, idxexpr.wr(i), simplify = FALSE)
    }
    
    # now create the replication series
    if (isTRUE(replacement)) {
      reps <- lapply(1:length(symbols), symsample.wr)
    } else {
      reps <- lapply(1:length(symbols), symsample.nr)
    }
    names(reps) <- symbols
    
  } # end flat.to.reduced
  
  ################################################################
  # reps now exists as a list of structure reps[[symbol]][[rep]]
  # each rep has columns start, duration, quantity
  
  ####################
  # Generate Transactions
  
  # create portfolios
  for (i in seq_along(reps[[1]])) {
    # name the simulated portfolio
    simport <- paste("txnsim", Portfolio, i, sep = ".")
    # remove portfolio if it exists, we need to overwrite it anyway
    suppressWarnings(rm(list = paste0("portfolio.", simport), envir = .blotter))
    # generate portfolio
    simport <- initPortf(
      name = simport,
      symbols = symbols,
      initDate = initDate,
      currency = currency,
      initEq = initEq
    )
  }
  
  # this will be called by lapply over the list of replicates for a
  txnsimtxns <- function (i, symbol = symbol, ...) {
    simport <- paste("txnsim", Portfolio, i, sep = ".")
    #print(paste(simport,symbol))
    dargs <- list(...)
    if (!is.null(dargs$env))
      env <- dargs$env
    else
      env <- .GlobalEnv
    if (!is.null(dargs$prefer))
      prefer <- dargs$prefer
    else
      prefer <- NULL
    
    prices <- getPrice(get(symbol, pos = env),
                       prefer = prefer)[, 1]
    
    # the rep list has a start, duration, quantity in each row
    # we'll loop by row over that object to create an object for addTxns
    # @TODO find something more efficient than a for loop here
    # txns <- list()
    df <- reps[[symbol]][[i]]
    if (class(df) == 'data.frame')
      df <- list('1' = df)
    dflist <- df
    txnlist <- list()
    for (li in 1:length(dflist)) {
      txns <- list()
      df <- dflist[[li]]
      #df <- df[which(df$quantity != 0),] # remove zero quantity trades
      df <-
        df[which(df$duration != 0), ] # remove zero duration trades
      for (r in 1:nrow(df)) {
        # opening trade
        open  <- data.frame(
          start = df[r, 1],
          TxnQty = df[r, "quantity"],
          TxnPrice = as.numeric(last(prices[paste0("/", df[r, 1])]))
        )
        # closing trade
        close <-
          data.frame(
            start = index(last(prices[paste0("/", df[r, 1] + df[r, "duration"])])),
            TxnQty = -1 * df[r, "quantity"],
            TxnPrice = as.numeric(last(prices[paste0("/", df[r, 1] + df[r, "duration"])]))
          )
        txns[[r]] <- rbind(open, close)
      } # end loop over rows
      # we now have a list of transactions, turn them into an xts object
      txns <- do.call(rbind, txns)
      txns <-
        xts(txns[, c("TxnQty", "TxnPrice")], order.by = txns[, 1])
      txns <- txns[which(txns$TxnQty != 0), ]
      txnlist[[li]] <- txns
    }
    txns <- do.call(rbind, txnlist)
    addTxns(Portfolio = simport,
            Symbol = symbol,
            TxnData = txns)
    txns # return the data for later use
  }
  
  # loop over symbols in each replicate
  for (symbol in symbols) {
    ltxn <-
      lapply(1:length(reps[[symbol]]), txnsimtxns, symbol = symbol)
  } # end loop over symbols in replicate
  
  for (i in seq_along(reps[[1]])) {
    # update the simulated portfolio
    simport <- paste("txnsim", Portfolio, i, sep = ".")
    if (isTRUE(update))
      updatePortf(Portfolio = simport, ...)
  }
  
  # generate the return object
  ret <- list(
    replicates = reps,
    transactions = ltxn,
    backtest.trades = backtest.trades,
    initeq = initEq,
    seed = seed,
    call = match.call()
  )
  class(ret) <- "txnsim"
  ret
} # end txnsim fn


#' plot method for objects of type 'txnsim'
#'
#' @param x object of type 'txnsim' to plot
#' @param y not used, to match generic signature, may hold overlay data in the future
#' @param \dots any other passthrough parameters
#' @author Jasen Mackie, Brian G. Peterson
#' @seealso \code{\link{txnsim}}
#' @export
plot.txnsim <- function(x, y, ...) {
  n <- x$call$n
  port <- x$call$Portfolio
  cumpl <- NULL
  for (i in 1:n) {
    p <- paste('txnsim', port, i, sep = '.')
    if (!is.null(cumpl)) {
      cumpl <-
        cbind(cumpl, cumsum(getPortfolio(p)$summary$Net.Trading.PL[-1]))
      colnames(cumpl) <-
        c(colnames(cumpl)[-length(colnames(cumpl))], p)
    } else {
      cumpl <- cumsum(.getPortfolio(p)$summary$Net.Trading.PL[-1])
      colnames(cumpl) <- p
    }
  }
  cumpl <-
    cumpl[-which(complete.cases(cumpl) == FALSE)] # subset away rows with NA
  
  backtestpl <-
    cumsum(.getPortfolio(port)$summary$Net.Trading.PL[-1])
  colnames(backtestpl) <- port
  
  pt <- plot.xts(
    cumpl
    ,
    col = "lightgray"
    ,
    main = paste(port, 'simulation cumulative P&L')
    ,
    grid.ticks.on = 'years'
  )
  pt <-
    lines(cumsum(.getPortfolio(port)$summary$Net.Trading.PL[-1]), col = "red")
  print(pt)
  
  cumpl <- cbind(backtestpl, cumpl)
  invisible(cumpl)
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2016
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################