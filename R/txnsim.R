#' Monte Carlo analysis of round turn trades
#'
#' Running simulations with similar properties as the backtest or production
#' portfolio may allow the analyst to evaluate the distribution of returns
#' possible with similar trading approaches and evaluate skill versus luck or
#' overfitting.
#'
#' @details
#'
#' Statisticians talk about the 'stylized facts' of a data set.  If you consider
#' the stylized facts of a series of transactions that are the output of a 
#' discretionary or systematic trading strategy, it should be clear that there 
#' is a lot of information available to work with.  Initial analysis such as 
#' \code{\link{tradeStats}} and \code{\link{perTradeStats}} can describe the 
#' results of the series of transactions which resulted from the trading 
#' strategy.  What else can we learn from these transactions regarding trading 
#' style or the skill of the trader? \code{txnsim} seeks to conduct a simulation
#' over the properties of sampled round turn trades to help evaluate this.
#' 
#' With \code{tradeDef='flat.to.flat'}, the samples are simply rearranging 
#' quantity and duration of round turns.  This may be enough for a strategy that
#' only puts on a single level per round turn.
#' 
#' With \code{tradeDef='increased.to.reduced'}, typically used for more complex 
#' strategies, the simulation is also significantly more complicated, especially
#' with \code{replacement=TRUE}.  In this latter case, the simulation must try 
#' to retain stylized factos of the observed strategy, specifically:
#' 
#' \itemize{
#'   \item{percent time in market}
#'   \item{percent time flat}
#'   \item{ratio of long to short position taking}
#'   \item{number of levels or layered trades observed}
#' }
#'
#' In order to do this, samples are taken and randomized for flat periods, 
#' short periods, and long periods, and then these samples are interleaved and 
#' layered to construct the random strategy.  The overall goal is to construct a 
#' random strategy that preserves as many of the stylized facts (or style) of
#' the observed strategy as possible, while demonstrating no skill.  The round 
#' turn trades of the random replicate strategies, while outwardly resembling 
#' the original strategy in summary time series statistics, are the result of 
#' random combinations of observed features taking place at random times in the
#' tested time period.
#' 
#' It should be noted that the first opened trade of the observed series and the
#' replicates will take place at the same time.  Quantity and duration may differ,
#' but the trade will start at the same time, unless the first sampled period is
#' a flat one.  We may choose to relax this in the future and add or subtract a 
#' short amount of duration to the replicates to randomize the first entry more
#' fully as well.
#'  
#' The user may wish to pass \code{Interval} in dots to mark the portfolio at a
#' different frequency than the market data, especially for intraday market
#' data.  Note that market data must be available to call
#' \code{\link{updatePortf}} on.
#' 
#' @param Portfolio string identifying a portfolio
#' @param n number of simulations, default = 100
#' @param replacement sample with or without replacement, default TRUE
#' @param tradeDef string to determine which definition of 'trade' to use. See \code{\link{tradeStats}}
#' @param \dots any other passthrough parameters
#'
#' @return a list object of class 'txnsim' containing:
#' \itemize{
#'   \item{\code{replicates}:}{a list by symbol containing all the resampled start,quantity, duration time series replicates}
#'   \item{\code{transactions}:}{a list by symbol for each replicate of the Txn object passed to \code{\link{addTxns}}}
#'   \item{\code{backtest.trades}:}{list by symbol containing trade start, quantity, duration from the original backtest}
#'   \item{\code{cumpl}:}{an \code{xts} object containing the cumulative P&L of each replicate portfolio}
#'   \item{\code{initEq}:}{a numeric variable containing the initEq of the portfolio, for starting portfolio value}
#'   \item{\code{seed}:}{ the value of \code{.Random.seed} for replication, if required}
#'   \item{\code{call}:}{an object of type \code{call} that contains the \code{txnsim} call}
#' }
#'
#' Note that this object and its slots may change in the future.
#' Slots \code{replicates},\code{transactions}, and \code{call} are likely
#' to exist in all future versions of this function, but other slots may be added
#' and removed as \code{S3method}'s are developed.
#' 
#' The \code{backtest.trades} object contains the stylized facts of the observed 
#' series, and consists of a list with one slot per instrument in the input 
#' portfolio.  Each slot in that list contains a \code{data.frame} of
#' \itemize{
#'   \item{\code{Start}:}{timestamp of the start of the round turn, discarded later}
#'   \item{\code{duration}:}{duration (difference from beginning ot end) of the observed round turn trade}
#'   \item{\code{quantity}:}{quantity of the round turn trade, or 0 for flat periods}
#' }
#' 
#' with additional attributes for the observed stylized facts:
#' 
#' \itemize{
#'   \item{\code{calendar.duration}:}{total length/duration of the observed series}
#'   \item{\code{trade.duration}:}{total length/durtation used by round turn trades }
#'   \item{\code{flat.duration}:}{aggregate length/duration of periods when observed series was flat}
#'   \item{\code{flat.stddev}:}{standard deviation of the duration of individual flat periods}
#'   \item{\code{first.start}:}{timestamp of the start of the first trade, to avoid starting simulations during a training period}
#'   \item{\code{period}:}{periodicity of the observed series}
#' }
#' 
#' 
#' @author Jasen Mackie, Brian G. Peterson
#' @references
#' Burns, Patrick. 2006. Random Portfolios for Evaluating Trading Strategies. http://papers.ssrn.com/sol3/papers.cfm?abstract_id=881735
#' @seealso \code{\link{mcsim}}, \code{\link{updatePortf}} , \code{\link{perTradeStats}}
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
#'   require('quantstrat') #sorry for the circular dependency
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
                   tradeDef = c('increased.to.reduced', 'flat.to.flat', 'flat.to.reduced'),
                   ...) 
{
  # befor doing anything inside the function which would affect the state,
  # store the current random seed for later replication, if needed 
  seed <- .GlobalEnv$.Random.seed

  # use the first tradeDef
  tradeDef <- tradeDef[1]
  
  # First get strategy start dates, duration and quantity
  # get portfolio, account and symbols objects
  p <- getPortfolio(Portfolio)
  symbols <- names(p$symbols)
  initDate <- attr(p, "initDate")
  currency <- attr(p, "currency")
  initEq   <- attr(p, "initEq")
  
  
  txnstruct <- function(i) {
    pt <- perTradeStats(Portfolio, symbols[i], tradeDef = tradeDef, includeFlatPeriods = TRUE)

    nonflat <- which(pt$Init.Qty!=0)
    
    # get duration of non-flat periods
    tradeduration <- sum(pt$duration[nonflat])
    
    # get duration and standard deviation of flat periods
    zeroduration  <- sum(pt$duration[-nonflat])
    zerostddev    <- sd(pt$duration[-nonflat])
    
    # calendar duration of the entire strategy
    stratduration <- difftime(last(pt$End[nonflat]), pt$Start[1], units = "secs")
    
    # build dataframe of start dates and durations
    txnsimdf <- data.frame(start    = pt$Start,
                           duration = pt$duration,
                           quantity = pt$Init.Qty)
  
    attr(txnsimdf,"calendar.duration") <- stratduration
    attr(txnsimdf,"trade.duration")    <- tradeduration
    attr(txnsimdf,"flat.duration")     <- zeroduration 
    attr(txnsimdf,"flat.stddev")       <- zerostddev
    attr(txnsimdf,"first.start")       <- pt$Start[1]
    attr(txnsimdf,"period")            <- attr(pt,'trade.periodicity')
    
    txnsimdf
  }
  
  # create a list of perTradeStats outputs per symbol
  backtest.trades <- lapply(1:length(symbols), txnstruct)
  names(backtest.trades) <- symbols
  
  ################################################################
  # common utility functions

  # no replacement functions are common to all tradeDef methods

  # index expression for the replicate call, without replacement
  idxexpr.nr <- function(i, ...) {
    sample(nrow(backtest.trades[[i]]))
  }

  # inner function to build the replicate df
  repsgen.nr <- function(j, i, idx) {
    # build a vector of start times
    start <- first(backtest.trades[[i]]$start) +
      cumsum(as.numeric(backtest.trades[[i]]$duration[idx[[j]]]))
    # add the fist start time back in
    start <- c(first(backtest.trades[[i]]$start), start)
    # take off the last end time, since we won't put in a closing trade
    start <- start[-length(start)]
    x <- data.frame(start = start,
                    duration = backtest.trades[[i]]$duration[idx[[j]]],
                    quantity = backtest.trades[[i]]$quantity[idx[[j]]])
  } # end inner lapply function
  
  # outer function over the symbols
  symsample.nr <- function(i) {
    idx <- replicate(n, idxexpr.nr(i), simplify = FALSE)
    symreps <- lapply(1:length(idx), repsgen.nr, i, idx)
  }
  
  ################################################################
  ################################################################
  
  if (tradeDef == "flat.to.flat") {
    ### first set up functions for the lapply
    ## with replacement fns are different between flat.to.flat and other methods
    
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
        nsamples <- round(((targetdur - dur) / avgdur) * fudgefactor, 0)
        nsamples <- ifelse(nsamples == 0, 1, nsamples)
        # print(nsamples) # for debugging
        dur
      }
      # could truncate data frame here to correct total duration
      
      # the row which takes our duration over the target
      xsrow <- last(which(cumsum(as.numeric(tdf$duration)) < (targetdur))) + 1
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
      start <- first(backtest.trades[[i]]$start) + cumsum(as.numeric(tdf$duration))
      # add the first start time back in
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
    ## with replacement fns are different to other methods
    
    # sample and layer trades, with replacement
    tradesample.wr <- function(trades) {
      
      fudgefactor <- 1.1 # fudgefactor is added to size for sampling
      
      # stylized facts
      calendardur <- attr(trades, 'calendar.duration')
      totaldur    <- sum(trades$duration)
      avgdur      <- as.numeric(mean(trades$duration))
      traderows   <- which(trades$quantity != 0)
      longrows    <- which(trades$quantity  > 0)
      shortrows   <- which(trades$quantity  < 0)
      flatrows    <- which(trades$quantity == 0)
      flatdur     <- sum(trades[flatrows,'duration'])
      longdur     <- sum(trades[longrows,'duration'])
      shortdur    <- sum(trades[shortrows,'duration'])
      lsratio     <- as.numeric(longdur)/as.numeric(shortdur)
      
      subsample <- function(svector, targetdur) {
        #`trades` already exists in function scope 
        
        dur <- 0 # initialize duration counter
        tdf <- data.frame() #initialize output data.frame
        nsamples <- round(length(svector) * fudgefactor, 0)
        while (dur < targetdur) {
          s <- sample(svector, nsamples, replace = TRUE)
          sdf <- data.frame(duration = trades[s,'duration'],
                            quantity = trades[s,'quantity'])
          if (is.null(tdf$duration)) {
            tdf <- sdf
          } else {
            tdf <- rbind(tdf, sdf)
          }
          dur <- sum(tdf$duration)
          nsamples <- round(((targetdur - dur) / avgdur) * fudgefactor, 0)
          nsamples <- ifelse(nsamples == 0, 1, nsamples)
          # print(nsamples) # for debugging
          dur
        }
        # could truncate data frame here to correct total duration
        # the row which takes our duration over the target
        xsrow <- last(which(cumsum(as.numeric(tdf$duration)) < (targetdur))) + 1
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
        
        tdf  # return target data frame
      } # end subsample
      
      #sample long, short, flat periods
      flatdf  <- subsample(svector = flatrows, targetdur = flatdur)
      longdf  <- subsample(svector = longrows, targetdur = longdur)
      shortdf <- subsample(svector = shortrows, targetdur = shortdur)
      
      
      # make the first layer
      # 1. start with flat periods
      firstlayer <- flatdf
      # 2. segment trades for first layer
      targetlongdur <- structure(round((calendardur-flatdur)*lsratio),units='secs',class='difftime')
      targetlongrow <- last(which(cumsum(as.numeric(longdf$duration))<targetlongdur))
      firstlayer    <- rbind(firstlayer,longdf[1:targetlongrow,])
      targetshortrow<- last( which( cumsum(as.numeric(shortdf$duration))<(calendardur-sum(firstlayer$duration)) ) )
      firstlayer    <- rbind(firstlayer,shortdf[1:targetshortrow,])
      firstlayer    <- firstlayer[sample(nrow(firstlayer),replace=FALSE),]       
      # firstlayer should be just slightly longer than calendardur, we'll truncate later

      tdf <- firstlayer # establish target data.frame

      # build a vector of start times
      start <- first(trades$start) + cumsum(as.numeric(tdf$duration))
      # add the first start time back in
      start <- c(first(trades$start), start)
      # take off the last end time, since we won't put in a closing trade
      start <- start[-length(start)]
      # add start column to tdf
      tdf$start <- start
      # rearrange columns for consistency
      tdf <- tdf[, c("start", "duration", "quantity")]
      
      ###########
      # now build the extra layers
      # further layers need to respect flat periods, and long/short.
      # they can 'overlap' existing round turns, even sequential shorter round turns
      # in many cases the layer one simulated portfolios may put multiple
      # trades on in sequence, without intervening flat periods

      # how many layers do we need?
      num_overlaps <- ceiling(totaldur/as.numeric(calendardur))
      
      if(num_overlaps>1){

        ###
        #construct a frame of the first layer for reference
        # construct the cumulative position
        tmpdf <- tdf
        tmpdf$pos <- cumsum(tdf$quantity)
        # construct a vector of end times:
        tmpdf$end <- first(trades$start) + cumsum(as.numeric(tmpdf$duration))
        tmpdf$lsi <- ifelse(tmpdf$pos>0, 1, ifelse(tmpdf$pos<0,-1,0))
        # split remaining longs and shorts by num_overlaps -1  ?
        
        # construct series of random starts
        period <- attr(trades,'period')
        timeseq <- seq.POSIXt( from = attr(trades,"first.start")
                               , to = period$end
                               , by = period$units
        )
        
        # get the range and number of rows remaining of long and short trades
        shortrange <- (targetshortrow+1):nrow(shortdf)
        nshort     <- length(shortrange)
        longrange  <- (targetlongrow+1):nrow(longdf)
        nlong      <- length(longrange)
        
        # construct randomized target starting timestamps for long and short 
        # trades for each layer after the first layer
        sh.samples <- replicate( n = num_overlaps-1
                                 ,expr = sample(x = timeseq, size = nshort, replace = FALSE) )
        names(ln.samples)<-2:num_overlaps
        ln.samples <- replicate( n = num_overlaps-1
                                 ,expr = sample(x = timeseq, size = nlong, replace = FALSE) )
        names(ln.samples)<-2:num_overlaps
        
        layerdfs<-list()
        li <- longrange[1]
        si <- shortrange[1]
        #some challenges:
        #  - each slot in the ln.samples and sh.samples list contains a number 
        #    of timestamps equal to the *total* number of desired long/short trades
        #  - we don't know how many trades should occure on each layer
        #  - we don't really know how the trades were overlapped in the original, 
        #    just the stylized facts.
        #  - the timestamps may not line up with long/short periods
        #  - any 'valid' timestamps, when paired with a trade, may overlap the 
        #    end of a non-flat period
        # 
        # given these challenges, we still need to construct a target series
        # 
        ############
        # proposed process
        ############
        # - loop over layers
        #    - loop over long/short timestamps
        #       - if timestamp occurs in a long/short period
        #            - get the next trade from longdf/shortdf 
        #            - increment li/si so we don't duplicate trades
        #            - if timestamp + trade duration overlaps the next flat period, do we truncate?
        #       - else move to next timestamp
        #    - if we run out of trades, stop
        #    - if on last layer, and we still have trades, find places to put them
        #
        # I think this can construct our layers using the randomized start times

        # now loop over the layers and construct a target data frame for each
        for(laynum in names(ln.samples)){
          ln.ts <- ln.samples[[laynum]]
          sh.ts <- sh.samples[[laynum]]

          layer.trades <- NULL
          
          # loop over the long/short timestamps
          for(lts in 1:length(ln.ts)){
            if(li>max(longrange)) break() # no more trades to process
            test.ts <- ln.ts[lts]
            # get the closest trade from the first layer
            flayer.tn <- last(which(firstlayer$start<=test.ts))
            flayer.trade <- firstlayer[flayer.tn,]
            if(flayer.trade$quantity>0){
              targetend <- test.ts + flayer.trade$duration
              ftend <- flayer.trade$start + flayer.trade$duration
              while(targetend > ftend){
                # we've gone over the duration, check the next trade
                if (firstlayer[flayer.tn+1,'quantity']>0){
                  ftend <- ftend + firstlayer[flayer.tn+1,'duration']
                  if(targetend < ftend){
                    break() # we're good, move on
                  }
                } else {
                  # truncate duration here? probably
                  # break the while loop
                  break()
                }
              } 
              # now we can build the target trade
              if(is.null(layer.trades)){
                layer.trades <- data.frame(start=test.ts,
                                           duration = longdf[li,'duration'],
                                           quantity = longdf[li,'quantity']
                                           )                
              } else {
                layer.trades <- rbind(layer.trades,
                                      data.frame(start=test.ts,
                                                 duration = longdf[li,'duration'],
                                                 quantity = longdf[li,'quantity']
                                                 )
                                      )
              }
              li<-li+1 #increment long index
            } else next() #next may be unecessary if we can avoid more code after this point in longs loop
          }
          #repeat a similar approach for shorts
          for(sts in 1:length(sn.ts)){
            
          }
          
          #now store the result
          layerdfs[[laynum]] <- layer.trades
        }
        layerdfs<-do.call(rbind,layerdfs)
        tdf <- rbind(tdf,layerdfs)
        # @TODO
        # test the samples for long/short
        # test the samples for duration
        # push the samples around so they work with layering
        # for the layers, we need to directly construct the start,quantity, duration format
        # subsequent layers need to be on top of earlier layers, so keep them spearate
        # ??? test for percentage of trades at each layer, and adjust accordingly ???
        # ??? test for maximum position? ???
        
      }
      
      #return the data frame
      tdf
    } # end tradesample.wr inner fn
    
    # outer function over the symbols
    symsample.wr <- function(i) {
      symreps <- replicate(n, tradesample.wr(trades=backtest.trades[[i]]), simplify = FALSE)
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
  portnames <- txnsim.portnames(Portfolio, replacement, n=length(reps[[1]]))
  # create portfolios
  for (i in seq_along(reps[[1]])) {
    # name the simulated portfolio
    simport <- portnames[i]
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
    simport <- portnames[i]
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
    
    prices <- getPrice(get(symbol, pos = env), prefer = prefer)[, 1]
    
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
      df <- df[which(df$duration != 0), ] # remove zero duration trades
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
      txns <- xts(txns[, c("TxnQty", "TxnPrice")], order.by = txns[, 1])
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
    ltxn <- lapply(1:length(reps[[symbol]]), txnsimtxns, symbol = symbol)
  } # end loop over symbols in replicate

  cumpl<-NULL  
  for (i in seq_along(reps[[1]])) {
    # update the simulated portfolio
    p <- portnames[i]
    updatePortf(Portfolio = p, ...)
    # construct the cumulative P&L slot
    if (is.null(cumpl)) {
      cumpl <- cumsum(.getPortfolio(p)$summary$Net.Trading.PL[-1])
    } else {
      cumpl <- cbind(cumpl, cumsum(getPortfolio(p)$summary$Net.Trading.PL[-1]))
    }
  }
  colnames(cumpl) <- portnames
  cumpl <- cumpl[-which(complete.cases(cumpl) == FALSE),] # subset away rows with NA, needed for confidence intervals, quantiles
  backtestpl <- cumsum(.getPortfolio(Portfolio)$summary$Net.Trading.PL[-1])
  colnames(backtestpl) <- Portfolio
  cumpl <- cbind(backtestpl,cumpl)
    
  # generate the return object
  ret <- list(
    replicates = reps,
    transactions = ltxn,
    backtest.trades = backtest.trades,
    cumpl = cumpl,
    initeq = initEq,
    seed = seed,
    call = match.call()
  )
  class(ret) <- "txnsim"
  ret
} # end txnsim fn


#' helper function for generating txnsim portfolio names
#'
#' called internally by txnsim and other txnsim generics to generate list 
#' of portfolios to/which hold the replcates
#' 
#' @param Portfolio root portfolio string name
#' @param replacement boolean
#' @param n number of replicate numbers
#'
#' @return character vector of portfolio names
txnsim.portnames <- function(Portfolio, replacement, n) {
  # name portfolios
  if (isTRUE(replacement)) {
    rpcstr <- 'wr'
  } else {
    rpcstr <- 'nr'
  }
  i <- 1:n
  # NOTE we may still want to clean out existing portfolios, 
  # or allow some other naming options
  portnames <- paste("txnsim", rpcstr, Portfolio, i, sep = ".")
}

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
  rpc <- x$call$replacement
  cumpl <- x$cumpl

  portnames <- txnsim.portnames(Portfolio=port, replacement=rpc , n=n)

  backtestpl <- cumpl[,1]
  
  #TODO FIXME make grid.ticks.on smarter based on periodicity
  pt <- plot.xts(  cumpl
                 , col = "lightgray"
                 , main = paste(port, 'simulation cumulative P&L')
                 , grid.ticks.on = 'years'
                )
  pt <- lines(backtestpl, col = "red")
  print(pt)
  
  invisible(cumpl)
}

#' quantile method for objects of type \code{txnsim}
#'
#' @param x object of type 'txnsim' to produce replicate quantiles
#' @param \dots any other passthrough parameters to \code{\link{quantile}}
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @export
quantile.txnsim <- function(x, ...) {
  ret <- x$cumpl
  q   <- quantile(ret)
  q
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