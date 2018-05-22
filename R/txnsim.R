
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
#' fully as well.  Inclusion of flat periods should provide a fair amount of
#' variation, so this may not be an issue.
#'
#' The user may wish to pass \code{Interval} in dots to mark the portfolio at a
#' different frequency than the market data, especially for intraday market
#' data.  Note that market data must be available to call
#' \code{\link{updatePortf}} on.
#'
#' We are including p-values for some sample statistics in the output as well.
#' Some notes are in order on how this is calculatated, and how it may be
#' interpreted. With small \code{n}, these p-values are meaningless.  With large
#' \code{n}, they should be fairly stable.  Per North et. al. (2002) who use
#' Davison & Hinkley (1997) as their source, the correct unbiased p-value for a
#' simulation sample statistic is:
#'
#' \deqn{ \frac{rank_bt+1}{n_samples+1} }{rank+1/n+1}
#'
#' where the rank of the observed statistic is compared against statistics
#' calculated on the simulation. Interpretation of this result takes some care.
#' The skeptical analyst would prefer to see a low p-value (e.g. the customary
#' 0.05). The same analyst should be concerned about overfitting if an
#' extraordinarily low p-value (e.g. 0.0001) is observed, or conclude that there
#' may be room to improve the strategy is available if the p-value is low but not
#' compelling (e.g. 0.15).  Issues of multiple testing bias should also be
#' considered.  Interpretation of the p-value of the mean is most easily fit into
#' the customary p<0.05 target.  Appropriate critical values for other statistics
#' may be lower or higher.
#'
#' @param Portfolio string identifying a portfolio
#' @param n number of simulations, default = 100
#' @param replacement sample with or without replacement, default TRUE
#' @param tradeDef string to determine which definition of 'trade' to use. See \code{\link{tradeStats}}
#' @param \dots any other passthrough parameters
#' @param CI numeric specifying desired Confidence Interval used in hist.txnsim(), default 0.95
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
#'   \item{\code{Portfolio}:}{ string identifying a portfolio}
#'   \item{\code{n}:}{ number of simulations, default = 100}
#'   \item{\code{replacement}:}{ sample with or without replacement, default TRUE}
#'   \item{\code{samplestats}:}{a numeric dataframe of various statistics for each replicate series}
#'   \item{\code{original}:}{a numeric dataframe of the statistics for the original series}
#'   \item{\code{ranks}:}{a numeric dataframe containing the ranking of the statistics}
#'   \item{\code{pvalues}:}{a numeric dataframe containing the pvalues for the observed backtest compared to the sampled ranks }
#'   \item{\code{stderror}:}{a numeric dataframe of the standard error of the statistics for the replicates}
#'   \item{\code{CI}:}{numeric specifying desired Confidence Interval used in hist.txnsim(), default 0.95}
#'   \item{\code{CIdf}:}{a numeric dataframe of the Confidence Intervals of the statistics for the bootstrapped replicates}
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
#'
#' North, B.V., D. Curtis, and P.C. Sham. Aug 2002. A Note on the Calculation of Empirical P Values from Monte Carlo Procedures. Am J Hum Genet. 2002 Aug; 71(2): 439-441. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC379178/
#'
#' Davison & Hinkley. 1997. Bootstrap methods and their application.
#'
#' @seealso \code{\link{mcsim}}, \code{\link{updatePortf}} , \code{\link{perTradeStats}}, \code{\link{hist.txnsim}}, \code{\link{quantile.txnsim}}
#' @examples
#' \dontrun{
#'
#'  n <- 10
#'
#'  ex.txnsim <- function(Portfolio
#'                         ,n=10
#'                         ,replacement=FALSE
#'                         , tradeDef='increased.to.reduced'
#'                         , chart=FALSE
#'                         )
#'  {
#'     out <- txnsim(Portfolio,n,replacement, tradeDef = tradeDef)
#'     if(isTRUE(chart)) {
#'       portnames <- blotter:::txnsim.portnames(Portfolio, replacement, n)
#'       for (i in 1:n){
#'       p<- portnames[i]
#'         symbols<-names(getPortfolio(p)$symbols)
#'       for(symbol in symbols) {
#'         dev.new()
#'     	   chart.Posn(p,symbol)
#'       }
#'     }
#'   }
#'	 invisible(out)
#'  } # end ex.txnsim
#'
#'   demo('longtrend',ask=FALSE)
#'   lt.nr <- ex.txnsim('longtrend',n, replacement = FALSE)
#'   lt.wr <- ex.txnsim('longtrend',n, replacement = TRUE, chart = TRUE)
#'   plot(lt.wr)
#'   hist(lt.wr)
#'
#'   require('quantstrat') #sorry for the circular dependency
#'   demo('bbands',ask=FALSE)
#'   bb.nr <- ex.txnsim('bbands',n, replacement = FALSE)
#'   bb.wr <- ex.txnsim('bbands',n, replacement = TRUE, chart = TRUE)
#'   plot(rsi.wr)
#'   hist(bb.wr)
#'
#' } #end dontrun
#'
#' @export
txnsim <- function(Portfolio,
                   n = 10,
                   replacement = TRUE,
                   tradeDef = c('increased.to.reduced', 'flat.to.flat', 'flat.to.reduced'),
                   ...,
                   CI = 0.95)
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
    
    # get max long and short positions
    if(any(pt$Max.Pos > 0) == TRUE){
      maxlongpos <- max(pt$Max.Pos)
    }
    if(any(pt$Max.Pos < 0) == TRUE){
      maxshortpos <- min(pt$Max.Pos) # maxshortpos implies maximum absolute short position
    }
    
    # Get duration difference between long start times and short start times
    # We will sample from these when building our replicate layers, however we should
    # only sample from differences in the same flat.to.flat continuous layer of
    # long or short positions.
    if(any(pt$Init.Qty > 0) == TRUE){
      longstartdiff <- diff(pt$Start[-which(pt$Init.Qty == 0)])
      tmp.pt.Init.Qty <- pt$Init.Qty[-which(pt$Init.Qty == 0)]
      signlag <- c(tmp.pt.Init.Qty[-1], 0)
      idx <- sign(signlag * tmp.pt.Init.Qty)
      idx <- ifelse(tmp.pt.Init.Qty < 0, -1, idx)
      # cbind(longstartdiff/86400, tmp.pt.Init.Qty, signlag, idx) # inspect for debugging
      idx <- which(idx > 0)
      longstartdiff <- longstartdiff[idx]
      longstartdiff <- longstartdiff[-which(longstartdiff == 0)] # get rid of zero period durations, dont want to sample those as they will break things downstream
    }
    if(any(pt$Init.Qty < 0) == TRUE){
      shortstartdiff <- diff(pt$Start[-which(pt$Init.Qty == 0)])
      tmp.pt.Init.Qty <- pt$Init.Qty[-which(pt$Init.Qty == 0)]
      signlag <- c(tmp.pt.Init.Qty[-1], 0)
      idx <- sign(signlag * tmp.pt.Init.Qty)
      idx <- ifelse(tmp.pt.Init.Qty > 0, -1, idx)
      # cbind(shortstartdiff/86400, tmp.pt.Init.Qty, signlag, idx) # inspect for debugging
      idx <- which(idx > 0)
      shortstartdiff <- shortstartdiff[idx]
      shortstartdiff <- shortstartdiff[-which(shortstartdiff == 0)] # get rid of zero period durations, dont want to sample those as they will break things downstream
    }
    
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
    
    longcheck <- try(missing(maxlongpos), silent = TRUE)
    shortcheck <- try(missing(maxshortpos), silent = TRUE)
    if (class(longcheck) != "try-error") attr(txnsimdf,"maxlongpos") <- maxlongpos
    if (class(shortcheck) != "try-error") attr(txnsimdf,"maxshortpos") <- maxshortpos
    
    longcheck <- try(missing(maxlongpos), silent = TRUE)
    shortcheck <- try(missing(maxshortpos), silent = TRUE)
    if (class(longcheck) != "try-error") attr(txnsimdf,"longstartdiff") <- longstartdiff
    if (class(shortcheck) != "try-error") attr(txnsimdf,"shortstartdiff") <- shortstartdiff
    
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
    tradesample <- function(trades, replacement=TRUE) {
      
      # fudgefactor is added to size for sampling
      if (isTRUE(replacement)){
        fudgefactor <- 1.1 # fudgefactor is added to size for sampling
      } else {
        fudgefactor <- 1 # no factor needed
      }
      
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
      lsratio     <- as.numeric(longdur)/(as.numeric(longdur) + as.numeric(shortdur))
      if(!is.null(attr(trades, 'maxlongpos'))){
        maxlongpos <- attr(trades, 'maxlongpos')
      }
      if(!is.null(attr(trades, 'maxshortpos'))){
        maxshortpos <- attr(trades, 'maxshortpos')
      }
      if(!is.null(attr(trades, 'maxlongpos'))){
        longstartdiff <- attr(trades, 'longstartdiff')
      }
      if(!is.null(attr(trades, 'maxshortpos'))){
        shortstartdiff <- attr(trades, 'shortstartdiff')
      }
      
      
      subsample <- function(svector, targetdur, replacement=TRUE) {
        #`trades` already exists in function scope
        
        dur <- 0 # initialize duration counter
        tdf <- data.frame() #initialize output data.frame
        nsamples <- round(length(svector) * fudgefactor, 0)
        while (dur < targetdur) {
          s <- sample(svector, nsamples, replace = replacement)
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
      if(flatdur > 0){
        flatdf  <- subsample(svector = flatrows, targetdur = flatdur)
      } else {
        flatdf <- NULL
      }
      if(longdur > 0){ # ie. there are long round turn trades in the strategy
        longdf  <- subsample(svector = longrows, targetdur = longdur)
      } else {
        longdf <- NULL
      }
      if(shortdur > 0){ # ie. there are short round turn trades in the strategy
        shortdf <- subsample(svector = shortrows, targetdur = shortdur)
      } else {
        shortdf <- NULL
      }
      #browser()
      # make the first layer
      # 1. start with flat periods
      firstlayer <- flatdf
      # 2. segment trades for first layer
      targetlongdur <- structure(round((calendardur-flatdur)*lsratio),units='secs',class='difftime')
      if(!is.null(longdf)){ # ie. there are long round turn trades in the strategy
        targetlongrow <- last(which(cumsum(as.numeric(longdf$duration))<targetlongdur))
        firstlayer    <- rbind(firstlayer,longdf[1:targetlongrow,])
      } else {
        targetlongrow <- 0
      }
      # firstlayer    <- rbind(firstlayer,longdf[1:targetlongrow,])
      if(!is.null(shortdf)){ # ie. there are short round turn trades in the strategy
        targetshortrow <- last( which( cumsum(as.numeric(shortdf$duration))<(calendardur-sum(firstlayer$duration)) ) )
        firstlayer     <- rbind(firstlayer,shortdf[1:targetshortrow,])
      } else {
        targetshortrow <- 0
      }
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
      num_overlaps <- ceiling(as.numeric(totaldur)/as.numeric(calendardur))
      
      if(num_overlaps>1){ # ie. total duration > calendar duration
        ###
        # construct a temporary frame of the first layer for reference
        # the rows are the round turns, not a full time series of transactions
        # construct the cumulative position
        tmpdf <- tdf
        # construct a vector of end times:
        tmpdf$end <- first(trades$start) + cumsum(as.numeric(tmpdf$duration))
        tmpdf$lsi <- ifelse(tmpdf$quantity>0, 1, ifelse(tmpdf$quantity<0,-1,0))
        # split remaining longs and shorts by num_overlaps -1  ?
        
        # construct series of random starts
        period <- attr(trades,'period')
        timeseq <- seq.POSIXt( from = attr(trades,"first.start")
                               , to = period$end
                               , by = period$units
        )
        
        # get the range and number of rows remaining of long and short trades
        if(targetshortrow != 0 && targetshortrow < nrow(shortdf)){ # ie. there are short round turn trades in the strategy
          shortrange <- (targetshortrow+1):nrow(shortdf)
          nshort     <- length(shortrange)
        } else {
          shortrange <- 0
          nshort <- 0
        }
        # nshort     <- length(shortrange)
        if(targetlongrow != 0 && targetlongrow < nrow(longdf)){ # ie. there are long round turn trades in the strategy
          longrange  <- (targetlongrow+1):nrow(longdf)
          nlong      <- length(longrange)
        } else {
          longrange <- 0
          nlong <- 0
        }
        # nlong      <- length(longrange)
        
        timesample <- function(timeseq, n, nsample) {
          x <- NULL
          if(is.null(x)) x<-data.frame(sample(x = timeseq, size = nsample, replace = FALSE))
          x
        }
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
        
        # another challenge:
        #  - limiting layering to max long or short pos of the original strategy
        #
        # proposed process
        #
        # - copy the tdf dataframe to tmp_tdf which is simply every sampled txn in the first layer
        # - this first layer dataframe includes 0-qty periods with their durations
        # - the initial variables are start time, duration and quantity
        # - for each of the long and short layering, we follow a fairly methodical approach to 
        #    identifying the first trade in the respective long or short period
        # - because we could end up sampling multiple sequential long or short trades which are all candidates
        #    for layering, we need to keep track of their cumsum relative to the max pos of the long or short
        #    position in the original strategy
        # - we make use of the maxlongpos and maxshortpos stylized facts, stored earlier in our tradesample function
        #    which is only called when tradeDef = "flat.to.reduced" || "increased.to.reduced"
        # - this is because there is no layering in "flat.to.flat"
        #
        # back to tmp_tdf
        # - we add some new columns to tmp_tdf in order to identify which row is the start of a long or short trade,
        #    regardless of how many long or short trades there are in sequence
        # - we use 'lqty' and 'sqty' to identify all long qty and short quantity trades respectively
        # - we use 'ltrade' and 'strade' to assign integer "1" to all long and short trades respectively
        # - we use 'f.ltrade' and 'f.strade' to identify the first long trade and the first short trade in the sequence respectively
        # - when sampling a timestamp for layering, we will make sure that adding the respective quantity does
        #    not take our cumsum position over (under) that of the maxlongpos (maxshortpos) by tracking the cumsum
        #    in the same row as the first long or short trade in the sequence. If adding the new layered quantity takes
        #    us over (under), do nothing. Move on. If we will still be inside, add the sampled row with relevant start
        #    timestamp, duration and quantity. Later on (in the txnsim.txns helper function) we use this data to build
        #    transactions which we add to a portfolio.
        # - we use 'lcumsum' and 'scumsum' to store the cumsum of each long and short trade quantity respectively
        # - we only add to the layer if doing so does not take our cumsum over the maxpos observed in the original strategy
        
        tmp_tdf <- tdf # set up a temp dataframe based on tdf
        
        # cumsum sequential longs on firstlayer
        if(targetlongrow > 0){ # ie. there are long trades in the strategy
          tmp_tdf$lqty <- tmp_tdf$quantity
          tmp_tdf$lqty[which(tmp_tdf$quantity < 0)] <- 0
          tmp_tdf$ltrade <- 0
          tmp_tdf$ltrade[-1] <- ifelse(diff(cumsum(tmp_tdf$lqty)) > 0, tmp_tdf$ltrade[-1] <- 1, 0)
          if(tmp_tdf$lqty[1] > 0) tmp_tdf$ltrade[1] <- 1 else tmp_tdf$ltrade[1] <- 0
          tmp_tdf$f.ltrade <- 0
          tmp_tdf$f.ltrade[-1] <- ifelse(diff(tmp_tdf$ltrade) > 0, tmp_tdf$f.ltrade[-1] <- 1, 0)
          if(tmp_tdf$ltrade[1] == 1) tmp_tdf$f.ltrade[1] <- 1 else tmp_tdf$f.ltrade[1] <- 0
          tmp_tdf$lcumsum <- 0 # tmp_tdf$cumsum for monitoring the cumsum of layered quantity
          lfirstindex <- which(tmp_tdf$f.ltrade == 1) # index of the first long trade in the sequence
          lindex <- which(tmp_tdf$ltrade == 1) # index of all long trades
          interval <- findInterval(lindex,lfirstindex) # count of all duplicates equates to long trades in sequence
          for(cs in 1:(length(lfirstindex)-1)){
            tmp_tdf$lcumsum[lfirstindex[cs]] <- sum(tmp_tdf$lqty[lfirstindex[cs]:lindex[(first(which(interval > cs))-1)]])
          }
          tmp_tdf$lcumsum[lfirstindex[length(lfirstindex)]] <- sum(tmp_tdf$lqty[lfirstindex[cs+1]:last(lindex)])
        }
        
        # cumsum sequential shorts on firstlayer
        if(targetshortrow > 0){ #ie. there are short trades in the strategy
          tmp_tdf$sqty <- tmp_tdf$quantity
          tmp_tdf$sqty[which(tmp_tdf$quantity > 0)] <- 0
          tmp_tdf$strade <- 0
          tmp_tdf$strade[-1] <- ifelse(diff(cumsum(tmp_tdf$sqty)) < 0, tmp_tdf$strade[-1] <- 1, 0)
          if(tmp_tdf$sqty[1] < 0) tmp_tdf$strade[1] <- 1 else tmp_tdf$strade[1] <- 0
          tmp_tdf$f.strade <- 0
          tmp_tdf$f.strade[-1] <- ifelse(diff(tmp_tdf$strade) > 0, tmp_tdf$f.strade[-1] <- 1, 0)
          if(tmp_tdf$strade[1] == 1) tmp_tdf$f.strade[1] <- 1 else tmp_tdf$f.strade[1] <- 0
          tmp_tdf$scumsum <- 0 # tmp_tdf$cumsum for monitoring the cumsum of layered quantity
          sfirstindex <- which(tmp_tdf$f.strade == 1) # index of the first short trade in the sequence
          sindex <- which(tmp_tdf$strade == 1) # index of all short trades
          interval <- findInterval(sindex,sfirstindex) # count of all duplicates equates to short trades in sequence
          for(cs in 1:(length(sfirstindex)-1)){
            tmp_tdf$scumsum[sfirstindex[cs]] <- sum(tmp_tdf$sqty[sfirstindex[cs]:sindex[(first(which(interval > cs))-1)]])
          }
          tmp_tdf$scumsum[sfirstindex[length(sfirstindex)]] <- sum(tmp_tdf$sqty[sfirstindex[cs+1]:last(sindex)])
        }
        
        cumlongdur <- sum(tmp_tdf$duration[which(tmp_tdf$quantity > 0)])
        cumshortdur <- sum(tmp_tdf$duration[which(tmp_tdf$quantity < 0)])
        wlc2 <- 0
        # Use a while loop to build layers until total duration matches x% of target
        # Longs while loop
        layer.trades <- NULL
        while(cumlongdur < (1 * longdur) && wlc2 <= 1000){
          wlc2 <- wlc2 + 1
          # print(wlc2)
          # ln.ts <- sample(timeseq,1) # square brackets converts the output to a POSIXct
          ln.dur <- sample(longstartdiff,1)
          # layer.trades <- NULL
          # test.ts <- ln.ts
          # get the closest trade from the first layer
          # flayer.tn <- last(which(tdf$start<=test.ts))
          flayer.tn <- sample(tdf$start[which(tdf$quantity > 0)], 1)
          flayer.idx <- which(tdf$start == flayer.tn)
          test.ts <- flayer.tn + ln.dur
          if(last(which(tdf$start<=test.ts)) == flayer.idx){
            # flayer.trade <- tdf[flayer.tn,]
            flayer.trade <- tdf[which(tdf$start == flayer.tn),]
            # if(flayer.trade$start == "2013-10-29 02:00:00"){
            #   browser()
            # }
            if(flayer.trade$quantity>0){ # we are going to layer
              # targetend <- test.ts + flayer.trade$duration
              targetend <- test.ts + longdf[li,'duration']
              ftend <- flayer.trade$start + flayer.trade$duration
              txnlongdur <- longdf[li,'duration']
              wlc <- 1 # while loop counter
              while(targetend >= ftend){
                # we've gone over the duration, check the next trade
                # if (!is.na(tdf[flayer.tn+1,'quantity']) && tdf[flayer.tn+1,'quantity']>0){
                if (!is.na(tdf[flayer.idx+wlc,'quantity']) && tdf[flayer.idx+wlc,'quantity']>0){
                  # ftend <- ftend + tdf[flayer.tn+1,'duration']
                  ftend <- ftend + tdf[flayer.idx+wlc,'duration']
                  if(targetend < ftend){
                    txnlongdur <- longdf[li,'duration']
                    break() # we're good, move on
                  } else {
                    wlc <- wlc +1 # we're still over the duration, check the next trade
                  }
                } else {
                  # truncate duration here
                  txnlongdur <- difftime(ftend, test.ts, units = "secs")
                  # longdf[li,'duration']<-difftime(ftend, test.ts, units = "secs")
                  # break the while loop
                  break()
                }
              }
              # now we can build the target trade
              # but first check to make sure the new layered trade will not take our quantity
              # above the maximum observed in the original strategy. If it does, do nothing. If it does
              # take us over, build the trade.
              idx <- last(which(tmp_tdf$start<=test.ts & tmp_tdf$f.ltrade == 1))
              if(length(idx) == 0){ # idx is "an argument of length zero"
                idx <- flayer.tn # set idx = flayer.tn so we have a valid value with which to subset
              }
              if ((tmp_tdf$lcumsum[idx] + longdf[li,'quantity']) <= maxlongpos){ # build target trade
                if(is.null(layer.trades)){
                  layer.trades <- data.frame(start=test.ts,
                                             duration = txnlongdur,
                                             quantity = longdf[li,'quantity']
                  )
                } else {
                  layer.trades <- rbind(layer.trades,
                                        data.frame(start=test.ts,
                                                   duration = txnlongdur,
                                                   quantity = longdf[li,'quantity']
                                        )
                  )
                }
                tmp_tdf$lcumsum[idx] <- tmp_tdf$lcumsum[idx] + longdf[li,'quantity'] # add new layered quantity for comparison to maxlongpos in next applicable loop, if any
                # cumlongdur <- cumlongdur + longdf[li,'duration']
                cumlongdur <- cumlongdur + txnlongdur
                # li<-li+1 #increment long index
                li <- sample(longrange, 1)
              } else if(tmp_tdf$lcumsum[idx] < maxlongpos){
                if(is.null(layer.trades)){
                  layer.trades <- data.frame(start=test.ts,
                                             duration = txnlongdur,
                                             quantity = maxlongpos - tmp_tdf$lcumsum[idx]
                  )
                } else {
                  layer.trades <- rbind(layer.trades,
                                        data.frame(start=test.ts,
                                                   duration = txnlongdur,
                                                   quantity = maxlongpos - tmp_tdf$lcumsum[idx]
                                        )
                  )
                }
                tmp_tdf$lcumsum[idx] <- tmp_tdf$lcumsum[idx] + longdf[li,'quantity'] # add new layered quantity for comparison to maxlongpos in next applicable loop, if any
                # cumlongdur <- cumlongdur + longdf[li,'duration']
                cumlongdur <- cumlongdur + txnlongdur
                # li<-li+1 #increment long index
                li <- sample(longrange, 1)
              }
            } else next() #next may be unecessary if we can avoid more code after this point in longs loop
          }
          
        }# end long layering
        
        
        # Shorts while loop
        wsc2 <- 0
        while(cumshortdur < (1 * shortdur && wsc2 <= 1000)){
          wsc2 <- wsc2 + 1
          # sh.ts <- sample(timeseq,1)
          sh.dur <- sample(shortstartdiff,1)
          # test.ts <- sh.ts
          # get the closest trade from the first layer
          # flayer.tn <- last(which(tdf$start<=test.ts))
          flayer.tn <- sample(tdf$start[which(tdf$quantity < 0)], 1)
          flayer.idx <- which(tdf$start == flayer.tn)
          test.ts <- flayer.tn + sh.dur
          if(last(which(tdf$start<=test.ts)) != flayer.idx){
            # flayer.trade <- tdf[flayer.tn,]
            flayer.trade <- tdf[which(tdf$start == flayer.tn),]
            if(flayer.trade$quantity<0){
              # targetend <- test.ts + flayer.trade$duration
              targetend <- test.ts + shortdf[si,'duration']
              ftend <- flayer.trade$start + flayer.trade$duration
              txnshortdur <- shortdf[si,'duration']
              wlc <- 1 # while loop counter
              while(targetend >= ftend){
                # we've gone over the duration, check the next trade
                # if (!is.na(tdf[flayer.tn+1,'quantity']) && tdf[flayer.tn+1,'quantity']<0){
                if (!is.na(tdf[flayer.idx+wlc,'quantity']) && tdf[flayer.idx+wlc,'quantity']<0){
                  # ftend <- ftend + tdf[flayer.tn+1,'duration']
                  ftend <- ftend + tdf[flayer.idx+wlc,'duration']
                  if(targetend < ftend){
                    txnshortdur <- shortdf[si,'duration']
                    break() # we're good, move on
                  } else {
                    wlc <- wlc +1 # we're still over the duration, check the next trade
                  }
                } else {
                  # truncate duration here
                  # shortdf[si,'duration']<-difftime(ftend, test.ts, units = "secs")
                  txnshortdur <- difftime(ftend, test.ts, units = "secs")
                  # break the while loop
                  break()
                }
              }
              # now we can build the target trade
              # but first check to make sure the new layered trade will not take our quantity
              # above the maximum observed in the original strategy. If it does, do nothing. If it does
              # take us over, build the trade.
              idx <- last(which(tmp_tdf$start<=test.ts & tmp_tdf$f.strade == 1))
              if(length(idx) == 0){ # idx is "an argument of length zero"
                idx <- flayer.tn # set idx = flayer.tn so we have a valid value with which to subset
              }
              if ((tmp_tdf$scumsum[idx] + shortdf[si,'quantity']) >= maxshortpos){ # build target trade
                if(is.null(layer.trades)){
                  layer.trades <- data.frame(start=test.ts,
                                             duration = txnshortdur,
                                             quantity = shortdf[si,'quantity']
                  )
                } else {
                  layer.trades <- rbind(layer.trades,
                                        data.frame(start=test.ts,
                                                   duration = txnshortdur,
                                                   quantity = shortdf[si,'quantity']
                                        )
                  )
                }
                tmp_tdf$scumsum[idx] <- tmp_tdf$scumsum[idx] + shortdf[si,'quantity'] # add new layered quantity for comparison to maxshortpos in next applicable loop, if any
                # cumshortdur <- cumshortdur + shortdf[si,'duration']
                cumshortdur <- cumshortdur + txnshortdur
                # si<-si+1 #increment short index
                si <- sample(shortrange, 1)
              } else if(tmp_tdf$scumsum[idx] > maxshortpos){
                if(is.null(layer.trades)){
                  layer.trades <- data.frame(start=test.ts,
                                             duration = txnshortdur,
                                             quantity = maxshortpos - tmp_tdf$scumsum[idx]
                  )
                } else {
                  layer.trades <- rbind(layer.trades,
                                        data.frame(start=test.ts,
                                                   duration = txnshortdur,
                                                   quantity = maxshortpos - tmp_tdf$scumsum[idx]
                                        )
                  )
                }
                tmp_tdf$scumsum[idx] <- tmp_tdf$scumsum[idx] + shortdf[si,'quantity'] # add new layered quantity for comparison to maxshortpos in next applicable loop, if any
                # cumshortdur <- cumshortdur + shortdf[si,'duration']
                cumshortdur <- cumshortdur + txnshortdur
                # si<-si+1 #increment short index
                si <- sample(shortrange, 1)
              }
            } else next() #next may be unecessary if we can avoid more code after this point in shorts loop
          }
        }# end short layering
        
        #now store the result
        layerdfs <- layer.trades
        # layerdfs<-do.call(rbind,layerdfs)
        tdf <- rbind(tdf,layerdfs)
        # @TODO
        # double check that all long and short trades have been allocated, do something if not
        # ??? test for percentage of trades at each layer, and adjust accordingly ???
        # ??? test for maximum position? ???
      }
      #return the data frame
      tdf
    } # end tradesample.wr inner fn
    
    # outer function over the symbols
    symsample <- function(i) {
      symreps <- replicate(n, tradesample(trades=backtest.trades[[i]], replacement=replacement), simplify = FALSE)
    }
    reps <- lapply(1:length(symbols), symsample )
    names(reps) <- symbols
    
  } # end flat.to.reduced/increased.to.reduced method
  
  ################################################################
  # reps now exists as a list of structure reps[[symbol]][[rep]]
  # each rep has columns start, duration, quantity
  
  # ####################
  # # Generate Transactions
  # create the portfolios
  portnames <- txnsim.portnames(Portfolio, replacement, n)
  txnsim.portfs( Portfolio=Portfolio
                 , replacement=replacement
                 , n=length(reps[[1]])
                 , symbols=symbols
                 , initDate=initDate
                 , currency=currency
                 , initEq=initEq
                 , ...
  )
  
  # create the transactions
  ltxn <- txnsim.txns( reps = reps
                       , Portfolio=Portfolio
                       , replacement=replacement
                       , n=length(reps[[1]])
                       , ...
  )
  
  cumpl<-NULL
  perpl<-NULL
  for (i in seq_along(reps[[1]])) {
    # update the simulated portfolio
    p <- portnames[i]
    updatePortf(Portfolio = p, ...)
    # construct the cumulative P&L slot
    if (is.null(cumpl)) {
      perpl <- .getPortfolio(p)$summary$Net.Trading.PL[-1]
      cumpl <- cumsum(.getPortfolio(p)$summary$Net.Trading.PL[-1])
    } else {
      perpl <- cbind(perpl, .getPortfolio(p)$summary$Net.Trading.PL[-1])
      cumpl <- cbind(cumpl, cumsum(getPortfolio(p)$summary$Net.Trading.PL[-1]))
    }
  }
  
  colnames(perpl) <- portnames
  colnames(cumpl) <- portnames
  
  # add the observed portfolio here for comparison
  backtestperpl <- .getPortfolio(Portfolio)$summary$Net.Trading.PL[-1]
  colnames(backtestperpl) <- Portfolio
  backtestpl <- cumsum(backtestperpl)
  cumpl <- cbind(backtestpl,cumpl)
  perpl <- cbind(backtestperpl,perpl)
  
  if(any(is.na(cumpl))){
    cumpl <- cumpl[-which(complete.cases(cumpl) == FALSE),] # subset away rows with NA, needed for confidence intervals, quantiles
  }
  
  # compute sample stats
  sampleoutput <- data.frame(matrix(nrow = n+1, ncol = 6))
  colnames(sampleoutput) <- c("mean","median","stddev","maxDD","sharpe","totalPL")
  sampleoutput$mean    <- apply(perpl, 2, function(x) { mean(x, na.rm=TRUE) } )
  sampleoutput$median  <- apply(perpl, 2, function(x) { median(x, na.rm=TRUE) } )
  sampleoutput$stddev  <- apply(perpl, 2, function(x) { StdDev(x) } )
  sampleoutput$maxDD   <- apply(perpl, 2, function(x) { -max(cummax(cumsum(na.omit(x)))-cumsum(na.omit(x))) } )
  sampleoutput$sharpe  <- apply(perpl, 2, function(x) { mean(x, na.rm=TRUE)/StdDev(x) } )
  sampleoutput$totalPL <- apply(perpl, 2, function(x) { sum(na.omit(x)) } )
  rownames(sampleoutput)<-colnames(perpl)
  
  # store stats for use later in hist.mcsim and summary.mcsim
  original <- sampleoutput[1,]
  
  
  # compute p-values
  ranks <- apply(-sampleoutput,2,rank)
  # correct calc for unbiased p-value is rank+1/nsamples+1
  # where rank is rank of the sample statistic of the observation vs. samples
  # we've included the observed series in the sample, so the correct calc
  # is rank/nsamples
  pvalues <- ranks[1,]/nrow(ranks)
  sigd    <- nchar(n+1)
  pvalues <- round(pvalues, digits=sigd )
  
  # Compute standard errors of the sample stats
  stderror <- data.frame(matrix(nrow = 1, ncol = 6))
  colnames(stderror) <- c("mean","median","stddev","maxDD","sharpe","totalPL")
  row.names(stderror) <- "Std. Error"
  stderror$mean <- StdDev(sampleoutput[,1])
  stderror$median <- StdDev(sampleoutput[,2])
  stderror$stddev <- StdDev(sampleoutput[,3])
  stderror$maxDD <- StdDev(sampleoutput[,4])
  stderror$sharpe <- StdDev(sampleoutput[,5])
  stderror$totalPL <- StdDev(sampleoutput[,6])
  
  # Compute Confidence Intervals, but first add the CI functions
  CI_lower <- function(samplemean, merr) {
    #out <- original - bias - merr #based on boot package implementation in norm.ci
    out <- samplemean - merr #more generic implementation
    out
  }
  CI_upper <- function(samplemean, merr) {
    #out <- original - bias + merr #based on boot package implementation in norm.ci
    out <- samplemean + merr #more generic implementation
    out
  }
  CI_mean <- cbind(CI_lower(mean(sampleoutput[,1]), StdDev(sampleoutput[,1])*qnorm((1+CI)/2)),
                   CI_upper(mean(sampleoutput[,1]), StdDev(sampleoutput[,1])*qnorm((1+CI)/2)))
  
  CI_median <- cbind(CI_lower(mean(sampleoutput[,2]), StdDev(sampleoutput[,2])*qnorm((1+CI)/2)),
                     CI_upper(mean(sampleoutput[,2]), StdDev(sampleoutput[,2])*qnorm((1+CI)/2)))
  
  CI_stddev <- cbind(CI_lower(mean(sampleoutput[,3]), StdDev(sampleoutput[,3])*qnorm((1+CI)/2)),
                     CI_upper(mean(sampleoutput[,3]), StdDev(sampleoutput[,3])*qnorm((1+CI)/2)))
  
  CI_maxDD <- cbind(CI_lower(mean(sampleoutput[,4]), StdDev(sampleoutput[,4])*qnorm((1+CI)/2)),
                    CI_upper(mean(sampleoutput[,4]), StdDev(sampleoutput[,4])*qnorm((1+CI)/2)))
  
  CI_sharpe <- cbind(CI_lower(mean(sampleoutput[,5]), StdDev(sampleoutput[,5])*qnorm((1+CI)/2)),
                     CI_upper(mean(sampleoutput[,5]), StdDev(sampleoutput[,5])*qnorm((1+CI)/2)))
  
  CI_totalPL <- cbind(CI_lower(mean(sampleoutput[,6]), StdDev(sampleoutput[,6])*qnorm((1+CI)/2)),
                      CI_upper(mean(sampleoutput[,6]), StdDev(sampleoutput[,6])*qnorm((1+CI)/2)))
  
  # Build the Confidence Interval dataframes
  CIdf <- data.frame(matrix(nrow = 2, ncol = 6))
  colnames(CIdf) <- c("mean","median","stddev","maxDD","sharpe","totalPL")
  row.names(CIdf) <- c("Lower CI","Upper CI")
  CIdf$mean[row.names(CIdf) == "Lower CI"] <- CI_mean[1,1]
  CIdf$mean[row.names(CIdf) == "Upper CI"] <- CI_mean[1,2]
  
  CIdf$median[row.names(CIdf) == "Lower CI"] <- CI_median[1,1]
  CIdf$median[row.names(CIdf) == "Upper CI"] <- CI_median[1,2]
  
  CIdf$stddev[row.names(CIdf) == "Lower CI"] <- CI_stddev[1,1]
  CIdf$stddev[row.names(CIdf) == "Upper CI"] <- CI_stddev[1,2]
  
  CIdf$maxDD[row.names(CIdf) == "Lower CI"] <- CI_maxDD[1,1]
  CIdf$maxDD[row.names(CIdf) == "Upper CI"] <- CI_maxDD[1,2]
  
  CIdf$sharpe[row.names(CIdf) == "Lower CI"] <- CI_sharpe[1,1]
  CIdf$sharpe[row.names(CIdf) == "Upper CI"] <- CI_sharpe[1,2]
  
  CIdf$totalPL[row.names(CIdf) == "Lower CI"] <- CI_totalPL[1,1]
  CIdf$totalPL[row.names(CIdf) == "Upper CI"] <- CI_totalPL[1,2]
  
  # generate the return object
  ret <- list(
    replicates = reps,
    transactions = ltxn,
    backtest.trades = backtest.trades,
    perpl = perpl,
    cumpl = cumpl,
    initeq = initEq,
    seed = seed,
    call = match.call(),
    replacement = replacement,
    Portfolio = Portfolio,
    n = n,
    samplestats=sampleoutput,
    original=original,
    ranks=ranks,
    pvalues=pvalues,
    stderror=stderror,
    CI=CI,
    CIdf=CIdf
  )
  class(ret) <- "txnsim"
  ret
} # end txnsim fn


#' convenience function to generate portfolios for txnsim replicates
#'
#' If you have a txnsim object and market data, you should be able to rebuild
#' the replicate portfolios. This function creates all those portfolios.
#'
#' @param Portfolio string identifying a portfolio
#' @param n number of simulations, default = 100
#' @param replacement sample with or without replacement, default TRUE
#' @param symbols character vector of symbol names
#' @param initDate initialization Date to use for replicate portfolios
#' @param currency base currency for replicate portfolios
#' @param initEq initial equity to use for replicate portfolios
#'
#' @seealso \code{\link{txnsim}}, \code{\link{txnsim.txns}}
txnsim.portfs <- function(Portfolio, replacement, n, symbols, initDate, currency, initEq) {
  portnames <- txnsim.portnames(Portfolio, replacement, n)
  # create portfolios
  for (i in 1:n) {
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
}

#' convenience function to create transactions from txnsim replicates
#'
#' If you have a txnsim object and market data, you should be able to rebuild
#' the replicate portfolios.
#'
#' @param reps replicates slot from txnsim object
#' @param Portfolio string identifying a portfolio
#' @param replacement sample with or without replacement, default TRUE
#' @param n number of simulations, default = 100
#' @param \dots any other passthrough parameters, most usefully \code{env} and \code{prefer}
#'
#' @return a list by symbol of txns suitable for calling \code{\link{addTxns}}
#'
#' @seealso \code{\link{txnsim}}, \code{\link{txnsim.txns}} , \code{\link{addTxns}}
txnsim.txns <- function (reps, Portfolio, replacement, n, ...) {
  portnames <- txnsim.portnames(Portfolio, replacement, n)
  ltxn <- list()
  for (symbol in names(reps)){
    ltxn[[symbol]] <- lapply(1:length(reps[[symbol]]), function(i) {
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
        # Build opening transaction dataframe
        txns_open <- data.frame(matrix(nrow = nrow(df), ncol = 3))
        df <- df[order(df[,1]),] # when layering, we have older order timestamps from layer 2+ after more recent timestamps from layer 1
        idx_open <- findInterval(df[,1],index(prices))
        txns_open[,1] <- index(prices)[idx_open]
        txns_open[,2] <- df[, "quantity"]
        txns_open[,3] <- as.numeric(prices[idx_open])
        # Build closing transaction dataframe
        txns_close <- data.frame(matrix(nrow = nrow(df), ncol = 3))
        idx_close <- findInterval(df[,1]+df[, "duration"],index(prices))
        txns_close[,1] <- index(prices)[idx_close]
        txns_close[,2] <- -1 * df[, "quantity"]
        txns_close[,3] <- as.numeric(prices[idx_close])
        # Combine open and close dataframes, order, rename columns, remove zero quantity transactions
        txns <- rbind(txns_close, txns_open) # bind open txns below close txns so txns on same timestamp are in correct order
        txns <- xts(txns[,-1], order.by = txns[, 1])
        colnames(txns) <- (c("TxnQty", "TxnPrice"))
        txns <- txns[which(txns$TxnQty != 0), ]
        txnlist[[li]] <- txns
      }
      txns <- do.call(rbind, txnlist)
      addTxns(Portfolio = simport,
              Symbol = symbol,
              TxnData = txns)
      txns # return the data for later use
    })
  }
  ltxn # return the transaction list for later use
}

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
  return(portnames)
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
  cumpl <- x$cumpl
  
  backtestpl <- cumpl[,1]
  
  #TODO FIXME make grid.ticks.on smarter based on periodicity
  pt <- plot.xts(  cumpl
                   , col = "lightgray"
                   , main = paste(x$Portfolio, 'txnsim cumulative P&L',x$n,'reps. with replace=',x$replacement)
                   , grid.ticks.on = 'years'
  )
  pt <- lines(backtestpl, col = "red")
  print(pt)
  
  invisible(cumpl)
}

#' quantile method for objects of type \code{txnsim}
#'
#' calculates quantiles of cumulative P&L of the simulated strategies
#'
#' @param x object of type 'txnsim' to produce replicate quantiles
#' @param \dots any other passthrough parameters to \code{\link{quantile}}
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @export
quantile.txnsim <- function(x, ...) {
  ret <- x$cumpl
  q   <- quantile(na.omit(ret))
  q
}

#' hist method for objects of type \code{txnsim}
#'
#' @param x object of type txnsim to plot
#' @param \dots any other passthrough parameters
#' @param normalize TRUE/FALSE whether to normalize the hist by div, default FALSE as no normalized data yet
#' @param methods are statistics to include in hist output, default methods=c("mean","median","stddev","maxDD","sharpe")
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @importFrom graphics axis box hist lines par text
#'
#' @export
hist.txnsim <- function(x, ..., normalize=FALSE,
                        methods = c("mean",
                                    "median",
                                    "stddev",
                                    "maxDD",
                                    "sharpe")) {
  ret <- x
  hh <- function(x, main, breaks="FD"
                 , xlab, ylab = "Density"
                 , col = "lightgray", border = "white", freq=FALSE, ...
                 , b, b.label, v, c.label, t, u, u.label, ci_L,ci_H, tci_L="Lower Confidence Interval", tci_H="Upper Confidence Interval"
  ){
    
    hhh <- hist(x, main=main, breaks=breaks, xlab=xlab, ylab=ylab, col=col, border=border, freq=freq, cex.main=0.70)
    hhh
    box(col = "darkgray")
    abline(v = b, col = "red", lty = 2)
    b.label = b.label
    hhh = rep(0.2 * par("usr")[3] + 1 * par("usr")[4], length(b))
    text(b, hhh/1.85, b.label, offset = 0.4, pos = 2, cex = 0.8, srt = 90, col = "red")
    abline(v = v, col = "darkgray", lty = 2)
    c.label = c.label
    text(t, hhh/1.85, c.label, offset = 0.4, pos = 2, cex = 0.8, srt = 90)
    abline(v = ci_L, col="blue", lty=2)
    text(ci_L, hhh, tci_L, offset = 0.4, pos = 2, cex = 0.8, srt = 90, col="blue")
    abline(v = ci_H, col="blue", lty=2)
    text(ci_H, hhh, tci_H, offset = 0.4, pos = 2, cex = 0.8, srt = 90, col="blue")
    abline(v = u, col="blue", lty=2)
    text(u, hhh, u.label, offset = 0.4, pos = 2, cex = 0.8, srt = 90, col="blue")
    hhh
  }
  if(isTRUE(normalize) && ret$initeq>1) {
    xname <- paste(ret$n, "replicates", ret$w, "using", ret$CI, "confidence interval")
    h <- NULL
    for (method in methods) {
      switch (method,
              mean = {
                hh(ret$percsamplestats$mean, paste("Mean distribution of" , xname)
                   , xlab="Mean Return"
                   , b = ret$percoriginal$mean
                   , b.label = ("Backtest Mean Return")
                   , v = median(na.omit(ret$percsamplestats$mean))
                   , c.label = ("Simulation Median Return")
                   , t = median(na.omit(ret$percsamplestats$mean))
                   , u.label = ("Simulation Mean Return")
                   , u = mean(na.omit(ret$percsamplestats$mean))
                   , ci_L = ret$CIdf_perc$mean[1]
                   , ci_H = ret$CIdf_perc$mean[2]
                )
              },
              median = {
                hh(ret$percsamplestats$median, paste("Median distribution of", xname)
                   , xlab="Median Return"
                   , b = ret$percoriginal$median
                   , b.label = ("Backtest Median Return")
                   , v = median(na.omit(ret$percsamplestats$median))
                   , c.label = ("Simulation Median Return")
                   , t = median(na.omit(ret$percsamplestats$median))
                   , u.label = ("Simulation Mean Return")
                   , u = mean(na.omit(ret$percsamplestats$median))
                   , ci_L = ret$CIdf_perc$median[1]
                   , ci_H = ret$CIdf_perc$median[2]
                )
              },
              stddev = {
                hh(ret$percsamplestats$stddev, paste("Std Dev distribution of" , xname)
                   , xlab="stddev"
                   , b = ret$percoriginal$stddev
                   , b.label = ("Backtest Std Dev")
                   , v = median(na.omit(ret$percsamplestats$stddev))
                   , c.label = ("Simulation Median Std Dev")
                   , t = median(na.omit(ret$percsamplestats$stddev))
                   , u.label = ("Simulation Mean Std Dev")
                   , u = mean(na.omit(ret$percsamplestats$stddev))
                   , ci_L = ret$CIdf_perc$stddev[1]
                   , ci_H = ret$CIdf_perc$stddev[2]
                )
              },
              maxDD = {
                hh(ret$percsamplestats$maxDD, paste("maxDrawdown distribution of" , xname)
                   , xlab="Max Drawdown"
                   , b = ret$percoriginal$maxDD
                   , b.label = ("Backtest Max Drawdown")
                   , v = median(na.omit(ret$percsamplestats$maxDD))
                   , c.label = ("Simulation Median Max Drawdown")
                   , t = median(na.omit(ret$percsamplestats$maxDD))
                   , u.label = ("Simulation Mean Max Drawdown")
                   , u = mean(na.omit(ret$percsamplestats$maxDD))
                   , ci_L = ret$CIdf_perc$maxDD[1]
                   , ci_H = ret$CIdf_perc$maxDD[2]
                )
              },
              sharpe = {
                hh(ret$percsamplestats$sharpe, paste("quasi-Sharpe distribution of" , xname)
                   , xlab="quasi-sharpe"
                   , b = ret$percoriginal$sharpe
                   , b.label = ("Backtest quasi-Sharpe ratio")
                   , v = median(na.omit(ret$percsamplestats$sharpe))
                   , c.label = ("Simulation Median quasi-Sharpe ratio")
                   , t = median(na.omit(ret$percsamplestats$sharpe))
                   , u.label = ("Simulation Mean quasi-Sharpe ratio")
                   , u = mean(na.omit(ret$percsamplestats$sharpe))
                   , ci_L = ret$CIdf_perc$sharpe[1]
                   , ci_H = ret$CIdf_perc$sharpe[2]
                )
              }
      )
    }
    
  } else {
    # do not normalize
    xname <- paste(ret$n, "replicates", ret$w, "using", ret$CI, "confidence interval")
    h <- NULL
    for (method in methods) {
      switch (method,
              mean = {
                hh(ret$samplestats$mean, paste("Mean distribution of" , xname)
                   , xlab="Mean Return"
                   , b = ret$original$mean
                   , b.label = ("Backtest Mean Return")
                   , v = median(na.omit(ret$samplestats$mean))
                   , c.label = ("Simulation Median Return")
                   , t = median(na.omit(ret$samplestats$mean))
                   , u.label = ("Simulation Mean Return")
                   , u = mean(na.omit(ret$samplestats$mean))
                   , ci_L = ret$CIdf$mean[1]
                   , ci_H = ret$CIdf$mean[2]
                )
              },
              median = {
                hh(ret$samplestats$median, paste("Median distribution of" , xname)
                   , xlab="Median Return"
                   , b = ret$original$median
                   , b.label = ("Backtest Median Return")
                   , v = median(na.omit(ret$samplestats$median))
                   , c.label = ("Simulation Median Return")
                   , t = median(na.omit(ret$samplestats$median))
                   , u.label = ("Simulation Mean Return")
                   , u = mean(na.omit(ret$samplestats$median))
                   , ci_L = ret$CIdf$median[1]
                   , ci_H = ret$CIdf$median[2]
                )
              },
              stddev = {
                hh(ret$samplestats$stddev, paste("Std Dev distribution of" , xname)
                   , xlab="stddev"
                   , b = ret$original$stddev
                   , b.label = ("Backtest Std Dev")
                   , v = median(na.omit(ret$samplestats$stddev))
                   , c.label = ("Simulation Median Std Dev")
                   , t = median(na.omit(ret$samplestats$stddev))
                   , u.label = ("Simulation Mean Std Dev")
                   , u = mean(na.omit(ret$samplestats$stddev))
                   , ci_L = ret$CIdf$stddev[1]
                   , ci_H = ret$CIdf$stddev[2]
                )
              },
              maxDD = {
                hh(ret$samplestats$maxDD, paste("maxDrawdown distribution of" , xname)
                   , xlab="Max Drawdown"
                   , b = ret$original$maxDD
                   , b.label = ("Backtest Max Drawdown")
                   , v = median(na.omit(ret$samplestats$maxDD))
                   , c.label = ("Simulation Median Max Drawdown")
                   , t = median(na.omit(ret$samplestats$maxDD))
                   , u.label = ("Simulation Mean Max Drawdown")
                   , u = mean(na.omit(ret$samplestats$maxDD))
                   , ci_L = ret$CIdf$maxDD[1]
                   , ci_H = ret$CIdf$maxDD[2]
                )
              },
              sharpe = {
                hh(ret$samplestats$sharpe, paste("quasi-Sharpe distribution of" , xname)
                   , xlab="quasi-sharpe"
                   , b = ret$original$sharpe
                   , b.label = ("Backtest quasi-Sharpe ratio")
                   , v = median(na.omit(ret$samplestats$sharpe))
                   , c.label = ("Simulation Median quasi-Sharpe ratio")
                   , t = median(na.omit(ret$samplestats$sharpe))
                   , u.label = ("Simulation Mean quasi-Sharpe ratio")
                   , u = mean(na.omit(ret$samplestats$sharpe))
                   , ci_L = ret$CIdf$sharpe[1]
                   , ci_H = ret$CIdf$sharpe[2]
                )
              }
      )
    }
  }
}



#' summary and print methods for objects of type txnsim
#'
#' @param x an object of type txnsim
#' @param object an object of type txnsim
#' @param ... any other passthrough parameters
#'
#' @method summary txnsim
#' @export
summary.txnsim <- function(object,...){
  out<-t(rbind(object$original,object$stderror,object$CIdf))
  colnames(out)[1]<-'backtest'
  out
}

#' @rdname summary.txnsim
#' @method print txnsim
#' @export
print.txnsim <- function(x,...){
  round(summary.txnsim(x,...),3)
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
