#' Monte Carlo simulate strategy results
#'
#' This function resamples the daily transaction, cash equity, or percent-return
#' P&L from a portfolio of trading results.  It may be applied to both real
#' (post-trade) and backtested transactions.
#'
#' The general argument here is that you can compare the performance of real
#' portfolio equity or a backtest equity curve to a sampled version of the same.
#'
#' We've chosen to use daily frequency for the resampling period. If your holding
#' period is longer than one day, on average, the samples will increase
#' variability in the overall path.  If your average holding period is shorter
#' than a day, the \code{\link{mcsim}} function will still provide a useful
#' benchmark for comparing to other strategies, but you may additionally wish to
#' sample round turn trades, as provided in \code{\link{TODO_ADD_ME}}.
#'
#' A few of the arguments and methods probably deserve more discussion as well.
#'
#' \code{use} describes the method to use to generate the initial daily P\&L to
#' be sampled from.  The options are:
#' \itemize{
#'   \item{equity}{will use daily portfolio cash P&L}
#'   \item{txn}{will use transaction \code{Net.Trading.PL}}
#'   \item{returns}{will use \code{\link{PortfReturns} to generate percent returns}}
#' }
#'
#' Sampling may be performed either with or without replacement.
#' \itemize{
#'   \item{without replacement}{If sampled **without** replacement, the replicated
#'   equity curves will use all the same data as the input series, only reordered.
#'   This will lead to all the replicates having identical final P\&L and mean
#'   returns, but different paths along the way.}
#'   \item{with replacement}{If sampled **with** replacement, individual
#'   observations may be re-used.  This will tend to create more variability than
#'   replicates without replacement.}
#' }
#'
#' If the post-trade or backtested equity curve exhibits autocorrelation, runs,
#' streaks, etc. it may be advantageous to utilize a block resampling method.
#' The length of the block "\code{l}" may be fixed or variable.
#' If a \code{varblock} method is used, the distribution of block lengths will
#' be uniform random for \code{replacement=FALSE} and geometric random for
#' \code{replacement=TRUE}.  By sampling blocks, the resampled returns will
#' contain more of the structure of the original series.  If \code{varblock=TRUE},
#' the blocks will be of variable length, centered around \code{l}.
#'
#' @param Portfolio string identifier of portfolio name
#' @param Account string identifier of account name
#' @param n number of simulations, default = 1000
#' @param replacement sample with or without replacement, default TRUE
#' @param \dots any other passthrough parameters
#' @param use determines whether to use 'equity', 'txn', or 'returns' P\&L, default = "equity" ie. daily
#' @param l block length, default = 1
#' @param varblock boolean to determine whether to use variable block length, default FALSE
#' @param gap numeric number of periods from start of series to start on, to eliminate leading NA's
#' @return a list containing:
#' \itemize{
#'   \item{\code{replicates}:}{an xts object containing all the resampled time series replicates}
#'   \item{\code{dailypl}:}{an xts object containing daily P&L from the original backtest}
#'   \item{\code{initeq}:}{a numeric variable containing the initEq of the portfolio, for starting portfolio value}
#'   \item{\code{num}:}{a numeric variable reporting the number of replicaes in the simulation}
#'   \item{\code{length}:}{a numeric variable reporting the block length used in the simulation, if any}
#'   \item{\code{ddvec}:}{a numeric vector of drawdowns for each replicate series}
#'   \item{\code{w}:}{a string containing information on whether the simulation is with or without replacement}
#'   \item{\code{use}:}{ a string with the value of the 'use' parameter, for checking later}
#'   \item{\code{call}:}{an object of type \code{call} that contains the \code{mcsim} call}
#' }
#'
#' Note that this object and its slots may change in the future.
#' Slots \code{replicates},\code{dailypl},\code{initeq}, and \code{call} are likely
#' to exist in all future versions of this function, but other slots may be added
#' and removed as \code{S3method}'s are developed.
#'
#' @note
#' Requires boot package
#' @importFrom boot tsboot boot.array
#' @importFrom foreach foreach %dopar%
#' @author Jasen Mackie, Brian G. Peterson
#' @seealso
#'   \code{\link{boot}}
#'   \code{\link{plot.mcsim}}
#'   \code{\link{hist.mcsim}}
#' @examples
#' \dontrun{
#'
#' demo('longtrend', ask=FALSE)
#'
#' nrsim <- mcsim("longtrend", "longtrend", n=1000, replacement=FALSE, l=1, gap=10)
#' nrblocksim <- mcsim("longtrend", "longtrend", n=1000, replacement=FALSE, l=10, gap=10)
#' rsim <- mcsim("longtrend", "longtrend", n=1000, replacement=TRUE, l=1, gap=10)
#' varsim <- mcsim("longtrend", "longtrend", n=1000, replacement=TRUE, l=10, varblock=TRUE, gap=10)
#' nrvarsim <- mcsim("longtrend", "longtrend", n=1000, replacement=FALSE, l=10, varblock=TRUE, gap=10)
#'
#' quantile(varsim)
#' quantile(nrsim)
#' quantile(nrvarsim)
#'
#' plot(nrsim, normalize=TRUE)
#' plot(nrsim, normalize=FALSE)
#' plot(varsim)
#' plot(rsim)
#' hist(rsim)
#' hist(varsim)
#'
#' } #end dontrun
#'
#' @export
mcsim <- function(  Portfolio
                    , Account
                    , n = 1000
                    , replacement = TRUE
                    , ...
                    , use=c('equity','txns','returns')
                    , l = 1
                    , varblock = FALSE
                    , gap = 1

){

  use=use[1] #take the first value if the user didn't specify
  switch (use,
          Eq =, eq =, Equity =, equity =, cumPL = {
            dailyPL <- dailyEqPL(Portfolio, incl.total = TRUE)
            dailyPL <- dailyPL[gap:nrow(dailyPL), ncol(dailyPL)]
          },
          Txns =, txns =, Trades =, trades = {
            dailyPL <- dailyTxnPL(Portfolio,  incl.total = TRUE)
            dailyPL <- dailyPL[gap:nrow(dailyPL), ncol(dailyPL)]
          },
          Rets =, rets =, Returns=, returns =, percent = {
            dailyPL <- PortfReturns(Account = Account, Portfolios = Portfolio)
            use <- 'returns' # standardize for later checks
          }
  )

  # p <- getPortfolio(Portfolio)
  a <- getAccount(Account)
  initEq <- attributes(a)$initEq
  use=c('equity','txns')
  tmp <- NULL
  k <- NULL
  EndEqdf <- data.frame(dailyPL)

  if(isTRUE(replacement)) {
    if(isTRUE(varblock)) {
      sim <- 'geom'
      # tsboot will use a geometric random distribution of block length centered on l
    } else {
      sim <- 'fixed'
      # tsboot will use a fixed block length l
    }
    tsb <- tsboot(dailyPL, function(x) { -max(cummax(cumsum(x))-cumsum(x)) }, n, l, sim = sim, ...)
    inds <- t(boot.array(tsb))
    #k <- NULL
    tsbootARR <- NULL
    tsbootxts <- NULL
    EndEqdf[is.na(EndEqdf)] <- 0
    for(k in 1:ncol(inds)){
      tmp <- cbind(tmp, EndEqdf[inds[,k],])
    }
    tsbootARR <- apply(tmp, 2, function(x) cumsum(x))
    which(is.na(tsbootARR))
    tsbootxts <- xts(tsbootARR, index(dailyPL))
    dd <- tsb$t
    withorwithout <- "WITH REPLACEMENT"
  } else if(!isTRUE(replacement)) {
    tsbootxts <- foreach (k=1:n, .combine=cbind.xts ) %dopar% {
      if(isTRUE(l>1)) {
        # do a block resample, without replacement
        # first, resample the index
        x <- 1:length(dailyPL)
        # now sample blocks
        if (isTRUE(varblock)){
          # this method creates variable length blocks with a uniform random
          # distribution centered on 'l'
          s <- sort(sample(x=x[2:length(x)-1],size = l-1,replace = FALSE))
        } else {
          # fixed block length
          # this method chooses a random start from 1:l(ength) and then
          # samples fixed blocks of length l to the end of the series
          s <- seq(sample.int(l,1),length(x),by=l)
        }
        blocks<-split(x, findInterval(x,s))
        # now reassemble the target index order
        idx <- unlist(blocks[sample(names(blocks),size = length(blocks),replace = FALSE)]) ; names(idx)<-NULL
        tmp <- as.vector(dailyPL)[idx]
      } else {
        # block length is 1, just sample with or without replacement
        tmp <- sample(as.vector(dailyPL), replace = replacement)
      }
      tsbootARR <- cumsum(tmp)
      tsbootxts <- xts(tsbootARR, index(dailyPL))
    }
    dd <- apply(tsbootxts, 2, function(x) { -max(cummax(x)-(x)) } )
    withorwithout <- "WITHOUT REPLACEMENT"
  }

  ret <- list(replicates=tsbootxts
              , dailypl=dailyPL
              , initeq=initEq
              , num=n, length=l
              , ddvec=dd
              , w=withorwithout
              , use = use
              , call=match.call()
             ) #end return list

  class(ret) <- "mcsim"
  ret
}



#' plot method for objects of type \code{mcsim}
#'
#' @param x object of type 'mcsim' to plot
#' @param y not used, to match generic signature, may hold overlay daya in the future
#' @param \dots any other passthrough parameters
#' @param normalize TRUE/FALSE whether to normalize the plot by initEq, default TRUE
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @export
plot.mcsim <- function(x, y, ..., normalize=TRUE) {
  ret <- x
  if(isTRUE(normalize) && ret$initeq>1 && ret$use != 'returns'){
    div <- ret$initeq
  } else {
    div <- 1
  }
  p   <- plot.xts(ret$replicates/div
                  , col = "lightgray"
                  , main = paste0("Sample Backtest ",ret$w)
                  , grid.ticks.on = 'years'
  )
  p   <- lines(cumsum(ret$dailypl/div), col = "red")
  p
}

#' hist method for objects of type \code{mcsim}
#'
#' @param x object of type mcsim to plot
#' @param \dots any other passthrough parameters
#' @param normalize TRUE/FALSE whether to normalize the plot by initEq, default TRUE
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @importFrom graphics axis box hist lines par text
#'
#' @export
hist.mcsim <- function(x, ..., normalize=TRUE) {
  ret <- x
  if(isTRUE(normalize) && ret$initeq>1 && ret$use != 'returns'){
    ret$dailypl <- rbind(xts(0,order.by=index(ret$dailypl[1])-1), ret$dailypl)
    idxarr <- -(cummax(cumsum(ret$dailypl))-cumsum(ret$dailypl))
    maxDD <- min(idxarr) # maxDD from idxarr
    # find index of maxDD
    idxmaxdd <- which(maxDD == idxarr)[1] # subset 1st element if vector length > 1
    divhist <- cummax(cumsum((ret$dailypl)))[idxmaxdd]+ret$initeq # add max cum PL with initeq to get div to use for normalizing maxDD

    # now calculate div for sampled maxDDs
    idxarrtsb <- apply(ret$replicates, 2, function(x) { -(cummax(x)-(x)) } )
    maxDDtsb <- apply(idxarrtsb, 2, function(x) {min(x)} )
    idxmaxddtsb <- NULL
    for (i in 1:ret$num) {
      idxmaxddtsb[i] <- which(maxDDtsb[i] == idxarrtsb[,i])[1]
    }
    div <- NULL
    for (i in 1:ret$num) {
      div[i] <- cummax(ret$replicates[,i])[idxmaxddtsb[i]] + ret$initeq
    }

    # and finally calculate div for sample median
    divmed <- NULL
    divmed <- ifelse(ret$num%%2!=0, div[which(ret$ddvec==median(ret$ddvec))], div[which(ret$ddvec[-1]==median(ret$ddvec[-1]))])

  } else {  # do not normalize
    divhist <- 1
    div <- 1
    divmed <- 1
  }
  xname <- paste(ret$num, "replicates with block length", ret$length)
  h <- hist(ret$ddvec/div, main = paste("Drawdown distribution", ret$w, "of" , xname), breaks="FD"
            , xlab = "Max Drawdown", ylab = "Density"
            , col = "lightgray", border = "white", freq=FALSE
  )
  h
  box(col = "darkgray")

  b = -max(cummax(cumsum(ret$dailypl))-cumsum(ret$dailypl))/divhist
  abline(v = b, col = "darkgray", lty = 2)
  b.label = ("Backtest Max Drawdown")
  h = rep(0.2 * par("usr")[3] + 1 * par("usr")[4], length(b))
  text(b, h, b.label, offset = 0.2, pos = 2, cex = 0.8, srt = 90)

  abline(v=median(na.omit(ret$ddvec))/divmed, col = "darkgray", lty = 2)
  c.label = ("Sample Median Max Drawdown")
  text(median(na.omit(ret$ddvec))/divmed, h, c.label, offset = 0.2, pos = 2, cex = 0.8, srt = 90)
  
  axis(side=1, at=seq(0,min(ret$ddvec/div),-0.1), labels=seq(0,min(ret$ddvec/div),-0.1))
}

#' quantile method for objects of type \code{mcsim}
#'
#' @param x object of type 'mcsim' to produce replicate quantiles
#' @param \dots any other passthrough parameters
#' @param normalize TRUE/FALSE whether to normalize the plot by initEq, default TRUE
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @export
quantile.mcsim <- function(x, ..., normalize=TRUE) {
  ret <- x
  q   <- quantile(ret$replicates)
  q
}


###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/)
# Copyright (c) 2008-2016 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
