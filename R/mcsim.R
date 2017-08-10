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
#' sample round turn trades, as provided in (TODO: add link once function exists).
#'
#' A few of the arguments and methods probably deserve more discussion as well.
#'
#' \code{use} describes the method to use to generate the initial daily P\&L to
#' be sampled from.  The options are:
#' \itemize{
#'   \item{equity}{will use daily portfolio cash P&L}
#'   \item{txn}{will use transaction \code{Net.Trading.PL}}
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
#' @param use determines whether to use 'equity', 'txn', 'returns', or 'cash' P\&L, default = "equity" ie. daily
#' @param l block length, default = 1
#' @param varblock boolean to determine whether to use variable block length, default FALSE
#' @param gap numeric number of periods from start of series to start on, to eliminate leading NA's
#' @param CI numeric specifying desired Confidence Interval used in hist.mcsim(), default 0.95
#' @param cashPL optional regular xts object of cash P&L if \code{use='cash'} and you don't want to use a blotter Portfolio to get P&L.
#' @return a list object of class 'mcsim' containing:
#' \itemize{
#'   \item{\code{replicates}:}{an xts object containing all the resampled time series replicates}
#'   \item{\code{percreplicates}:}{an xts object containing all the resampled time series replicates in percent}
#'   \item{\code{dailypl}:}{an xts object containing daily P&L from the original backtest}
#'   \item{\code{percdailypl}:}{an xts object containing daily P&L in percent from the original backtest}
#'   \item{\code{initeq}:}{a numeric variable containing the initEq of the portfolio, for starting portfolio value}
#'   \item{\code{num}:}{a numeric variable reporting the number of replicaes in the simulation}
#'   \item{\code{length}:}{a numeric variable reporting the block length used in the simulation, if any}
#'   \item{\code{samplestats}:}{a numeric dataframe of various statistics for each replicate series}
#'   \item{\code{percsamplestats}:}{a numeric dataframe of various statistics for each replicate series in percent}
#'   \item{\code{original}:}{a numeric dataframe of the statistics for the original series}
#'   \item{\code{percoriginal}:}{a numeric dataframe of the statistics for the original series in percent terms}
#'   \item{\code{stderror}:}{a numeric dataframe of the standard error of the statistics for the replicates}
#'   \item{\code{percstderror}:}{a numeric dataframe of the standard error of the statistics for the replicates in percent}
#'   \item{\code{CI}:}{numeric specifying desired Confidence Interval used in hist.mcsim(), default 0.95}
#'   \item{\code{CIdf}:}{a numeric dataframe of the Confidence Intervals of the statistics for the bootstrapped replicates}
#'   \item{\code{CIdf_perc}:}{a numeric dataframe of the Confidence Intervals of the statistics for the bootstrapped replicates in percent}
#'   \item{\code{w}:}{a string containing information on whether the simulation is with or without replacement}
#'   \item{\code{use}:}{ a string with the value of the 'use' parameter, for checking later}
#'   \item{\code{seed}:}{ the value of \code{.Random.seed} for replication, if required}
#'   \item{\code{call}:}{an object of type \code{call} that contains the \code{mcsim} call}
#' }
#'
#' If \code{use='cash'}, you must also supply a daily (or other regular frequency) 
#' cash P&L xts object in the \code{dailyPL} argument.
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
#' nrsim <- mcsim("longtrend", "longtrend", n=1000, replacement=FALSE, l=1, gap=10, CI=0.95)
#' nrblocksim.75 <- mcsim("longtrend", "longtrend", n=1000, replacement=FALSE, l=10, gap=10, CI=0.75)
#' rsim <- mcsim("longtrend", "longtrend", n=1000, replacement=TRUE, l=1, gap=10)
#' varsim <- mcsim("longtrend", "longtrend", n=1000, replacement=TRUE, l=10, varblock=TRUE, gap=10)
#' nrvarsim <- mcsim("longtrend", "longtrend", n=1000, replacement=FALSE, l=10, varblock=TRUE, gap=10)
#'
#' quantile(varsim)
#' quantile(nrsim)
#' quantile(nrvarsim)
#' 
#' summary(varsim, normalize=FALSE)
#' summary(nrsim)
#' summary(nrvarsim)
#'
#' plot(nrsim, normalize=TRUE)
#' plot(nrsim, normalize=FALSE)
#' plot(varsim)
#' plot(rsim)
#' hist(nrblocksim.75)
#' hist(rsim)
#' hist(varsim)
#'
#' } #end dontrun
#'
#' @export
mcsim <- function(  Portfolio = NULL
                    , Account = NULL
                    , n = 1000
                    , replacement = TRUE
                    , ...
                    , use=c('equity','txns','returns','cash')
                    , l = 1
                    , varblock = FALSE
                    , gap = 1
                    , CI = 0.95
                    , cashPL = NULL
                    
){
  seed = .GlobalEnv$.Random.seed # store the random seed for replication, if needed
  use=use[1] #take the first value if the user didn't specify
  switch (use,
          Eq =, eq =, Equity =, equity =, cumPL = {
            dailyPL <- dailyEqPL(Portfolio, incl.total = TRUE)
          },
          Txns =, txns =, Trades =, trades = {
            dailyPL <- dailyTxnPL(Portfolio,  incl.total = TRUE)
          },
          cash =, cashPL =, {
            if(!hasArg('cashPL') || is.null(cashPL)) {
              stop('you must supply regular cash P&L xts in the cashPL arg to use cash P&L directly')
            } 
            dailyPL <- checkData(cashPL)
          }
  )
  # trim out training period defined by 'gap' argument
  dailyPL <- dailyPL[gap:nrow(dailyPL), ncol(dailyPL)]
  
  ##################### confidence interval formulae ###########################
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
  ##############################################################################
  
  if (!is.null(Account)){
    a <- getAccount(Account)
    initEq <- attributes(a)$initEq
  } else {
    initEq <- 1 #avoid div by 0 error
  }
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
    fnames <- function(x, indices) {
      Mean <- mean(x)
      Median <- median(x)
      sd <- StdDev(xts(x, index(dailyPL))) # need to use xts for StdDev to work
      maxdd <- -max(cummax(cumsum(x))-cumsum(x))
      #     sharpedata <- xts(ROC(cumsum(x + initEq)),index(dailyPL))
      #     sharpedata[is.na(sharpedata)] <- 0
      #     sharpe <- SharpeRatio(sharpedata, FUN = "StdDev")
      sharpe <- Mean/sd # this is a rough version of sharpe using 'cash' mean & stddev as opposed to 'returns'
      fnames <- c(Mean, Median, sd, maxdd, sharpe)
      #fnames <- c(Mean)
      return(fnames)
    }
    tsb <- tsboot(coredata(dailyPL), statistic = fnames, n, l, sim = sim, ...)
    # For a boot tutprial see http://people.tamu.edu/~alawing/materials/ESSM689/Btutorial.pdf
    colnames(tsb$t) <- c("mean","median","stddev","maxDD","sharpe")
    #tsb <- tsboot(coredata(dailyPL), function(x) { -max(cummax(cumsum(x))-cumsum(x)) }, n, l, sim = sim, ...)
    inds <- t(boot.array(tsb))
    #k <- NULL
    tsbootARR <- NULL
    tsbootxts <- NULL
    EndEqdf[is.na(EndEqdf)] <- 0
    for(k in 1:ncol(inds)){
      tmp <- cbind(tmp, EndEqdf[inds[,k],])
    }
    tsbootxts <- xts(tmp, index(dailyPL))
    sampleoutput <- as.data.frame(tsb$t)
    
    roctsbootxts <- ROC(cumsum(tsbootxts)+initEq, type = "discrete")
    roctsbootxts[is.na(roctsbootxts)] <- 0
    samplepercoutput <- data.frame(matrix(nrow = n, ncol = 5))
    colnames(samplepercoutput) <- c("mean","median","stddev","maxDD","sharpe")
    samplepercoutput$mean <- apply(roctsbootxts, 2, function(x) { mean(x) } )
    samplepercoutput$median <- apply(roctsbootxts, 2, function(x) { median(x) } )
    samplepercoutput$stddev <- apply(roctsbootxts, 2, function(x) { StdDev(x) } )
    samplepercoutput$maxDD <- apply(roctsbootxts, 2, function(x) { maxDrawdown(x, invert = FALSE) } )
    samplepercoutput$sharpe <- apply(roctsbootxts, 2, function(x) { mean(x)/StdDev(x) } )
    
    withorwithout <- "with replacement"
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
          s <- sort(sample(x=x[2:length(x)-1],size = floor(length(x)/l),replace = FALSE))
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
      tsbootxts <- xts(tmp, index(dailyPL))
    }
    sampleoutput <- data.frame(matrix(nrow = n, ncol = 5))
    colnames(sampleoutput) <- c("mean","median","stddev","maxDD","sharpe")
    sampleoutput$mean <- apply(tsbootxts, 2, function(x) { mean(x) } )
    sampleoutput$median <- apply(tsbootxts, 2, function(x) { median(x) } )
    sampleoutput$stddev <- apply(tsbootxts, 2, function(x) { StdDev(x) } )
    sampleoutput$maxDD <- apply(tsbootxts, 2, function(x) { -max(cummax(cumsum(x))-cumsum(x)) } )
    sampleoutput$sharpe <- apply(tsbootxts, 2, function(x) { mean(x)/StdDev(x) } )
    
    roctsbootxts <- ROC(cumsum(tsbootxts)+initEq, type = "discrete")
    roctsbootxts[is.na(roctsbootxts)] <- 0
    samplepercoutput <- data.frame(matrix(nrow = n, ncol = 5))
    colnames(samplepercoutput) <- c("mean","median","stddev","maxDD","sharpe")
    samplepercoutput$mean <- apply(roctsbootxts, 2, function(x) { mean(x) } )
    samplepercoutput$median <- apply(roctsbootxts, 2, function(x) { median(x) } )
    samplepercoutput$stddev <- apply(roctsbootxts, 2, function(x) { StdDev(x) } )
    samplepercoutput$maxDD <- apply(roctsbootxts, 2, function(x) { maxDrawdown(x, invert = FALSE) } )
    samplepercoutput$sharpe <- apply(roctsbootxts, 2, function(x) { mean(x)/StdDev(x) } )
    
    withorwithout <- "without replacement"
  }
  
  percdailyPL <- ROC(cumsum(dailyPL)+initEq, type = "discrete")
  percdailyPL[is.na(percdailyPL)] <- 0
  
  # browser()
  # store stats for use later in hist.mcsim and summary.mcsim
  if(isTRUE(withorwithout == "WITH REPLACEMENT")) {
    # use output from tsboot for original backtest stats, tsb$t0
    original <- data.frame(t(tsb$t0))
    colnames(original) <- c("mean","median","stddev","maxDD","sharpe")
    # need to compute stats for backtest based on percent returns since tsboot called on cash returns
    percoriginal <- data.frame(matrix(nrow = 1, ncol = 5))
    colnames(percoriginal) <- c("mean","median","stddev","maxDD","sharpe")
    percoriginal$mean <- mean(percdailyPL)
    percoriginal$median <- median(percdailyPL)
    percoriginal$stddev <- StdDev(percdailyPL)
    percoriginal$maxDD <- maxDrawdown(percdailyPL, invert = FALSE)
    percoriginal$sharpe <- mean(percdailyPL)/StdDev(percdailyPL)
    
    # browser()
    # Compute standard errors of the sample stats
    stderror <- data.frame(matrix(nrow = 1, ncol = 5))
    colnames(stderror) <- c("mean","median","stddev","maxDD","sharpe")
    row.names(stderror) <- "Std. Error"
    stderror$mean <- StdDev(sampleoutput[,1])
    stderror$median <- StdDev(sampleoutput[,2])
    stderror$stddev <- StdDev(sampleoutput[,3])
    stderror$maxDD <- StdDev(sampleoutput[,4])
    stderror$sharpe <- StdDev(sampleoutput[,5])
    
    percstderror <- data.frame(matrix(nrow = 1, ncol = 5))
    colnames(percstderror) <- c("mean","median","stddev","maxDD","sharpe")
    row.names(percstderror) <- "Std. Error"
    percstderror$mean <- StdDev(samplepercoutput[,1])
    percstderror$median <- StdDev(samplepercoutput[,2])
    percstderror$stddev <- StdDev(samplepercoutput[,3])
    percstderror$maxDD <- StdDev(samplepercoutput[,4])
    percstderror$sharpe <- StdDev(samplepercoutput[,5])
    
    #browser()
    CI_mean <- cbind(CI_lower(mean(tsb$t[,1]), StdDev(tsb$t[,1])*qnorm((1+CI)/2)),
                     CI_upper(mean(tsb$t[,1]), StdDev(tsb$t[,1])*qnorm((1+CI)/2)))
    
    CI_median <- cbind(CI_lower(mean(tsb$t[,2]), StdDev(tsb$t[,2])*qnorm((1+CI)/2)),
                       CI_upper(mean(tsb$t[,2]), StdDev(tsb$t[,2])*qnorm((1+CI)/2)))
    
    CI_stddev <- cbind(CI_lower(mean(tsb$t[,3]), StdDev(tsb$t[,3])*qnorm((1+CI)/2)),
                       CI_upper(mean(tsb$t[,3]), StdDev(tsb$t[,3])*qnorm((1+CI)/2)))
    
    CI_maxDD <- cbind(CI_lower(mean(tsb$t[,4]), StdDev(tsb$t[,4])*qnorm((1+CI)/2)),
                      CI_upper(mean(tsb$t[,4]), StdDev(tsb$t[,4])*qnorm((1+CI)/2)))
    
    CI_sharpe <- cbind(CI_lower(mean(tsb$t[,5]), StdDev(tsb$t[,5])*qnorm((1+CI)/2)),
                       CI_upper(mean(tsb$t[,5]), StdDev(tsb$t[,5])*qnorm((1+CI)/2)))
    
    CI_percmean <- cbind(CI_lower(mean(samplepercoutput[,1]), StdDev(samplepercoutput[,1])*qnorm((1+CI)/2)),
                         CI_upper(mean(samplepercoutput[,1]), StdDev(samplepercoutput[,1])*qnorm((1+CI)/2)))
    
    CI_percmedian <- cbind(CI_lower(mean(samplepercoutput[,2]), StdDev(samplepercoutput[,2])*qnorm((1+CI)/2)),
                           CI_upper(mean(samplepercoutput[,2]), StdDev(samplepercoutput[,2])*qnorm((1+CI)/2)))
    
    CI_percstddev <- cbind(CI_lower(mean(samplepercoutput[,3]), StdDev(samplepercoutput[,3])*qnorm((1+CI)/2)),
                           CI_upper(mean(samplepercoutput[,3]), StdDev(samplepercoutput[,3])*qnorm((1+CI)/2)))
    
    CI_percmaxDD <- cbind(CI_lower(mean(samplepercoutput[,4]), StdDev(samplepercoutput[,4])*qnorm((1+CI)/2)),
                          CI_upper(mean(samplepercoutput[,4]), StdDev(samplepercoutput[,4])*qnorm((1+CI)/2)))
    
    CI_percsharpe <- cbind(CI_lower(mean(samplepercoutput[,5]), StdDev(samplepercoutput[,5])*qnorm((1+CI)/2)),
                           CI_upper(mean(samplepercoutput[,5]), StdDev(samplepercoutput[,5])*qnorm((1+CI)/2)))
    
    # Build the Confidence Interval dataframes, 1 for cash and 1 for percent returns
    CIdf <- data.frame(matrix(nrow = 2, ncol = 5))
    colnames(CIdf) <- c("mean","median","stddev","maxDD","sharpe")
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
    
    CIdf_perc <- data.frame(matrix(nrow = 2, ncol = 5))
    colnames(CIdf_perc) <- c("mean","median","stddev","maxDD","sharpe")
    row.names(CIdf_perc) <- c("Lower CI","Upper CI")
    CIdf_perc$mean[row.names(CIdf_perc) == "Lower CI"] <- CI_percmean[1,1]
    CIdf_perc$mean[row.names(CIdf_perc) == "Upper CI"] <- CI_percmean[1,2]
    
    CIdf_perc$median[row.names(CIdf_perc) == "Lower CI"] <- CI_percmedian[1,1]
    CIdf_perc$median[row.names(CIdf_perc) == "Upper CI"] <- CI_percmedian[1,2]
    
    CIdf_perc$stddev[row.names(CIdf_perc) == "Lower CI"] <- CI_percstddev[1,1]
    CIdf_perc$stddev[row.names(CIdf_perc) == "Upper CI"] <- CI_percstddev[1,2]
    
    CIdf_perc$maxDD[row.names(CIdf_perc) == "Lower CI"] <- CI_percmaxDD[1,1]
    CIdf_perc$maxDD[row.names(CIdf_perc) == "Upper CI"] <- CI_percmaxDD[1,2]
    
    CIdf_perc$sharpe[row.names(CIdf_perc) == "Lower CI"] <- CI_percsharpe[1,1]
    CIdf_perc$sharpe[row.names(CIdf_perc) == "Upper CI"] <- CI_percsharpe[1,2]
    
  } else {
    # compute stats for WITHOUT REPLACEMENT
    original <- data.frame(matrix(nrow = 1, ncol = 5))
    colnames(original) <- c("mean","median","stddev","maxDD","sharpe")
    original$mean <- mean(dailyPL)
    original$median <- median(dailyPL)
    original$stddev <- StdDev(dailyPL)
    original$maxDD <- -max(cummax(cumsum(dailyPL))-cumsum(dailyPL))
    original$sharpe <- mean(dailyPL)/StdDev(dailyPL)
    # need to compute stats for backtest based on percent returns
    percoriginal <- data.frame(matrix(nrow = 1, ncol = 5))
    colnames(percoriginal) <- c("mean","median","stddev","maxDD","sharpe")
    percoriginal$mean <- mean(percdailyPL)
    percoriginal$median <- median(percdailyPL)
    percoriginal$stddev <- StdDev(percdailyPL)
    percoriginal$maxDD <- maxDrawdown(percdailyPL, invert = FALSE)
    percoriginal$sharpe <- mean(percdailyPL)/StdDev(percdailyPL)
    
    # Compute standard errors of the sample stats
    stderror <- data.frame(matrix(nrow = 1, ncol = 5))
    colnames(stderror) <- c("mean","median","stddev","maxDD","sharpe")
    row.names(stderror) <- "Std. Error"
    stderror$mean <- StdDev(sampleoutput[,1])
    stderror$median <- StdDev(sampleoutput[,2])
    stderror$stddev <- StdDev(sampleoutput[,3])
    stderror$maxDD <- StdDev(sampleoutput[,4])
    stderror$sharpe <- StdDev(sampleoutput[,5])
    
    percstderror <- data.frame(matrix(nrow = 1, ncol = 5))
    colnames(percstderror) <- c("mean","median","stddev","maxDD","sharpe")
    row.names(percstderror) <- "Std. Error"
    percstderror$mean <- StdDev(samplepercoutput[,1])
    percstderror$median <- StdDev(samplepercoutput[,2])
    percstderror$stddev <- StdDev(samplepercoutput[,3])
    percstderror$maxDD <- StdDev(samplepercoutput[,4])
    percstderror$sharpe <- StdDev(samplepercoutput[,5])
    
    #browser()
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
    
    CI_percmean <- cbind(CI_lower(mean(samplepercoutput[,1]), StdDev(samplepercoutput[,1])*qnorm((1+CI)/2)),
                         CI_upper(mean(samplepercoutput[,1]), StdDev(samplepercoutput[,1])*qnorm((1+CI)/2)))
    
    CI_percmedian <- cbind(CI_lower(mean(samplepercoutput[,2]), StdDev(samplepercoutput[,2])*qnorm((1+CI)/2)),
                           CI_upper(mean(samplepercoutput[,2]), StdDev(samplepercoutput[,2])*qnorm((1+CI)/2)))
    
    CI_percstddev <- cbind(CI_lower(mean(samplepercoutput[,3]), StdDev(samplepercoutput[,3])*qnorm((1+CI)/2)),
                           CI_upper(mean(samplepercoutput[,3]), StdDev(samplepercoutput[,3])*qnorm((1+CI)/2)))
    
    CI_percmaxDD <- cbind(CI_lower(mean(samplepercoutput[,4]), StdDev(samplepercoutput[,4])*qnorm((1+CI)/2)),
                          CI_upper(mean(samplepercoutput[,4]), StdDev(samplepercoutput[,4])*qnorm((1+CI)/2)))
    
    CI_percsharpe <- cbind(CI_lower(mean(samplepercoutput[,5]), StdDev(samplepercoutput[,5])*qnorm((1+CI)/2)),
                           CI_upper(mean(samplepercoutput[,5]), StdDev(samplepercoutput[,5])*qnorm((1+CI)/2)))
    
    # Build the Confidence Interval dataframes, 1 for cash and 1 for percent returns
    CIdf <- data.frame(matrix(nrow = 2, ncol = 5))
    colnames(CIdf) <- c("mean","median","stddev","maxDD","sharpe")
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
    
    CIdf_perc <- data.frame(matrix(nrow = 2, ncol = 5))
    colnames(CIdf_perc) <- c("mean","median","stddev","maxDD","sharpe")
    row.names(CIdf_perc) <- c("Lower CI","Upper CI")
    CIdf_perc$mean[row.names(CIdf_perc) == "Lower CI"] <- CI_percmean[1,1]
    CIdf_perc$mean[row.names(CIdf_perc) == "Upper CI"] <- CI_percmean[1,2]
    
    CIdf_perc$median[row.names(CIdf_perc) == "Lower CI"] <- CI_percmedian[1,1]
    CIdf_perc$median[row.names(CIdf_perc) == "Upper CI"] <- CI_percmedian[1,2]
    
    CIdf_perc$stddev[row.names(CIdf_perc) == "Lower CI"] <- CI_percstddev[1,1]
    CIdf_perc$stddev[row.names(CIdf_perc) == "Upper CI"] <- CI_percstddev[1,2]
    
    CIdf_perc$maxDD[row.names(CIdf_perc) == "Lower CI"] <- CI_percmaxDD[1,1]
    CIdf_perc$maxDD[row.names(CIdf_perc) == "Upper CI"] <- CI_percmaxDD[1,2]
    
    CIdf_perc$sharpe[row.names(CIdf_perc) == "Lower CI"] <- CI_percsharpe[1,1]
    CIdf_perc$sharpe[row.names(CIdf_perc) == "Upper CI"] <- CI_percsharpe[1,2]
    
  }
  
  ret <- list(replicates=tsbootxts
              , percreplicates=roctsbootxts
              , dailypl=dailyPL
              , percdailypl=percdailyPL
              , initeq=initEq
              , num=n, length=l
              , samplestats=sampleoutput
              , percsamplestats=samplepercoutput
              , original=original
              , percoriginal=percoriginal
              , stderror=stderror
              , percstderror=percstderror
              , CI=CI
              , CIdf=CIdf
              , CIdf_perc=CIdf_perc
              , w=withorwithout
              , use=use
              , seed=seed
              , call=match.call()
  ) #end return list
  
  class(ret) <- "mcsim"
  ret
}

#' plot method for objects of type \code{mcsim}
#'
#' @param x object of type 'mcsim' to plot
#' @param y not used, to match generic signature, may hold overlay data in the future
#' @param \dots any other passthrough parameters
#' @param normalize TRUE/FALSE whether to normalize the plot by initEq, default TRUE
#' @author Jasen Mackie, Brian G. Peterson
#' @seealso \code{\link{mcsim}}
#' @export
plot.mcsim <- function(x, y, ..., normalize=TRUE) {
  ret <- x
  if(isTRUE(normalize) && ret$initeq>1){
    x1 <- cumsum(ret$percreplicates)
    x2 <- cumsum(ret$percdailypl)
  # browser()
  } else {
    x1 <- cumsum(ret$replicates)
    x2 <- cumsum(ret$dailypl)
  # browser()
  }
  p <- plot.xts(x1
                , col = "lightgray"
                , main = paste(ret$num, "replicates", ret$w, "and block length", ret$length)
                , grid.ticks.on = 'years'
  )
  p   <- lines(x2, col = "red")
  p
}

#' hist method for objects of type \code{mcsim}
#'
#' @param x object of type mcsim to plot
#' @param \dots any other passthrough parameters
#' @param normalize TRUE/FALSE whether to normalize the hist by div, default TRUE
#' @param methods are statistics to include in hist output, default methods=c("mean","median","stddev","maxDD","sharpe")
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @importFrom graphics axis box hist lines par text
#'
#' @export
hist.mcsim <- function(x, ..., normalize=TRUE, 
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
    xname <- paste(ret$num, "replicates", ret$w, "using block length", ret$length, "and", ret$CI, "confidence interval")
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
    xname <- paste(ret$num, "replicates", ret$w, "using block length", ret$length, "and", ret$CI, "confidence interval")
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


#' quantile method for objects of type \code{mcsim}
#'
#' generates quantiles of cumulative P&L of the replicated equity curves
#' 
#' @param x object of type 'mcsim' to produce replicate quantiles
#' @param \dots any other passthrough parameters
#' @param normalize TRUE/FALSE whether to normalize the plot by initEq, default TRUE
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @export
quantile.mcsim <- function(x, ..., normalize=TRUE) {
  ret <- x
  
  if(isTRUE(normalize) && ret$initeq>1){
    x1 <- cumsum(ret$percreplicates)
  } else {
    x1 <- cumsum(ret$replicates)
  }
  
  if(isTRUE(normalize)) {
    q   <- quantile(na.omit(x1))
  } else {
    q   <- quantile(na.omit(x1))
  }
  q
}

#' summary and print methods for objects of type \code{mcsim}
#'
#' @param object object of type 'mcsim' to produce a sample and backtest summary
#' @param x an object of type 'mcsim'
#' @param \dots any other passthrough parameters
#' @param normalize TRUE/FALSE whether to use normalized percent-based summary stats, default TRUE
#' @author Jasen Mackie, Brian G. Peterson
#'
#' @method summary mcsim
#' @export
summary.mcsim <- function(object, ..., normalize=TRUE) {
  ret <- object
  if(isTRUE(normalize)){
    sampletablemedian <- apply(ret$percsamplestats, 2, function(x) { median(x) } )
    sampletablemean <- apply(ret$percsamplestats, 2, function(x) { mean(x) } )
    backtesttable <- NULL
    for (name in names(sampletablemedian)) {
      switch (name,
              mean = {
                backtesttable <- cbind(backtesttable, ret$percoriginal$mean)
              },
              median = {
                backtesttable <- cbind(backtesttable, ret$percoriginal$median)
              },
              stddev = {
                backtesttable <- cbind(backtesttable, ret$percoriginal$stddev)
              },
              maxDD = {
                backtesttable <- cbind(backtesttable, ret$percoriginal$maxDD)
              },
              sharpe = {
                backtesttable <- cbind(backtesttable, ret$percoriginal$stddev)
              }
      )
    }
    summarytable <- rbind(sampletablemean, sampletablemedian, backtesttable)
    rownames(summarytable) <- c("Sample Mean", "Sample Median", "Backtest")
    t(rbind(summarytable, ret$CIdf_perc, ret$percstderror))
    
  } else {
    sampletablemedian <- apply(ret$samplestats, 2, function(x) { median(x) } )
    sampletablemean <- apply(ret$samplestats, 2, function(x) { mean(x) } )
    backtesttable <- NULL
    for (name in names(sampletablemedian)) {
      switch (name,
              mean = {
                backtesttable <- cbind(backtesttable, ret$original$mean)
              },
              median = {
                backtesttable <- cbind(backtesttable, ret$original$median)
              },
              stddev = {
                backtesttable <- cbind(backtesttable, ret$original$stddev)
              },
              maxDD = {
                backtesttable <- cbind(backtesttable, ret$original$maxDD)
              },
              sharpe = {
                backtesttable <- cbind(backtesttable, ret$original$sharpe)
              }
      )
    }
    summarytable <- rbind(sampletablemean, sampletablemedian, backtesttable)
    rownames(summarytable) <- c("Sample Mean", "Sample Median", "Backtest")
    t(rbind(summarytable, ret$CIdf, ret$stderror))
  }
}

#' @rdname summary.mcsim
#' @method print mcsim
#' @export
print.mcsim <- function(x,..., normalize=TRUE){
  round(summary.mcsim(x,..., normalize=normalize),3)
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
