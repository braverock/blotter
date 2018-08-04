###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2017
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

#' @title Probability of backtest overfitting
#' @description Performs the probability of backtest overfitting computations.
#' @details This function performs the probability of backtest overfitting calculation
#' using a combinatorially-symmetric cross validation (CSCV) approach proposed by Bailey et al in the
#' paper \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2326253}. We have ported most of the source code
#' from Matt Barry's implementation with his gracious consent. There will likely be a few extensions from Matt's
#' version, with the first being to make the function compatible with the \code{portfolios}
#' object in \code{blotter}.
#' @param retmatrix a \eqn{TxN} data frame of returns, where \eqn{T} is the samples per study and \eqn{N} is the number of studies
#' @param portfolios string name of portfolio, or optionally a vector of portfolios, if not using the \code{retmatrix} param
#' @param sm the number of subsets (ie. subset matrices) of \code{m} for CSCV combinations; 
#' must evenly divide \code{m} 
#' @param f the function to evaluate a study's performance; required
#' @param threshold the performance metric threshold 
#' (e.g. 0 for Sharpe, 1 for Omega)
#' @param inf_sub infinity substitution value for reasonable plotting
#' @param allow_parallel whether to enable parallel processing, default FALSE
#' @param ... any other passthrough parameters
#' @param strategy optional strategy specification that would contain more information on the process, default NULL
#' @param trials optional number of trials, default NULL
#' @param audit optional audit environment containing the results of parameter optimization or walk forward, default NULL
#' @param env optional environment to find market data in, if required.
#' @keywords probability backtest overfitting PBO CSCV
#' @return object of class \code{pbo} containing list of PBO calculation results 
#' and settings
#' @author Matt Barry, Jasen Mackie, Brian G. Peterson
#' @export
#' @importFrom utils combn
#' @references 
#' Baily et al., "The Probability of Backtest Overfitting,"
#' \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2326253}
#'
#' Baily et al., "Pseudo-Mathematics and Financial Charlatanism: The Effects of Backtest Overfitting on Out-of-Sample Performance,"
#' \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2308659}
#'
#' Matt Barry's Vignette for the original \code{pbo} package
#' \url{https://cran.r-project.org/web/packages/pbo/vignettes/pbo.html}
#' @examples
#' \dontrun{
#' sharpe <- function(x,rf=0.03/252) {
#'sr <- apply(x,2,function(col) {
#'  er = col - rf
#'  return(mean(er)/sd(er))
#'})
#'return(sr)
#'}
#' demo("macdParameters", ask=FALSE)
#' pbo  <- pbo(portfolios = 'macd'
#'             ,s=8
#'             ,f=sharpe
#'             ,threshold=0
#'             ,inf_sub=6
#'             ,allow_parallel=FALSE
#'             ,strategy='macd'
#'             ,audit=.audit)
#' summary(pbo)
#' }

pbo <- function( retmatrix=NULL
                 , portfolios=NULL
                 , sm=8
                 , f=NA
                 , threshold=0
                 , inf_sub=6
                 , allow_parallel=FALSE
                 , ...
                 , strategy=NULL
                 , trials=NULL
                 , audit=NULL
                 , env=.GlobalEnv)
{
  #check inputs
  if(is.null(retmatrix)&&length(portfolios==1)&&is.null(audit)){
    stop("Not enough information to calculate.  \n",
         "Need either \n",
         "  - multiple portfolios \n",
         "  - single portfolio plus audit environment \n")
  }
  
  if(is.null(retmatrix)&&is.null(strategy)&&is.null(trials)){
    stop("Not enough information to calculate.  \n",
         "Need either \n",
         "  - strategy object with trials slot \n",
         "  - explicit number of trials \n")
  }
  #initialize things we'll need:
  if(!is.null(strategy)){
    if(!is.strategy((strategy))){
      s<-try(getStrategy(strategy))
      if(inherits(s,"try-error"))
        stop ("You must supply an object of type 'strategy'.")
    } else {
      s <- strategy
    }
    s_trials<-s$trials
    if(!is.null(trials) && s_trials>trials){
      trials <- s_trials
    }
    if(is.null(trials)) trials <- s_trials
  }
  if(!is.null(retmatrix)) {
    trials <- ncol(retmatrix)
  }
  if(trials==0 || !is.numeric(trials)){
      stop("You must supply a numeric number of trials or a strategy with trials included")
    }
  #loop over portfolios
  if(is.null(retmatrix)){
    ret<-list()
    for(portfolio in portfolios){
      if(!is.null(audit)){
        if(!is.environment(audit)){
          stop("audit parameter should be an environment containing trial portfolios")
        } else{
          # run dailyStats on all (matching) portfolios if there
          pvec <- ls(pattern = paste0('portfolio.',portfolio),name = audit)
          if(length(pvec)){
            if(length(pvec)>trials) trials <- trials + length(pvec)
            # run dailyStats on all (matching) portfolios if there
            # dailySt <- c(dailySt,lapply(pvec, function(x){ dailyStats(x,perSymbol = FALSE, method='moment', envir=audit) }))
            ret <- c(ret, lapply(pvec, function(x){ ROC(cumsum(.getPortfolio(x, env = .audit)$summary$Net.Trading.PL)) }))
            # dailySt <- do.call(rbind,dailySt)
            ret <- do.call(cbind,ret)
          }
          # put target portfolio first
          # ret <- cbind(portfolios[1],ret)
          # colnames(ret) <- c(portfolios[1],pvec)
          colnames(ret) <- c(pvec)
          ret[is.na(ret)] <- 0
          ret[!is.finite(ret)] <- 0
        }
      } else {
        #if we don't have an audit environment, we just need stats on all the portfolios
        ret <- c(ret, ROC(cumsum(.getPortfolio(portfolio)$summary$Net.Trading.PL)))
        ret <- do.call(cbind,ret)
        colnames(ret) <- portfolios
      }
    }
  } else {
    ret <- retmatrix
  }
  
  # Number of tests assumed
  num_test <- trials
  
  m <- ret
  t <- nrow(m)             # samples per study
  # Trim m to make the combinations truly symmetric
  # TODO: add the trimmed observations back
  if(t%%sm != 0){
    xsrows <- t%%sm
    m <- m[-(1:xsrows),]
    t <- nrow(m)
  }
  n <- ncol(m)             # studies
  cs <- combn(sm,sm/2)       # combinations
  sn <- t / sm              # partition size
  test_config <- bquote(N == .(n) ~~ T == .(t) ~~ S == .(sm))
  
  # initialize results lists
  cs_results <- list()
  
  # a worker iteration function for a single case
  cs_compute <- function(csi) {
    # partition indices
    is_i <- cs[,csi]
    
    # in-sample indices
    is_indices <- as.vector(sapply(is_i,function(i) {
      start <- sn * i - sn + 1
      end <- start + sn - 1
      start:end
    }))
    
    # out-of-sample indices
    os_indices <- setdiff(1:t,is_indices) 
    
    # training and test sets (in sample, out of sample)
    # choosing not to reassign row names of each to 1:(T/2)
    # after R don't need to save J or J_bar so could skip this assignment
    j <- m[is_indices,]
    j_bar <- m[os_indices,]
    
    # compute performance over the N strategies in each subset
    # could use for R any summary statistic e.g. SharpeRatio or Omega
    # r <- mapply(f,j)
    # r_bar <- mapply(f,j_bar)
    r <- f(j)
    r_bar <- f(j_bar)
    
    # compute n* by argmax over R vector
    n_star <- which.max(r)
    n_max_oos <- which.max(r_bar)
    
    # rank of n*th result from OOS performance; converted to (0,1) interval
    os_rank <- rank(r_bar)[n_star]
    omega_bar_c <- os_rank / length(r_bar)
    
    # logit
    # note the value can be Inf
    lambda_c <- log(omega_bar_c / (1 - omega_bar_c))
    
    list(r,r_bar,n_star,n_max_oos,os_rank,omega_bar_c,lambda_c)
  }
  
  # for each partition combination
  cs_results <- NULL
  if ( allow_parallel ) {
    # require(foreach,quietly=TRUE)
    cs_results <- foreach ( csi=1:ncol(cs),
                            .combine=rbind,
                            .multicombine=TRUE) %dopar%
      cs_compute(csi)
  } else {
    for ( csi in 1:ncol(cs) ) {
      cs_results <- rbind(cs_results,cs_compute(csi))
    }
  }
  
  colnames(cs_results) <- c("R","R_bar","n*","n_max_oos","os_rank","omega_bar","lambda")
  rownames(cs_results) <- 1:ncol(cs)
  
  lambda <- as.numeric(cs_results[,"lambda"])
  lambda[which(lambda==Inf)] <- inf_sub # for plotting
  
  # probability of backtest overfit
  # using MC test count approach of lambda count
  phi <- sum(ifelse(lambda<=0,1,0))/ncol(cs)
  # -- alternative might use relative frequency sum 
  # rf <- as.data.frame( table(lambda) / ncol(CS), stringAsFactors=FALSE)
  # phi <- sum(ifelse(rf$lambda <= 0, rf$Freq, 0))
  # -- alternative might use density fit
  # d <- density(lambda,kernel="rectangular")
  
  # performance degradation
  rn_pairs <- as.data.frame(do.call(rbind,lapply(1:ncol(cs),function(i) {
    n <- cs_results[[i,3]]
    r <- cs_results[[i,1]]
    rb <- cs_results[[i,2]]
    return(c(r[n],rb[n]))
  })))
  colnames(rn_pairs) <- c("Rn","Rbn")
  
  # linear fit to pairs, extract results for plot annotations
  linear_fit <- lm(rn_pairs)
  m <- signif(as.numeric(linear_fit$coefficients[1]),digits=5) # slope
  b <- signif(as.numeric(linear_fit$coefficients[2]),digits=5) # intercept
  ar2 <- signif(summary(linear_fit)$adj.r.squared,digits=2) # adj R-squared
  
  # probability out-of-sample below threshold
  p_oos_bt <- signif(length(which(rn_pairs$Rbn<threshold)) / 
                       nrow(rn_pairs),digits=3)
  rv = list(
    results=cs_results,
    combos=cs,
    lambda=lambda,
    phi=phi,
    rn_pairs=rn_pairs,
    func=as.character(substitute(f)),
    slope=m,
    intercept=b,
    ar2=ar2,
    threshold=threshold,
    below_threshold=p_oos_bt,
    test_config=test_config,
    inf_sub=inf_sub)
  class(rv) <- "pbo"
  rv
  
  # result <- pbo.cscv(ret, s=8, f, threshold=0, inf_sub=6, allow_parallel=FALSE)
  # result$call <- match.call()
  # result
} # end pbo wrapper

#' summary method for objects of type \code{pbo}
#'
#' @param object object of type 'pbo' to summarise
#'
#' @param ... \dots any other passthrough parameters
#' @method summary pbo
#' @export
summary.pbo <- function(object,...) {
  writeLines(c(paste("Performance function",
                     object$func,
                     "with threshold",
                     object$threshold),
               ""))
  results <- c(object$phi,object$slope,object$ar2,object$below_threshold)
  names(results) <- c("p_bo","slope","ar^2","p_loss")
  results
}
