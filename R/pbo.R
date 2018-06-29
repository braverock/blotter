#' @title Probability of backtest overfitting
#' @description Performs the probability of backtest overfitting computations.
#' @details
#' This function performs the probability of backtest overfitting calculation
#' using a combinatorially-symmetric cross validation (CSCV) approach. 
#' 
#' @param m a \eqn{TxN} data frame of returns, where \eqn{T} is the samples per study and \eqn{N} is the number of studies.
#' @param s the number of subsets of \code{m} for CSCV combinations; 
#' must evenly divide \code{m} 
#' @param f the function to evaluate a study's performance; required
#' @param threshold the performance metric threshold 
#' (e.g. 0 for Sharpe, 1 for Omega)
#' @param inf_sub infinity substitution value for reasonable plotting
#' @param allow_parallel whether to enable parallel processing, default FALSE
#' @keywords probability backtest overfitting PBO CSCV
#' @return object of class \code{pbo} containing list of PBO calculation results 
#' and settings
#' @export
#' @importFrom utils combn
#' @references Baily et al., "The Probability of Backtest Overfitting," 
#' \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2326253}
#' @examples
#' \dontrun{
#' require(pbo)
#' require(PerformanceAnalytics)
#' n <- 100
#' t <- 1000
#' s <- 8
#' m <- data.frame(matrix(rnorm(n*t,mean=0,sd=1),
#'   nrow=t,ncol=n,byrow=TRUE,
#'   dimnames=list(1:t,1:n)),
#'   check.names=FALSE)
#' p <- pbo(m,s,f=Omega,threshold=1)
#' }
pbo <- function(m,s=4,f=NA,threshold=0,inf_sub=6,allow_parallel=FALSE) {
  stopifnot(is.function(f))
  
  t <- nrow(m)             # samples per study
  n <- ncol(m)             # studies
  cs <- combn(s,s/2)       # combinations
  sn <- t / s              # partition size
  test_config <- bquote(N == .(n) ~~ T == .(t) ~~ S == .(s))
  
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
}

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