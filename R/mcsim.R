#' Retrieves the most recent value of the capital account
#' @param Account string identifier of account
#' @param n number of monte carlo simulations
#' @param Replace boolean for sampling with or without replacement DEFAULT = TRUE
#' @return a ggplot object of simulation bands
#' @note 
#' Requires ggplot2 package
#' @export
#' @author Jasen Mackie, Brian G. Peterson
#' @seealso \code{\link{ggplot}}

mcsim <- function(Account, n = 100, Replace = TRUE){
  
    a <- getAccount(Account)
    EndEq <- a$summary$End.Eq
  
    s1.dates <- index(a$summary)
  

    ret <- ROC(EndEq)
    chart.CumReturns(ret)
  
    # Set up for Sample() and Replicate()
    ret_sample <- replicate(n,sample(as.vector(ret[-1,]), replace=Replace)) #use ret[-1] so we exclude 1st NA value from ROC calc
    ret_cum_sample <- apply(ret_sample, 2, function(x) cumsum(x))
    ret_cum_samplexts <- xts(ret_cum_sample, s1.dates[-1]) #use s1.dates[-1] so that length of dates is identical to length of ret_sample
  
    # Build the 5% and 95% quantile datasets
    # TODO add params for user defined quantiles
    ret_5 <- apply(ret_cum_samplexts, 1, function(x) quantile(x, .05))
    ret_5 <- as.xts(ret_5)
  
    ret_95 <- apply(ret_cum_samplexts, 1, function(x) quantile(x, .95))
    ret_95 <- as.xts(ret_95)
  
    ret_25 <- apply(ret_cum_samplexts, 1, function(x) quantile(x, .25))
    ret_25 <- as.xts(ret_25)
  
    ret_75 <- apply(ret_cum_samplexts, 1, function(x) quantile(x, .75))
    ret_75 <- as.xts(ret_75)
  
    charts <- merge(ret_5, ret_95, ret_25, ret_75)
  
    # Draw the graph with a ribbon
    h <- ggplot(charts, aes(x = index(charts))) +
      geom_ribbon(aes(ymin = ret_25, ymax = ret_75, colour = "50%"), alpha = 0.3, fill = "red3") +
      geom_ribbon(aes(ymin = ret_5, ymax = ret_95, colour = "90%"), alpha = 0.3, fill = "cornflowerblue") +
      theme(axis.text.x = element_text(angle=0, hjust = 0),
            axis.title = element_text(face = 'bold', size = 14),
            title = element_text(face = 'bold', size = 16),
            legend.position = 'bottom',
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.key.width = unit(2, 'cm'))
    #h <- h + geom_line(aes(y = cumsum(ret[-1,])), colour = "black", linetype = 1) +
    h <- h + #geom_line(aes(y = EndEq[-1,])), colour = "black", linetype = 1) +
      # TODO need to figure out why geom_line returns aes(no 'y' error) 
      ylab(label="Cumulative Returns") +
      xlab(label="Time") +
      ggtitle("Returns Distribution")
    #h
  
    return(h)
}