#' Add capital account transactions, such as capital additions and withdrawals or interest income (expense)
#' 
#' For the specified Account, take in the date, amount, and type of transaction and append it to the correct list in the account object
#' 
#' @param Account Account name, as string
#' @param TxnDate  transaction date as ISO 8601, e.g., '2008-09-01' or '2010-01-05 09:54:23.12345'
#' @param TxnType string indicating the type of account transaction, only "Addition", "Withdrawal", or "Interest" are currently supported
#' @param Amount As of now, the currency of the transaction MUST MATCH the currency of the Account.  Patches welcome.
#' @param \dots any other passthrough parameters  
#' @param verbose  If TRUE (default) the function prints the elements of the transaction in a line to the screen, e.g., "2007-01-08 Withdrawal 15,012,235". Suppress using FALSE.
#' @details
#' Adds capital transactions to a rudimentary transactions table in the Account object.  This may be useful when tracking the denominator of returns when there are changes to the account's capital or significant interest income.
#' In the Account$summary table, there are several placeholder columns that mimic the CFTC's 13-column report.  Columns of interest here are "Additions", "Withdrawals", and "Interest".  
#' Transactions added with this function will be added into the appropriate one of three slots in the Account object (Account$additions, Account$withdrawals, or Account$Interest), which contains an xts object of individual transactions with a date and amount.  The \code{\link{updateAcct}} function will read the transactions from each list in turn, aggregate them by the specified date scope, and slot them into the \code{Account$summary} table as it's built. \code{\link{updateEndEq}} should then just work.
#' 
#' @seealso \code{\link{initAcct}}, \code{\link{updateAcct}}, \code{\link{updateEndEq}}
#' @author Peter Carl
#' @export
addAcctTxn <- function(Account, TxnDate, TxnType = c("Additions", "Withdrawals", "Interest"), Amount, ..., verbose=TRUE)
{ # @author Peter Carl    
  aname <- Account
  
  if(!is.timeBased(TxnDate) ){
    TxnDate<-as.POSIXct(TxnDate)
  }
  
  Account<-get(paste("account",aname,sep='.'),envir=.blotter)
  
  NewTxn = xts(t(c(Amount)), order.by=TxnDate)
  # case:
  switch(TxnType,
         Additions = { Account$Additions<-rbind(Account$Additions, NewTxn) },
         Withdrawals = { Account$Withdrawals<-rbind(Account$Withdrawals, NewTxn) },
         Interest = { Account$Interest<-rbind(Account$Interest, NewTxn) },
         stop("Not a valid transaction type. Select from 'Additions', 'Withdrawals', or 'Interest'.")
    )
  
  if(verbose)
    print(paste(format(TxnDate, "%Y-%m-%d %H:%M:%S"), TxnType, Amount, sep=" "))
  
  assign(paste("account",aname,sep='.'), Account, envir=.blotter)
}
