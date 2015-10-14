# @param x a data frame or matrix where each row represents a different record
impute_prelim = function(x, parallel= FALSE, leave_cores= 2) {
  
  # 00. get some initial statistics on missingness.
  numMissing = sum(is.na(x)) 
  
  if(numMissing == 0) {
    return(list (numMissing = numMissing,
                 missing_rows_indices = NULL,
                 missing_cols_indices = NULL,
                 x_missing = NULL))
  }
  
  if (parallel == FALSE) {
    missing_rows_indices = which(apply(x, 1, function(i) {
      any(is.na(i))
    }))
    missing_cols_indices = which(apply(x, 2, function(j) {
      any(is.na(j))
    }))
  } else {
    cl <- makeCluster(detectCores() - leave_cores)
    
    missing_rows_indices = which(parRapply(cl= cl, x, function(i) {
      any(is.na(i))
    }))
    missing_cols_indices = which(parCapply(cl= cl, x, function(j) {
      any(is.na(j))
    }))
    
    stopCluster(cl)
  }
  
  
  if (is.null(rownames(x))) { ## this should NEVER be called in actual usage.
                              ## impute_prelim should only be called by other functions.
    rownames(x) <- 1:nrow(x)  ## it is only needed in testing, when impute_prelim is called alone.
  }
  
  # 01. add a row identifier to x[, <missing columns>]
  # [10/13/15] rownames used as ID for both canopies and imputation process.
  x_missing = cbind(as.integer(rownames(x)), x)[missing_rows_indices,,drop=F]
  
  # 02. return
  return(list (numMissing = numMissing,
               missing_rows_indices = missing_rows_indices,
               missing_cols_indices = missing_cols_indices,
               x_missing = x_missing))
}