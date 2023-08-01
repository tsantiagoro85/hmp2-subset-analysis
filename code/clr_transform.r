# CLR transform function

require(ALDEx2)
require(zCompositions)

# Get CLR using ALDEx median
# Courtesy of Brandon Lieng
# denom can be (all, iqlr, zero, lvha, median,user). See ALDEx2 documentation for description

get.clr <- function(x, denom="all") {
  d.clr.list <- suppressMessages(aldex.clr(x, denom=denom))
  d.clr <- data.frame(matrix(ncol=ncol(x), nrow=nrow(x)))
  colnames(d.clr) <- colnames(x)
  rownames(d.clr) <- rownames(x)
  
  for (i in 1:ncol(x)) {
    d.clr[,i] <- apply(d.clr.list@analysisData[i][[1]], 1, function(y) {return(median(y))})
  }
  
  # Transpose -- samples are now ROWS and features are COLUMNS
  return(t(d.clr))
}

# Ge the CLR using czm zero estimator
clr.czm <-function(x) {
  d.czm <- cmultRepl(as.matrix(t(x)), label=0, method="CZM")
  # returns the default proportions output
  # Sampes are now ROWS and features are COLUMNS
  cat("Samples are now rows, and features are columns")
  return(d.czm)
}