#' rollcov
#'
#' calculate rolling covariance
#' @author Andrew Dow
#' @param x is a zoo object
#' @param width is the rolling window.
#' @return a covariance data frame.
#' @examples 
#' rollcov(f,36)
f <- zoo(f[,-1],order.by = f[,1])
rollcov <- function(x, width=10) {
  len <- NROW(x)
  add <- rep(1:(len-width)-1,each=width)
  seq.list <- split(rep(1:width,len-width)+add, add)
  lapply(seq.list, function(y) cov(x[y,]))
}
all <- rollcov(f,36)
all <- do.call('rbind',all)
