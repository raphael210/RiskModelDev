

f <- zoo(f[,-1],order.by = f[,1])
rollcov <- function(x, width=10) {
  len <- NROW(x)
  add <- rep(1:(len-width)-1,each=width)
  seq.list <- split(rep(1:width,len-width)+add, add)
  lapply(seq.list, function(y) cov(x[y,]))
}
all <- rollcov(f,36)
all <- do.call('rbind',all)
