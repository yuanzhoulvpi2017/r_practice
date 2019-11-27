mysum <- function(x, n) {
  i <- 0
  res <- 0
  while(i <= n) {
    
    res <- res + x ** i
    #cat("i: ", i, "\tres:", res, "\n")
    i <- i + 1
  }
  return(res)
}

mysum(-1, 2)

