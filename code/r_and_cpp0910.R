xx <- faithful$eruptions
fit <- density(xx)
plot(fit)

fit1 <- density(xx)
fit2 <- replicate(10000, {
  x <- sample(xx, replace = TRUE);
  density(x, from = min(fit1$x), to = max(fit1$x))$y
})
fit3 <- apply(fit2, 1, quantile, c(0.025, 0.975))
plot(fit1, ylim = range(fit3))
polygon(c(fit$x, rev(fit1$x)),
        c(fit3[1,], rev(fit3[2,])),
        col = 'grey', border = TRUE)
lines(fit1)

#1.2.2 
fibR <- function(n) {
  if (n == 0) return(0)
  if (n == 1) return(1)
  return(fibR(n - 1) + fibR(n - 2))
}
fibR(4)
fibR(100)

#1.2.4
library(inline)
incltxt <- '
int fibonacci(const int x) {
if (x == 0) return(0);
if (x == 1) return(1);
return fibonacci(x - 1) + fibonacci(x - 2);
}'

fibRcpp <- cxxfunction(signature(xs = "int"),
                       plugin = "Rcpp",
                       incl = incltxt,
                       body = '
                       int x = Rcpp::as<int>(xs);
                       return Rcpp::wrap(fibonacci(x));
                       ')
