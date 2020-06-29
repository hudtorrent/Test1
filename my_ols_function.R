### Test 1 ###

## I just want to test RStudio within GitHub ##

## Linear regression example ##

n <- 300
x <- runif(n,-1,1)
e <- rnorm(n,0,0.3)
b0 <- 2
b1 <- -1
y <- b0 + b1*x + e

## My own function ##

f.ols <- function(X,y){
  # X is supposed to have a colunm of ones
  
  y <- as.matrix(y)
  b <- solve(t(X)%*%X)%*%t(X)%*%y
  yh <- X%*%b
  eh <- y - yh
  
  sqr <- sum(eh^2)
  
  return(list("coef" = b,
              "fittef" = yh,
              "residuals" = eh,
              "SQRes" = sqr))
}
