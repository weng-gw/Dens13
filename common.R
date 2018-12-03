library(mvtnorm)

## Function for calculating the gradient of a bivariate normal distribution.
## Not vectorized yet!!!
norm.grad <- function(x, mu,sigma){
    fvalue <-
        dmvnorm(x,mean=mu,sigma=sigma)*as.numeric(-qr.solve(sigma,x-mu))
    return(fvalue)
}

## Function for calculating the Hessian of a bivariate normal distribution.
## Not vectorized yet!!!
norm.hess <- function(x,mu,sigma){
    omega <- qr.solve(sigma)
    xbar <- qr.solve(sigma, x-mu)
    fvalue <- tcrossprod(xbar)-omega
    return(fvalue*dmvnorm(x,mean=mu,sigma=sigma))
}

