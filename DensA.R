source("common.R")
dbinorm <- function(x,y){
    xy <- cbind(x,y)
    sigma <- diag(c(1/4,1))
    d1 <- dmvnorm(xy,sigma=sigma)
    return(d1)
}

rbinorm <- function(n){
    sigma <- diag(c(1/4,1))
    simx <- rmvnorm(n,sigma=sigma)
    return(simx)
}

fx.fun <- function(x){
    mu <- c(0,0)
    sigma <- diag(c(1/4,1))
    return(norm.grad(x,mu=mu,sigma=sigma))
}

fxx.fun <- function(x){
    mu <- c(0,0)
    sigma <- diag(c(1/4,1))
    return(norm.hess(x,mu=mu,sigma=sigma))
}

