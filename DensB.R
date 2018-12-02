source("common.R")
dbinorm <- function(x,y){
    xy <- cbind(x,y)
    sigma <- matrix(c(1,7/10,7/10,1),nrow=2,ncol=2)
    d <- dmvnorm(xy,sigma=sigma)
    return(d)
}

rbinorm <- function(n){
    sigma <- matrix(c(1,7/10,7/10,1),nrow=2,ncol=2)
    simx <- rmvnorm(n,sigma=sigma)
    return(simx)
}

fx.fun <- function(x){
    mu <- c(0,0)
    sigma <- matrix(c(1,7/10,7/10,1),nrow=2,ncol=2)
    return(norm.grad(x=x,mu=mu,sigma=sigma))
}

fxx.fun <- function(x){
    mu <- c(0,0)
    sigma <- matrix(c(1,7/10,7/10,1),nrow=2,ncol=2)
    return(norm.hess(x=x,mu=mu,sigma=sigma))
}
