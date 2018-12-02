source("common.R")
dbinorm <- function(x,y){
    d1 <- dnorm(x,mean=0,sd=1/2)*dnorm(y,mean=0,sd=1)*2/3
    d2 <-
        dnorm(x,mean=0,sd=sqrt(1/200))*dnorm(y,mean=0,sd=sqrt(1/50))/3
    return(d1+d2)
}

rbinorm <- function(n){
    xsd <- c(1/2,sqrt(1/200))
    ysd <- c(1,sqrt(1/50))
    id <- sample(1:2,n,replace=TRUE,prob=c(2/3,1/3))
    simx <- rnorm(n,mean=0,sd=xsd[id])
    simy <- rnorm(n,mean=0,sd=ysd[id])
    return(cbind(simx,simy))
}


fx.fun <- function(x){
    mu1 <- c(0,0)
    mu2 <- c(0,0)
    sigma1 <- diag(c(1/4,1))
    sigma2 <- sigma1/50
    
    d1 <- norm.grad(x,mu=mu1,sigma=sigma1)*2/3
    d2 <- norm.grad(x,mu=mu2,sigma=sigma2)/3

    return(d1+d2)
}


fxx.fun <- function(x){
    mu1 <- c(0,0)
    mu2 <- c(0,0)
    sigma1 <- diag(c(1/4,1))
    sigma2 <- sigma1/50
    
    d1 <- norm.hess(x,mu=mu1,sigma=sigma1)*2/3
    d2 <- norm.hess(x,mu=mu2,sigma=sigma2)/3

    return(d1+d2)
}

