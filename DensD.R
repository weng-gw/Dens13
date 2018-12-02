source("common.R")
dbinorm <- function(x,y){
    xy <- cbind(x,y)
    sigma1 <- matrix(c(1,2,2,4),nrow=2,ncol=2)
    sigma2 <- matrix(c(4/9,-1/9,-1/9,1/9),nrow=2,ncol=2)

    d1 <- dmvnorm(xy,sigma=sigma1)*2/3
    d2 <- dmvnorm(xy,sigma=sigma2)/3

    return(d1+d2)
}

rbinorm <- function(n){
    simx <- matrix(0,nrow=n,ncol=2)
    randid <- sample(1:2,size=n,replace=TRUE,prob=c(2/3,1/3))
    n1 <- sum(randid==1)
    n2 <- n-n1

    sigma1 <- matrix(c(1,2,2,4),nrow=2,ncol=2)
    sigma2 <- matrix(c(4/9,-1/9,-1/9,1/9),nrow=2,ncol=2)

    simx1 <- rmvnorm(n1,sigma=sigma1)
    simx[randid==1,] <- simx1
    simx2 <- rmvnorm(n2,sigma=sigma2)
    simx[randid==2,] <- simx2

    return(simx)
}

    
fx.fun <- function(x){
    mu1 <- c(0,0)
    mu2 <- mu1
    sigma1 <- matrix(c(1,2,2,4),nrow=2,ncol=2)
    sigma2 <- matrix(c(4/9,-1/9,-1/9,1/9),nrow=2,ncol=2)

    d1 <- norm.grad(x,mu=mu1,sigma=sigma1)*2/3
    d2 <- norm.grad(x,mu=mu2,sigma=sigma2)/3

    return(d1+d2)
}


fxx.fun <- function(x){
    mu1 <- c(0,0)
    mu2 <- mu1
    sigma1 <- matrix(c(1,2,2,4),nrow=2,ncol=2)
    sigma2 <- matrix(c(4/9,-1/9,-1/9,1/9),nrow=2,ncol=2)

    d1 <- norm.hess(x,mu=mu1,sigma=sigma1)*2/3
    d2 <- norm.hess(x,mu=mu2,sigma=sigma2)/3

    return(d1+d2)
}


