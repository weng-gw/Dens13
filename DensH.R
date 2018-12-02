source("common.R")
dbinorm <- function(x,y){
    xy <- cbind(x,y)
    mu1 <- c(1,-1)
    mu2 <- c(-1,1)
    sigma1 <- matrix(c(4/9,28/90,28/90,4/9),nrow=2,ncol=2)
    sigma2 <- diag(2)*4/9

    d1 <- dmvnorm(xy,mean=mu1,sigma=sigma1)/2
    d2 <- dmvnorm(xy,mean=mu2,sigma=sigma2)/2

    return(d1+d2)
}

rbinorm <- function(n){
    simx <- matrix(0,nrow=n,ncol=2)
    randid <- sample(1:2,size=n,replace=TRUE)
    n1 <- sum(randid==1)
    n2 <- n-n1

    mu1 <- c(1,-1)
    mu2 <- c(-1,1)
    sigma1 <- matrix(c(4/9,28/90,28/90,4/9),nrow=2,ncol=2)
    sigma2 <- diag(2)*4/9


    simx1 <- rmvnorm(n1,mean=mu1,sigma=sigma1)
    simx[randid==1,] <- simx1
    simx2 <- rmvnorm(n2,mean=mu2,sigma=sigma2)
    simx[randid==2,] <- simx2

    return(simx)
}


fx.fun <- function(x){
    mu1 <- c(1,-1)
    mu2 <- c(-1,1)
    sigma1 <- matrix(c(4/9,28/90,28/90,4/9),nrow=2,ncol=2)
    sigma2 <- diag(2)*4/9
    
    d1 <- norm.grad(x,mu=mu1,sigma=sigma1)/2
    d2 <- norm.grad(x,mu=mu2,sigma=sigma2)/2

    return(d1+d2)
}

fxx.fun <- function(x){
    mu1 <- c(1,-1)
    mu2 <- c(-1,1)
    sigma1 <- matrix(c(4/9,28/90,28/90,4/9),nrow=2,ncol=2)
    sigma2 <- diag(2)*4/9
    
    d1 <- norm.hess(x,mu=mu1,sigma=sigma1)/2
    d2 <- norm.hess(x,mu=mu2,sigma=sigma2)/2

    return(d1+d2)
}


