source("common.R")
dbinorm <- function(x,y){
    xy <- cbind(x,y)
    mu1 <- c(0,0)
    mu2 <- c(1/2,1/2)
    mu3 <- c(13/12,13/12)
    sigma1 <- diag(c(1,1))
    sigma2 <- diag(c(4/9,4/9))
    sigma3 <- diag(c(25/81,25/81))

    d1 <- dmvnorm(xy,mean=mu1,sigma=sigma1)/5
    d2 <- dmvnorm(xy,mean=mu2,sigma=sigma2)/5
    d3 <- dmvnorm(xy,mean=mu3,sigma=sigma3)*3/5
    return(d1+d2+d3)
}

rbinorm <- function(n){
    simx <- matrix(0,nrow=n,ncol=2)
    randid <- sample(1:3,size=n,replace=TRUE,prob=c(1/5,1/5,3/5))
    n1 <- sum(randid==1)
    n2 <- sum(randid==2)
    n3 <- n-n1-n2

    mu1 <- c(0,0)
    mu2 <- c(1/2,1/2)
    mu3 <- c(13/12,13/12)
    sigma1 <- diag(c(1,1))
    sigma2 <- diag(c(4/9,4/9))
    sigma3 <- diag(c(25/81,25/81))

    simx1 <- rmvnorm(n1,mean=mu1,sigma=sigma1)
    simx[randid==1,] <- simx1
    simx2 <- rmvnorm(n2,mean=mu2,sigma=sigma2)
    simx[randid==2,] <- simx2
    simx3 <- rmvnorm(n3,mean=mu3,sigma=sigma3)
    simx[randid==3,] <- simx3

    return(simx)
}


fx.fun <- function(x){
    mu1 <- c(0,0)
    mu2 <- c(1/2,1/2)
    mu3 <- c(13/12,13/12)
    sigma1 <- diag(c(1,1))
    sigma2 <- diag(c(4/9,4/9))
    sigma3 <- diag(c(25/81,25/81))

    d1 <- norm.grad(x,mu=mu1,sigma=sigma1)/5
    d2 <- norm.grad(x,mu=mu2,sigma=sigma2)/5
    d3 <- norm.grad(x,mu=mu3,sigma=sigma3)*3/5

    return(d1+d2+d3)
}

fxx.fun <- function(x){
    mu1 <- c(0,0)
    mu2 <- c(1/2,1/2)
    mu3 <- c(13/12,13/12)
    sigma1 <- diag(c(1,1))
    sigma2 <- diag(c(4/9,4/9))
    sigma3 <- diag(c(25/81,25/81))

    d1 <- norm.hess(x,mu=mu1,sigma=sigma1)/5
    d2 <- norm.hess(x,mu=mu2,sigma=sigma2)/5
    d3 <- norm.hess(x,mu=mu3,sigma=sigma3)*3/5

    return(d1+d2+d3)
}

    
