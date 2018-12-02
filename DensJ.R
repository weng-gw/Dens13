source("common.R")

dbinorm <- function(x,y){
    xy <- cbind(x,y)
    mu1 <- c(-6/5,0)
    mu2 <- c(6/5,0)
    mu3 <- c(0,0)
    sigma1 <- matrix(c(9/25,63/250,63/250,9/25),nrow=2,ncol=2)
    sigma2 <- matrix(c(9/25,63/250,63/250,9/25),nrow=2,ncol=2)
    sigma3 <- matrix(c(9/25,-63/250,-63/250,9/25),nrow=2,ncol=2)

    d1 <- dmvnorm(xy,mean=mu1,sigma=sigma1)/3
    d2 <- dmvnorm(xy,mean=mu2,sigma=sigma2)/3
    d3 <- dmvnorm(xy,mean=mu3,sigma=sigma3)/3

    return(d1+d2+d3)
}


rbinorm <- function(n){
    simx <- matrix(0,nrow=n,ncol=2)
    randid <- sample(1:3,size=n,replace=TRUE)
    n1 <- sum(randid==1)
    n2 <- sum(randid==2)
    n3 <- n-n1-n2

    mu1 <- c(-6/5,0)
    mu2 <- c(6/5,0)
    mu3 <- c(0,0)
    sigma1 <- matrix(c(9/25,63/250,63/250,9/25),nrow=2,ncol=2)
    sigma2 <- matrix(c(9/25,63/250,63/250,9/25),nrow=2,ncol=2)
    sigma3 <- matrix(c(9/25,-63/250,-63/250,9/25),nrow=2,ncol=2)

    simx1 <- rmvnorm(n1,mean=mu1,sigma=sigma1)
    simx2 <- rmvnorm(n2,mean=mu2,sigma=sigma2)
    simx3 <- rmvnorm(n3,mean=mu3,sigma=sigma3)

    simx[randid==1,] <- simx1
    simx[randid==2,] <- simx2
    simx[randid==3,] <- simx3
    return(simx)
}


fx.fun <- function(x){
    mu1 <- c(-6/5,0)
    mu2 <- c(6/5,0)
    mu3 <- c(0,0)
    sigma1 <- matrix(c(9/25,63/250,63/250,9/25),nrow=2,ncol=2)
    sigma2 <- matrix(c(9/25,63/250,63/250,9/25),nrow=2,ncol=2)
    sigma3 <- matrix(c(9/25,-63/250,-63/250,9/25),nrow=2,ncol=2)

    d1 <- norm.grad(x,mu=mu1,sigma=sigma1)/3
    d2 <- norm.grad(x,mu=mu2,sigma=sigma2)/3
    d3 <- norm.grad(x,mu=mu3,sigma=sigma3)/3

    return(d1+d2+d3)
}



fxx.fun <- function(x){
    mu1 <- c(-6/5,0)
    mu2 <- c(6/5,0)
    mu3 <- c(0,0)
    sigma1 <- matrix(c(9/25,63/250,63/250,9/25),nrow=2,ncol=2)
    sigma2 <- matrix(c(9/25,63/250,63/250,9/25),nrow=2,ncol=2)
    sigma3 <- matrix(c(9/25,-63/250,-63/250,9/25),nrow=2,ncol=2)

    d1 <- norm.hess(x,mu=mu1,sigma=sigma1)/3
    d2 <- norm.hess(x,mu=mu2,sigma=sigma2)/3
    d3 <- norm.hess(x,mu=mu3,sigma=sigma3)/3

    return(d1+d2+d3)
}

