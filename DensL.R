source("common.R")

dbinorm <- function(x,y){
    xy <- cbind(x,y)

    mu1 <- c(-1,1)
    mu2 <- c(-1,-1)
    mu3 <- c(1,-1)
    mu4 <- c(1,1)
    sigma1 <- matrix(c(4/9,8/45,8/45,4/9),nrow=2,ncol=2)
    sigma2 <- matrix(c(4/9,12/45,12/45,4/9),nrow=2,ncol=2)
    sigma3 <- matrix(c(4/9,-28/90,-28/90,4/9),nrow=2,ncol=2)
    sigma4 <- matrix(c(4/9,-2/9,-2/9,4/9),nrow=2,ncol=2)

    d1 <- dmvnorm(xy,mean=mu1,sigma=sigma1)/8
    d2 <- dmvnorm(xy,mean=mu2,sigma=sigma2)*3/8
    d3 <- dmvnorm(xy,mean=mu3,sigma=sigma3)/8
    d4 <- dmvnorm(xy,mean=mu4,sigma=sigma4)*3/8

    return(d1+d2+d3+d4)
}

rbinorm <- function(n){
    simx <- matrix(0,nrow=n,ncol=2)
    randid <- sample(1:4,size=n,replace=TRUE,prob=c(1/8,3/8,1/8,3/8))
    n1 <- sum(randid==1)
    n2 <- sum(randid==2)
    n3 <- sum(randid==3)
    n4 <- n-n1-n2-n3

    mu1 <- c(-1,1)
    mu2 <- c(-1,-1)
    mu3 <- c(1,-1)
    mu4 <- c(1,1)
    sigma1 <- matrix(c(4/9,8/45,8/45,4/9),nrow=2,ncol=2)
    sigma2 <- matrix(c(4/9,12/45,12/45,4/9),nrow=2,ncol=2)
    sigma3 <- matrix(c(4/9,-28/90,-28/90,4/9),nrow=2,ncol=2)
    sigma4 <- matrix(c(4/9,-2/9,-2/9,4/9),nrow=2,ncol=2)


    simx1 <- rmvnorm(n1,mean=mu1,sigma=sigma1)
    simx2 <- rmvnorm(n2,mean=mu2,sigma=sigma2)
    simx3 <- rmvnorm(n3,mean=mu3,sigma=sigma3)
    simx4 <- rmvnorm(n4,mean=mu4,sigma=sigma4)

    simx[randid==1,] <- simx1
    simx[randid==2,] <- simx2
    simx[randid==3,] <- simx3
    simx[randid==4,] <- simx4
    return(simx)
}

fx.fun <- function(x){
    mu1 <- c(-1,1)
    mu2 <- c(-1,-1)
    mu3 <- c(1,-1)
    mu4 <- c(1,1)
    sigma1 <- matrix(c(4/9,8/45,8/45,4/9),nrow=2,ncol=2)
    sigma2 <- matrix(c(4/9,12/45,12/45,4/9),nrow=2,ncol=2)
    sigma3 <- matrix(c(4/9,-28/90,-28/90,4/9),nrow=2,ncol=2)
    sigma4 <- matrix(c(4/9,-2/9,-2/9,4/9),nrow=2,ncol=2)
        

    d1 <- norm.grad(x,mu=mu1,sigma=sigma1)/8
    d2 <- norm.grad(x,mu=mu2,sigma=sigma2)*3/8
    d3 <- norm.grad(x,mu=mu3,sigma=sigma3)/8
    d4 <- norm.grad(x,mu=mu4,sigma=sigma4)*3/8

    return(d1+d2+d3+d4)
}

fxx.fun <- function(x){
    mu1 <- c(-1,1)
    mu2 <- c(-1,-1)
    mu3 <- c(1,-1)
    mu4 <- c(1,1)
    sigma1 <- matrix(c(4/9,8/45,8/45,4/9),nrow=2,ncol=2)
    sigma2 <- matrix(c(4/9,12/45,12/45,4/9),nrow=2,ncol=2)
    sigma3 <- matrix(c(4/9,-28/90,-28/90,4/9),nrow=2,ncol=2)
    sigma4 <- matrix(c(4/9,-2/9,-2/9,4/9),nrow=2,ncol=2)
        

    d1 <- norm.hess(x,mu=mu1,sigma=sigma1)/8
    d2 <- norm.hess(x,mu=mu2,sigma=sigma2)*3/8
    d3 <- norm.hess(x,mu=mu3,sigma=sigma3)/8
    d4 <- norm.hess(x,mu=mu4,sigma=sigma4)*3/8

    return(d1+d2+d3+d4)
}


