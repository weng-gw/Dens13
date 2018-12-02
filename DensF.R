source("common.R")
dbinorm <- function(x,y){
    xy <- cbind(x,y)
    sigma <- diag(c(1/16,1))
    mu1 <- c(-3/2,0)
    mu2 <- c(3/2,0)

    d1 <- dmvnorm(xy,mean=mu1,sigma=sigma)/2
    d2 <- dmvnorm(xy,mean=mu2,sigma=sigma)/2

    return(d1+d2)
}


rbinorm <- function(n){
    simx <- matrix(0,nrow=n,ncol=2)
    randid <- sample(1:2,size=n,replace=TRUE)
    n1 <- sum(randid==1)
    n2 <- n-n1

    sigma <- diag(c(1/16,1))
    mu1 <- c(-3/2,0)
    mu2 <- c(3/2,0)

    simx1 <- rmvnorm(n1,mean=mu1,sigma=sigma)
    simx[randid==1,] <- simx1
    simx2 <- rmvnorm(n2,mean=mu2,sigma=sigma)
    simx[randid==2,] <- simx2

    return(simx)
}

fx.fun <- function(x){
    sigma <- diag(c(1/16,1))
    mu1 <- c(-3/2,0)
    mu2 <- c(3/2,0)
    
    d1 <- norm.grad(x,mu=mu1,sigma=sigma)/2
    d2 <- norm.grad(x,mu=mu2,sigma=sigma)/2

    return(d1+d2)
}

fxx.fun <- function(x){
    sigma <- diag(c(1/16,1))
    mu1 <- c(-3/2,0)
    mu2 <- c(3/2,0)
    
    d1 <- norm.hess(x,mu=mu1,sigma=sigma)/2
    d2 <- norm.hess(x,mu=mu2,sigma=sigma)/2

    return(d1+d2)
}


    
