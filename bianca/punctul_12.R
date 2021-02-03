clear_plots <- function() {
  if(!is.null(dev.list())) {
    dev.off()
  }
}


#PUNCTUL 12
suma<-function(f.X, f.Y)
{
  #suma
  f.Z <- function(z) integrate(function(x,z) f.Y(z-x)*f.X(x),-Inf,Inf,z)$value
  f.Z
  f.Z <- Vectorize(f.Z)   
  return(f.Z)
}

diferenta<-function(f.X, f.Y)
{
  #diferenta
  f.T <-  function(z) integrate(function(x,z) f.Y(x-z)*f.X(x),-Inf,Inf,z)$value
  f.T
  f.T <- Vectorize(f.T)   
}

#EXEMPLUL 1

set.seed(1)
X <- rnorm(1000,0.5,0.3)
Y <- rlnorm(1000,0.4,0.6)
Z <- X + Y

hist(Z,freq=F,breaks=50, xlim=c(0,30))
z <- seq(0,50,0.01)
lines(z,f.X(z),lty=2,col="yellow")
lines(z,f.Y(z),lty=2,col="green")
lines(z,suma(f.X, f.Y)(z),lty=2,col="red")
lines(z,diferenta(f.X, f.Y)(z),lty=2,col="blue")


#EXEMPLUL 2
clear_plots()
f.X <- function(x) dgamma(x, shape = 1)
f.Y <- function(x) dgamma(x, shape = 2)


set.seed(1)
X <- rgamma(1000, shape=1)
Y <- rgamma(1000, shape=2)
Z <- X + Y
hist(Z,freq=F,breaks=50, xlim=c(0,12))
z <- seq(0,50,0.01)
lines(z,f.X(z),lty=2,col="yellow")
lines(z,f.Y(z),lty=2,col="green")
lines(z,suma(f.X, f.Y)(z),lty=2,col="red")
lines(z,diferenta(f.X, f.Y)(z),lty=2,col="blue")

#EXEMPLUL 3
clear_plots()
expX <- function(x) dexp(x, rate=1)
expY <- function(x) dexp(x, rate=4)

set.seed(1)
X <- rexp(1000,1)
Y <- rexp(1000,4)
Z <- X + Y
hist(Z,freq=F,breaks=50, xlim=c(0,8))
z <- seq(0,50,0.01)
lines(z,expX(z),lty=2,col="yellow")
lines(z,expY(z),lty=2,col="green")
lines(z,suma(expX, expY)(z),lty=2,col="red")
lines(z,diferenta(expX, expY)(z),lty=2,col="blue")
