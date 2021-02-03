# 6


f <- function(x)
{
  if(x>0 && x<2)
  {
    return(3/8 * (4*x-2*x^2))
  }
  else
  {
    return(0)
  }
}


g <- function(x) x

Multiply=function(a,b){
  force(a)
  force(b)
  function(x){a(x)*b(x)}
}

media <- function(f)
{
  #media: integrala de la -Inf la Inf din x*f(x)
  g<-function(x){x}
  return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
}

dispersia<-function(f)
{
  #dispersia: integrala de la -Inf la Inf din (x-media)^2*f(x)
  g<-function(x){(x-media(f))^2}
  return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
}

comph <- function(f,g)
{
  function(x)
  {
      g(f(x))
  }
}

media(comph(f,g))

dispersia(comph(f,g))


#10

h = function(x,y)  4*x*y  


cov <- function(h, a,b,c,d)
{

  tmpx <- function(x,y)
  {
    Multiply(f=x, h(x,y))
  }
  
  mx <- integrate(Vectorize(function(x) { 
    sapply(x, function(x) {
      integrate(Vectorize(function(y) tmpx), c, d)$value
    })
  }), a, b)$value

  tmpy <- function(x,y)
  {
    Multiply(f=y, h(x,y))
  }
  
  my <- integrate(Vectorize(function(x) { 
    sapply(x, function(x) {
      integrate(Vectorize(function(y) tmpy), c, d)$value
    })
  }), a, b)$value
  
  
  
  tmpxy <- function(x,y)
  {
    Multiply(f=x*y, h(x,y))
  }
  
  mxy <- integrate(Vectorize(function(x) { 
    sapply(x, function(x) {
      integrate(Vectorize(function(y) tmpxy), c, d)$value
    })
  }), a, b)$value
  
  
  mxy - my - mx
  
}

cov(h, 0,1, 0,1)

