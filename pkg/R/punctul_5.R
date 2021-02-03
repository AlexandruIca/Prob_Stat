#PUNCTUL 5
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

momentul_centrat_de_ordin_r<-function(f, r)
{
  #momentul centrat de ordin r: integrala de la -Inf la Inf din (x-media)^r*f(x)
  g<-function(x){(x-media(f))^r}
  return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
}


momentul_initial_de_ordin_r<-function(f, r)
{
  #momentul initial de ordin r: integrala de la -Inf la Inf din x^r*f(x)
  g<-function(x){x^r}
  return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
}

#EXEMPLUL 1
functie1<-function(x)
{
  if(x>0 && x<2)
  {
    return (0.375*(4*x-2*x^2))
  }
  else return(0)
}
media(functie1)
dispersia(functie1)
momentul_centrat_de_ordin_r(functie1, 3)
momentul_initial_de_ordin_r(functie1, 1)

#EXEMPLUL 2
functie2<- function (x)
{
  if (x>=0) return (x^3*exp(-x/2))
  else return (0)
}
media(functie2)
dispersia(functie2)
momentul_centrat_de_ordin_r(functie2, 4)
momentul_initial_de_ordin_r(functie2, 5)

