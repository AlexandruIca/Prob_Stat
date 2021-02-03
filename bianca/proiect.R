#PUNCTUL 1

#Trebuie sa gasim constanta de normalizare astefl incat:
#1) f(x)>=0 pentru orice x
#2) integrala de la -Inf la Inf din f(x) = 1
Multiply=function(a,b){
  force(a)
  force(b)
  function(x){a(x)*b(x)}
}

pozitiva <- function(f)
{
  
  t1 <- seq(-10^(5),10^5,0.05)
  x <- sapply(t1, function(x)f(x))
  
  if(!all(x >=0))
  {
    return (FALSE)
  }
  return (TRUE)
}


punctul_1<-function(functie)
{
  #daca integrala de la -Inf la Inf din f(x) = 1 
  # => C=1/integrala de la -Inf la Inf din f(x)
  #caut sa calculez acest C
  C<-1/(integrate(Vectorize(functie), lower=-Inf, upper = Inf)$value)
  
  #daca cumva am impartit la 0
  if(C==Inf || C==-Inf)
    return("Nu exista constanta de normalizare pentru ca nu se indeplineste conditia 2)!")
  else
  {
    #mai am sa verific 1)
    #iau un range mare de valori in care analizez functia
    
    constanta <- function(x){C}
    functie_finala<- Multiply(constanta,functie)
    
    #daca in oricare dintre aceste puncte funtia e negativa nu se
    #indeplineste cerinta si nu avem o constanta de normalizare
    if(!pozitiva(functie_finala))
    {
      
      return("Nu exista constanta de normalizare pentru ca nu se indeplineste conditia 1)!")
    }
    else
    {
      return(C)
    }
  }
}

#EXEMPLUL 1
#rezultatul trebuie sa fie 3/8=0.375
functie1<- function(x)
{
  if(x>0 && x<2) return(4*x-2*x^2)
  else return(0)
}
punctul_1(functie1)

#EXEMPLUL 2
#rezultatul trebuie sa fie 1/96=0.01041667
functie2<- function (x)
{
  if (x>=0) return (x^3*exp(-x/2))
  else return (0)
}
punctul_1(functie2)

#EXEMPLUL 3
#functie divergenta => trebuie sa iasa la prima conditie
functie3<- function (x)
{
  return(x)
}
punctul_1(functie3)

#EXEMPLUL 4
#functia are valori si negative si pozitive, deci orice valoare
#ar avea C, nu va fi pozitiva intotdeauna => nu avem constanta
functie4<- function (x)
{
  if(x>=-2 && x<=1) return(x)
  else return(0)
}
punctul_1(functie4)


#PUNCTUL 2
isPDF <- function(f)
{
  
  check = TRUE
  #f este densitate de probabilitate <=> f(x) >= 0, oricare x si integrala de la -inf la inf din f(x) este 1
  integralValue <- integrate(Vectorize(f), lower = - Inf, upper = Inf)$value
  
  
  if(abs(integralValue - 1) > 10^(-4))
  { 
    check = FALSE
  }  
  #luam o plaja larga de valori in care sa analizam functia
  t1 <- seq(-10^(5),10^5,0.05)
  x <- sapply(t1, function(x)f(x))
  
  if(!all(x >= 0))
  {
    check = FALSE
  }
  return (check)
}

#Exemple
#Exemplul 1
f1 <- function(x)
{
  
  if(x>=0 && x<=pi/2) return(cos(x))
  return(0)
}
isPDF(f1)
#Exemplul 2
f12 <- function (x)
{
  if(x>=-10 && x <= -4) return(-x^2 - x)
  return(0)
}
isPDF(f12)


##########################################################
# Cerinta 3
##########################################################

# S4 class
# https://www.datacamp.com/community/tutorials/r-objects-and-classes
setClass("continuous_rv", representation(outcome_from="numeric", outcome_to="numeric", density_fn="function"))

# Am pus un x - x inutil pentru a evita erorile de genul:
#   "Unused argument 'x'"
# atunci cand nu se da densitatea explicit
RV <- function(from=0, to=Inf, density=function(x) { if(is.infinite(from) || is.infinite(to)) NULL else 1 / (to - from + 1) + (x - x) }) {
  return(new("continuous_rv", outcome_from=from, outcome_to=to, density_fn=density))
}

# Pentru a putea printa 'frumos' o variabila aleatoare fac overload pentru 'show'
setMethod("show", signature(object="continuous_rv"), function(object) {
  num_width <- 12
  interval <- seq(object@outcome_from, min(object@outcome_from + 9, object@outcome_to), 1)
  cat("Eveniment:     ")
  print(noquote(format(interval, width=num_width)))
  cat("Probabilitate: ")
  print(noquote(format(Vectorize(object@density_fn, "x")(interval), width=num_width)))
})

# Se poate da ca parametru orice functie
sample_dice <- RV(from=1, to=6)
sample_identity <- RV(from=1, density=function(x) { x })
sample_parabola <- RV(from=1, to=100, density=function(x) { x^2 })

# Exemple pe functiile de la 4)
sample_normal_dist <- RV(from=1, density=function(x) { density_normal_distribution(x, m=3, sd=5)  })
sample_exponential_dist <- RV(from=1, density=function(x) { density_exponential_distribution(x, lambda=2) })
sample_gamma_dist <- RV(from=1, density=function(x) { density_gamma_distribution(x, shape=2, scale=1/2) })


##########################################################
# Cerinta 4
##########################################################

# Helper pentru a sterge graficele din 'Plots' in Rstudio
clear_plots <- function() {
  if(!is.null(dev.list())) {
    dev.off()
  }
}

# Densitate pentru repartitia normala
# https://en.wikipedia.org/wiki/Normal_distribution
# Puteam folosi si dnorm
#
# m: media
# sd: deviatia standard
density_normal_distribution <- function(x, m, sd) {
  return(exp(-0.5 * ((x - m) / sd) ^ 2) / (1 / sd * sqrt(2 * pi)))
}

# Densitate pentru repartitia exponentiala
# https://en.wikipedia.org/wiki/Exponential_distribution
density_exponential_distribution <- function(x, lambda) {
  return(ifelse(x >= 0, lambda * exp(-lambda * x), 0))
}

# CDF pentru repartitia exponentiala
cdf_exponential_distribution <- function(x, lambda) {
  return(ifelse(x >= 0, lambda * exp(-lambda * x), 0))
}

# Densitate pentru repartitia gamma
# https://en.wikipedia.org/wiki/Gamma_distribution
density_gamma_distribution <- function(x, shape, scale) {
  return(((scale ^ shape) * (x ^ (shape - 1)) * exp(-shape * x)) / gamma(shape))
}

# CDF pentru repartitia gamma
# Aici am folosit direct pgamma deoarce nu am stiut cum sa traduc toate functiile din cdf-ul normal
cdf_gamma_distribution <- function(x, shape_, scale_) {
  return(pgamma(x, shape=shape_, scale=scale_))
}

# Functie generica ce accepta orice functie f cu un parametru x pe care o va desena
# cdf se poate da ca argument daca se stie o functie de distributie cumulativa pentru f
probability_any_distribution <- function(f, dist_name, center, offset, step=0.01, cdf=NULL) {
  from <- center - offset
  to <- center + offset
  step <- 0.01
  interval <- seq(from, to, step)
  
  if(is.null(cdf)) {
    probability <- function(t) {
      return(integrate(f, lower=-Inf, upper=t)$value)
    }
    
    # aplica functia probability pe un interval
    vect_probability = Vectorize(probability, vectorize.args='t')
    
    # Pun 2 grafice unul langa altul
    par(mfrow=c(1, 2))
    plot(f(x=interval), col="blue", xlab=paste("Densitate pentru ", dist_name, sep=""), ylab="")
    plot(interval, vect_probability(interval), xlab=paste("Probabilitate pentru ", dist_name, sep=""), ylab="")
  }
  else {
    par(mfrow=c(1, 2))
    plot(f(x=interval), col="blue", xlab=paste("Densitate pentru ", dist_name, sep=""), ylab="")
    plot(cdf(x=interval), xlab=paste("Probabilitate pentru", dist_name, sep=""), ylab="")
  }
}

# Exemple:

clear_plots()
probability_any_distribution(
  f=function(x) { return(density_exponential_distribution(x, lambda=1)) },
  cdf=function(x) { return(cdf_exponential_distribution(x, lambda=1)) },
  dist_name="repartitia exponentiala",
  center=0,
  offset=20
)

clear_plots()
# Exponentiala fara CDF
probability_any_distribution(
  f=function(x) { return(density_exponential_distribution(x, lambda=1)) },
  dist_name="repartitia exponentiala",
  center=0,
  offset=20
)

clear_plots()
probability_any_distribution(
  f=function(x) { return(density_gamma_distribution(x, shape=2, scale=1/2)) },
  cdf=function(x) { return(cdf_gamma_distribution(x, shape_=2, scale_=1/2)) },
  dist_name="repartitia gamma",
  center=0,
  offset=20
)

clear_plots()
probability_any_distribution(
  f=function(x) { return(density_normal_distribution(x, m=5, sd=3)) },
  dist_name="repartitia normala",
  center=0,
  offset=20
)


#PUNCTUL 5
media <- function(f)
{
  #media: integrala de la -Inf la Inf din x*f(x)
  if(isPDF(f))
  {
    g<-function(x){x}
    return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
  }
  else
  {
    return("Functia nu este functie densitate de probabilitate!")
  }
}

dispersia<-function(f)
{
  #dispersia: integrala de la -Inf la Inf din (x-media)^2*f(x)
  if(isPDF(f))
  {
    g<-function(x){(x-media(f))^2}
    return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
  }
  else
  {
    return("Functia nu este functie densitate de probabilitate!")
  }
}

momentul_centrat_de_ordin_r<-function(f, r)
{
  #momentul centrat de ordin r: integrala de la -Inf la Inf din (x-media)^r*f(x)
  if(isPDF(f))
  {
    g<-function(x){(x-media(f))^r}
    return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
  }
  else
  {
    return("Functia nu este functie densitate de probabilitate!")
  }
}

momentul_initial_de_ordin_r<-function(f, r)
{
  #momentul initial de ordin r: integrala de la -Inf la Inf din x^r*f(x)
  if(isPDF(f))
  {
    g<-function(x){x^r}
    return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
  }
  else
  {
    return("Functia nu este functie densitate de probabilitate!")
  }
}

#EXEMPLUL 1
functie1<-function(x)
{
  if(x>0 && x<2)
  {
    return (3/8*(4*x-2*x^2))
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

#PUNTUL 6


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
  if(isPDF(f))
  {
    g<-function(x){x}
    return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
  }
  else
  {
    return("Functia nu este functie densitate de probabilitate!")
  }
}

media <- function(f)
{
  #media: integrala de la -Inf la Inf din x*f(x)
  if(isPDF(f))
  {
    g<-function(x){x}
    return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
  }
  else
  {
    return("Functia nu este functie densitate de probabilitate!")
  }
}

comph <- function(f,g)
{
  if(isPDF(f))
    function(x)
    {
      g(f(x))
    }
  else
  {
    return("Functia nu este functie densitate de probabilitate!")
  }
}

media(comph(f,g))

dispersia(comph(f,g))


#PUNCTUL 11

marginalY <- function(y,fxy) sapply(y, function(yy) integrate(Vectorize(fxy),lower = -Inf, upper = Inf, y = yy)$value)

marginalX <- function(x,fxy) sapply(x, function(xx) integrate(Vectorize(fxy),lower = -Inf, upper = Inf, x = xx)$value)



conditionalX <- function(fxy,x,y)
{
  fxy(x,y)/marginalY(y,fxy)
}
conditionalY <- function(fxy,x,y)
{
  fxy(x,y)/marginalX(x,fxy)
  
}

#Exemple
#Exemplul 1
fxy1 <- function(x,y)
{
  if(0 <= x && x <= 1 && 0 <=y && y <= 1)
    return (x + 3/2*y^2)
  return (0)
}

marginalX(1,fxy1)
marginalY(0.5,fxy1)
marginalY(10,fxy1)

#Exemplul 2
fxy2 <- function(x,y)
{
  if(0 <= x && x <= 1 && 0 <= y && y <= 2)
    return ((x^2)/4 + (y^2)/4 + (x*y)/6)
  return (0)
}
marginalY(1,fxy2)
conditionalX(fxy2, 0.2, 1)

#Exemplul 3
fxy3 <- function(x,y)
{
  if (0 <= y && y <=x && x <=1)
    return (10*x^2*y)
  return (0)
  
}
marginalX(0.2,fxy3)
marginalX(3,fxy3)
marginalY(2,fxy3)
marginalY(0.6,fxy3)



# install.packages("stringr")
# install.packages("sets")
library(stringr)
library(sets)

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
