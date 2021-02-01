#punctul 1

functie1<- function(x)
{
  if(x>=0 && x<=1) return((exp(-x)+exp(x)))
  else return(0)
}
functie2<- function (n, x)
{
  if (x>=0) return ((1/gamma(n/2))*3^(-n/2)*x^(n/2-1)*exp(-(x)/3))
  else return (0)
}

punctul_1<-function(functie)
{
  # x<-1/(integrate(Vectorize(functie), lower=-Inf, upper = Inf)$value)
  # if(x==Inf)
  #   return("Nu exista constanta de normalizare!")
  # else
  #   return(x)
}


Multiply=function(a,b){
  force(a)
  force(b)
  function(x){a(x)*b(x)}
}
a<-antiD(x^-0.5 ~ x)
a
x<-punctul_1(functie1)
x
y<-punctul_1_bis(functie2)
y
#punctul 5
media <- function(f)
{
  g<-function(x){x}
  return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
}
y<-media(functie)
y

dispersia<-function(f)
{
  g<-function(x){(x-media(f))^2}
  return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
}
z<-dispersia(functie)
z

momentul_centrat_de_ordin_r<-function(f, r)
{
  g<-function(x){(x-media(f))^r}
  return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
}
momentul_centrat_de_ordin_2<-momentul_centrat_de_ordin_r(functie, 2)
momentul_centrat_de_ordin_2


momentul_initial_de_ordin_r<-function(f, r)
{
  g<-function(x){x^r}
  return(integrate(Vectorize(Multiply(g, f)), lower = -Inf, upper = Inf)$value)
}

#punctul 12
#suma si diferenta a doua variablile continue aleatoare 
#independente cu formula convolutiei

# Fie (X, Y) o variabila aleatoare continua cu densitatea
# de probabilitate f_XY.
# Facem transformare:
# u = x+y
# v = x
# Care admite inversa:
# x = v
# y = (u-v)

# Daca X si Y sunt independente =>
# f_XY(v, u-v) = f_x(v)f_Y(u-v)
# si densitatea sumei este produsul de convolutie al
# densitatilor f_X si f_Y, adica
# fU(u)=integrala de la -inf la + inf din f_X(v)
# si f_Y(u-v) dv


punctul_12 <- function(functie2)
{
  u=x+y
  v=x
  return(integral(functie2(x, v)*functie2(y, u-v), lower=-Inf, upper= Inf))
}
a <- punctul_12(functie2)
