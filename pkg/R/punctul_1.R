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
