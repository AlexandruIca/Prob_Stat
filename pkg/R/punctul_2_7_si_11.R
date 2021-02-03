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

#PUNCTUL 7
cdf <- function(f, a)
{
  return (integrate(Vectorize(f), lower = -Inf, upper = a)$value)
}

getFunction <- function(r)
{
  get(regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*", r)))
}
getNumbers <- function(r)
{
   t1 <- str_extract_all(r,"(>|>=|<|<=)\\s*[0-9]+([.][0-9]+)*")
   listOfNr <- sapply(t1, function(x) str_extract_all(x,"[0-9]+([.][0-9]+)*"))
   rez <- tuple(0)
   ifelse(sum(lengths(listOfNr)) == 1, rez <- tuple(as.numeric(listOfNr[[1]])), rez <- tuple(as.numeric(listOfNr[[1]]),as.numeric(listOfNr[[2]])))
   return (rez)
}
getOperators <- function(r)
{
   listOfOperators <- str_extract_all(r,"(<|>)")
   rez <- tuple(0)
   ifelse(lengths(listOfOperators) == 1, rez <- tuple(listOfOperators[[1]][1]), rez <- tuple(listOfOperators[[1]][1],listOfOperators[[1]][2]))
   return (rez)
}
isSimpleExpression <- function(r)
{
  (!identical(character(0), regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*\\s*(<|<=|>=|>)\\s*[0-9]+([.][0-9]+)*\\s*$",r))))
}
isCompoundExpression <- function(r)
{
  (!identical(character(0), regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*\\s*(>|>=|<|<=)\\s*[0-9]+([.][0-9]+)*\\s+(&&|\\|)\\s+[A-Za-z]+[0-9]*\\s*(<|<=|>|>=)\\s*[0-9]+([.][0-9]+)*\\s*$",r))))
}
isTypeP <- function(r, type)
{
  (!identical(character(0),regmatches(r,regexpr(type,r))))
}

P <- function(s)
{
  
  
  aux <- substitute(s)
  r <- deparse(aux)
 
  if(isSimpleExpression(r))
  {
    
    fct <- getFunction(r)
    value <- getNumbers(r)[[1]]
    
    #verificam daca avem </<= sau >/>=
    if(getOperators(r)[[1]] == "<")
            return (cdf(fct, value))
    return (1 - cdf(fct, value))
  }
 
  if(isCompoundExpression(r))
  {
    
    fct <- getFunction(r)
    values <- getNumbers(r)
    value1 <- values[[1]]
    value2 <- values[[2]]
   
    #vrem sa consideram toate perechile de operatori posibile (care au sens)
    #de asemenea, vrem sa vedem daca avem o probabilitate cu && sau conditionata
  
       operators <- getOperators(r)
       result <- ifelse(isTypeP(r,"\\|") && all(operators == tuple(">","<")), ((cdf(fct,value2) - cdf(fct,value1))/cdf(fct,value2)),
                 ifelse(isTypeP(r,"\\|") && all(operators == tuple("<",">")), ((cdf(fct,value1) - cdf(fct,value2))/(1 - cdf(fct,value2))), 
                 ifelse(isTypeP(r,"&&") && all(operators == tuple(">","<")), cdf(fct,value2) - cdf(fct,value1),
                 ifelse(isTypeP(r,"&&") && all(operators == tuple("<",">")), cdf(fct,value1) - cdf(fct,value2),0))))
                 
       return (result)
         
  }
  #pentru cazul in care avem P(X = x) se returneaza 0
  return (0)
}

#Exemple

#Exemplul 1
P(f1 == 0.2)
P(f1 >= 0.523)
P(f1 <= 1.047)
P(f1 <= 0.785 | f1 > 0.523)
#Exemplul 2

f2 <- function(x)
{
   if(x >= 0 && x <= 1)
       return (1/beta(2,3)*x*(1-x)^2)
   return (0)
}

P(f2 < 0.5)
P(f2 > 0.33)
P(f2 <= 0.5 | f2 > 0.25)

#Exemplul 3
f3 <- function(x)
{
  if(x >= 0)
     return (exp(-x))
  return (0)
}
P(f3 > 1 && f3 < 3)

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
