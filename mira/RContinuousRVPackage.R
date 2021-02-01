#PUNCTUL 2
punctul_2 <- function(f)
{
  
  check = TRUE
  #f este densitate de probabilitate <=> f(x) >= 0, oricare x si integrala de la -inf la inf din f(x) este 1
  integralValue <- integrate(Vectorize(f), lower = - Inf, upper = Inf)$value
  
  
  if(abs(integralValue - 1) > 10^(-4))
  { 
    check = FALSE
  }  
  #luam o plaja larga de valori in care sa analizam functia
  t1 <- seq(-10^(6),10^6,0.01)
  
  x <- f(t1)
  if(!all(x >= 0))
  {
    check = FALSE
  }
  return (check)
}

#Exemplu
f1 <- function(x)
{
 
  if(x>=0 && x<=pi/2) return(cos(x))
  else return(0)
}

punctul_2(f1)

#PUNCTUL 7
cdf <- function(f, a)
{
  return (integrate(Vectorize(f), lower = -Inf, upper = a)$value)
}


P <- function(s)
{
  
  
  aux <- substitute(s)
  r <- deparse(aux)
  
  # P (X < n) si P (X <= n)
  if(!identical(character(0), regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*\\s*(<|<=)\\s*[0-9]+[.][0-9]+\\s*$",r))))
  {
    
    fct <- get(regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*", r)))
    
    value <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+", r)))
    print(value)
    return (cdf(fct, value))
  }
  # P (X > n) si P ( X >= n)
  if(!identical(character(0), regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*\\s*(>|>=)\\s*[0-9]+[.][0-9]+\\s*$",r))))
  {
    
    fct <- get(regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*", r)))
    
    value <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+",r)))
    print(value)
    return (1 - cdf(fct, value))
  }
  # P (X < (sau <=) n && X > (sau >=) m)
  if(!identical(character(0), regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*\\s*(<|<=)\\s*[0-9]+[.][0-9]+\\s+&&\\s+[A-Za-z]+[0-9]*\\s*(>|>=)\\s*[0-9]+[.][0-9]+\\s*$",r))))
  {
    
    fct <- get(regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*", r)))
    
    value1 <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+",r)))
    value2 <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+\\s*$",r)))
    
    return (cdf(fct,value1) - cdf(fct,value2))
    
  }
  # P (X > (sau >=) n && X < (sau <=) n)
  if(!identical(character(0), regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*\\s*(>|>=)\\s*[0-9]+[.][0-9]+\\s+&&\\s+[A-Za-z]+[0-9]*\\s*(<|<=)\\s*[0-9]+[.][0-9]+\\s*$",r))))
  {
    
    fct <- get(regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*", r)))
    
    value1 <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+",r)))
    value2 <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+\\s*$",r)))
    
    return (cdf(fct,value2) - cdf(fct,value1))
    
  }
  ### P (X < (sau <=) n | X > (sau >=) m)
  if(!identical(character(0), regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*\\s*(<|<=)\\s+[0-9]+[.][0-9]+\\s+|\\s+[A-Za-z]+[0-9]*\\s*(>|>=)\\s*[0-9]+[.][0-9]+\\s*$",r))))
  {
    
    fct <- get(regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*", r)))
    
    value1 <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+",r)))
    value2 <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+\\s*$",r)))
    return (cdf(fct,value1) - cdf(fct,value2))/cdf(fct,value2)
    
  }
  ### P (X > (sau >=) n | X < (sau <=) m)
  if(!identical(character(0), regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*\\s*(>|>=)\\s*[0-9]+[.][0-9]+\\s+|\\s+[A-Za-z]+[0-9]*\\s*(<|<=)\\s*[0-9]+[.][0-9]+\\s*$",r))))
  {
    
    fct <- get(regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*", r)))
    
    value1 <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+",r)))
    value2 <- as.numeric(regmatches(r,regexpr("[0-9]+[.][0-9]+\\s*$",r)))
    return (cdf(fct,value2) - cdf(fct,value1))/cdf(fct,value2)
    
  }
  #pentru cazul in care avem P(X = x) se returneaza 0
  return (0)
}

#Exemple
P(f1 >= 0.1 | f1 <0.2)
P(f1 > 0.4)
P(f1 <= 0.8)
P(f1 >= 0.7 && f1 < 0.9)
P(f1 <= 0.6 && f1 > 0.2)

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




