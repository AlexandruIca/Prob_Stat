---
title: "Variabile Aleatoare Continue"
author:
    - "Bianca Stan"
    - "Mira Stroie"
    - "Alexandru Ică"
    - "Mihai Badea"
date: "Grupa 232, proiectul #1"
toc: true
output:
    pdf-document:
        template: template.tex
---
\newpage

# Structura Echipei
Lider: Stan Bianca-Mihaela

Cum ne-am împărțit sarcinile:

- Bianca a rezolvat cerințele 1, 5, 12.
- Mira a rezolvat cerințele 2, 7, 11.
- Alexandru a rezolvat cerințele 3 și 4, a realizat documentația și cât de curând trebuie să își găsească
motivația să facă și pachetul în sine...
* Mihai a rezolvat ...

# Cerința 1
Aleasă de Stan Bianca-Mihaela.\
\
Trebuie sa gasim o constanta de normalizare `C` astfel incat:
 1. f(x)>=0 pentru orice x
 2.  $$\int_-infty^\infty f(x) \, dx = 1$$\
\
Astfel, functia scrisa de mine calculeaza valoarea lui C=$$1/ \int f(x) \, dx $$ (din proprietatea 2). Daca nu s-a impartit la 0, verifica proprietatea 1. Pentru a verifica daca o functie are valori pozitive pe tot codomeniul sau am creat o functie care ia un range mare de valori pentru a analiza comportamentul functiei.
```
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
```
Daca constanta de normalizare duce la verificarea acestei proprietati, aceasta este corecta.\
\
Am ales exemple pentru fiecare caz:
- 2 functii pentru care exista constante de normalizare :
    - f(x)=$$4*x-2*x^2$$ pentru x in intervalul (0,2) si 0 in rest
    - f(x)=$$x^3*exp(-x/2)$$ pentru x >=0 si 0 in rest
- o functie divergenta care nu va verifica conditia 2 : functia identitate
- o functie convergenta dar care are si valori pozitive si negative, astfel incat oricare ar fi valoarea lui C, functia nu poate verifica conditia 1 : functia identitate pe intervalul [-2,1] si 0 in rest

# Cerința 2

# Cerința 3
Aleasă de Ică Alexandru.\
\
Am creat o clasă `continuous_rv` (tip S4) pentru a putea lucra ușor cu variabile aleatoare continue. Pentru
a crea efectiv o variabilă:
```R
my_var <- RV(from=1, to=10, density=function(x) { "funcția..." })
```
Unde `from` și `to` reprezintă intervalul pe care să se aplice densitatea.
Dacă `to` nu este dat explicit i se atribuie implicit valoarea $+\infty$.

Funcția densitate trebuie doar să aibă un parametru `x`. Pentru a putea reprezenta funcții cu mai mulți
parametri(de exemplu repartiția gamma) se va da o funcție pentru `density` care este un 'wrapper':
```R
RV(from=1, to=10, density=function(x) { exponential(x, lambda=2) })
```
Așa cum am făcut și în exemplele pe care le-am dat în cod.

În cazul în care nu se dă funcția densitate aceasta este automat:
$$ \frac{1}{to - from + 1} $$
Dacă se poate, adică dacă `to` și `from` nu sunt $\pm \infty$, altfel este `NULL`.

Pentru a evalua efectiv variabila am supraîncărcat metoda `show`, astfel trebuie doar specificat numele
variabilei în consolă și se vor afișa valorile ei pe interval(maxim 10):
```R
> RV(from=1, to=2)

Eveniment:       1   2
Probabilitate: 0.5 0.5
```

Am scris în comentarii sursele care m-au ajutat în rezolvarea problemei. Pentru exemple m-am folosit de
aceleași funcții pe care le-am definit și la cerința 4.

# Cerința 4
Aleasă de Ică Alexandru.\
\
Pentru a desena graficele densităților și funcțiilor de repartiție pentru orice caz, am ales să implementez
o funcție generică:
```R
probability_any_distribution <- function(
    f, dist_name, center, offset, step=0.01, cdf=NULL
  ) {
  # cod...
}
```
Unde `f` este funcția densitate, `dist_name` reprezintă numele funcției(pentru a putea da un nume relevant
graficelor), `center` și `offset` reprezintă intervalul pe care se vor desena graficele(de la
$center - offset$ până la $center + offset$), `step` reprezintă precizia cu care se vor desena graficele,
iar `cdf` reprezintă funcția distributivă cumulativă care poate fi omisă.

În cazul în care `cdf` este omisă atunci încerc să aproximez integrala pentru `f` folosindu-mă de
`integrate`, altfel se va desena graficul pentru funcția `cdf` dată.

Am implementat exemple pentru 3 funcții studiate(repartiția normală, exponențială și gamma). La fel ca la
punctul 3 funcția generică se așteaptă ca `f` să aibă doar un parametru `x`, pentru a putea implementa
funcțiile menționate anterior am atribuit lui `f` funcții anonime care 'împachetează' funcții cu oricâți
parametri. Tot ca la cerința 3 am pus în comentarii sursele care m-au ajutat la rezolvare.

Niște probleme întâmpinate în timpul rezolvării au fost erorile generate de funcția `plot` care nu au fost din
start evidente(de genul 'x and y lengths differ', deși era evident că integrez pe același interval)
și cele care au apărut când am vrut să integrez funcția dată pe un interval(mi-a luat mult până să îmi dau
seama că trebuia să scriu: `integrate(...)$value`).

# Cerința 5
Aleasă de Stan Bianca-Mihaela.\
\
Am folosit formulele cunoscute pentru medie, dispersie, momentul centrat de ordin r si momentul initial de ordin r:
- media: $$\int_-infty^\infty x*f(x) \, dx $$\
- dispersia: $$\int_-infty^\infty (x-E(x))^2*f(x) \, dx $$\
- momentul centrat de ordin r: $$\int_-infty^\infty (x-E(x))^r*f(x) \, dx $$\
- momentul initial de ordin r: $$\int_-infty^\infty x^r*f(x) \, dx $$\

# Cerința 7

# Cerința 11

# Cerința 12
Aleasă de Stan Bianca-Mihaela.\
\
Stim ca daca 2 variabile aleatoare continue, X si Y, sunt independente, atunci\
$$f_[U](u)=\int_-infty^\infty f_X(v)* f_Y(u-v) \, dv $$, unde u=x+y, U=X+Y si v=x\
si\
$$f_[U](u)=\int_-infty^\infty f_X(v)* f_Y(v-u) \, dv $$, unde u=x-y, U=X-Y si v=x\
\
Folosind aceste formule am construit histograme pentru distributia normala, uniforma si exponentiala.
