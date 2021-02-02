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
Lider: Bianca Stan

Cum ne-am împărțit sarcinile:

- Bianca a rezolvat cerințele 1, 5, 12.
- Mira a rezolvat cerințele 2, 7, 11.
- Alexandru a rezolvat cerințele 3 și 4, a realizat documentația și cât de curând trebuie să își găsească
motivația să facă și pachetul în sine...
* Mihai a rezolvat ...

# Cerința 1

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

# Cerința 7

# Cerința 11

# Cerința 12
