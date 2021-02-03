---
title: "Variabile Aleatoare Continue"
author:
    - "Bianca Stan"
    - "Mira Stroie"
    - "Alexandru Ică"
    - "Mihai Badea"
date: "Grupa 232, proiectul #1"
toc: false
output:
    pdf-document:
        template: template.tex
---
# Structura Echipei
Lider: Stan Bianca-Mihaela

Cum ne-am împărțit sarcinile:

- Bianca a rezolvat cerințele 1, 5, 12.
- Mira a rezolvat cerințele 2, 7, 11.
- Alexandru a rezolvat cerințele 3 și 4, a realizat documentația și pachetul în sine
- Mihai a rezolvat ...

# Cerința 1
Aleasă de Stan Bianca-Mihaela.\
\
Trebuie să găsim o constantă de normalizare `C` astfel încât:

1. $f(x)>=0 \ \forall x$
2. $\int_{-\infty}^{\infty} f(x) dx = 1$

Astfel, functia scrisa de mine calculeaza valoarea lui C=$\frac{1}{\int f(x) dx}$ (din proprietatea 2).
Daca nu s-a impartit la 0, verifica proprietatea 1. Pentru a verifica daca o functie are valori pozitive pe
tot codomeniul sau am creat o functie care ia un range mare de valori pentru a analiza comportamentul
functiei.
```R
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
Daca constanta de normalizare duce la verificarea acestei proprietati, aceasta este corecta.

Am ales exemple pentru fiecare caz:

- 2 functii pentru care exista constante de normalizare:
    * f(x)=$4x-2x^2$ pentru x in intervalul (0,2) si 0 in rest
    * f(x)=$x^3 e^{-x/2}$ pentru x >=0 si 0 in rest
- o functie divergenta care nu va verifica conditia 2: functia identitate
- o functie convergenta dar care are si valori pozitive si negative, astfel incat oricare ar fi valoarea
lui C, functia nu poate verifica conditia 1: functia identitate pe intervalul [-2,1] si 0 in rest

# Cerința 2
Aleasă de Stroie Mira. Pentru rezolvarea cerinței am implementat funcția `isPDF`. `isPDF` verifică dacă
o funcție introdusă de utilizator este densitate de probabilitate. Funcția se apelează `isPDF(f)`, unde `f`
reprezintă funcția dată.

Rezolvarea cerinței se bazează pe următorul fapt teoretic: o funcție f este densitate de probabilitate dacă
îndeplinește următoarele condiții:

$$f(x) >= 0$$
$$\int_{-\infty}^{\infty} f(x) dx = 1$$

Pentru verificarea primei condiții am generat prin intermediul funcției `seq` o secventă largă de valori.
```R
 t1 <- seq(-10^(5),10^5,0.05)
```
Am calculat valoarea funcției `f` în toate valorile generate, iar apoi am verificat ca toate rezultatele să
fie pozitive. Pentru a doua condiție am folosit funcția `integrate`, cu precizarea că, pentru a evita
eroriile cu numere reale, am comparat
$$abs(integralValue - 1) > 10^{-4}$$

În sursă am dat ca exemplu o funcție densitate de probabilitate discutată în problemele din laborator și
o funcție negativă, a cărei integrală e diferită de 1.

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

Am folosit formulele cunoscute pentru medie, dispersie, momentul centrat de ordin r si momentul initial
de ordin r:

- media: $\int_{-\infty}^{\infty} x f(x) dx$
- dispersia: $\int_{-\infty}^{\infty} (x-E(x))^2 f(x) dx$
- momentul centrat de ordin r: $\int_{-\infty}^{\infty} (x-E(x))^r f(x) dx$
- momentul initial de ordin r: $\int_{-\infty}^{\infty} x^r f(x) dx$

# Cerința 6
Aleasă de Badea Mihai.

Am refolosit formulele de la punctul anterior, aplicându-le pe rezultatul compunerii a două funcții, în speță funcția `g` - funcție precizată de utilizator și funcția `f` - funcție densitate de probabilitate aferentă lui `X`.

# Cerința 7

Aleasă de Mira Stroie. Pentru rezolvarea cerinței am implementat funcția `P`, asemănătoare funcției `P` din
pachetul `discreteRV`. `P` permite calcularea diferitelor tipuri de probabilități asociate unei variabile
aleatoare continue. Funcția se apelează `P(eveniment)` unde eveniment reprezintă o expresie logică.
Expresia poate fi de forma  `pdf operator valoare`, `pdf operator1 valoare1 && pdf operator2 valoare2`,
`pdf operator1 valoare1 | pdf operator2 valoare2` unde `pdf` reprezintă densitatea de probabilitate a unei
variabile aleatoare continue, iar operatorii pot să fie `<`,`<=`,`>`,`>=` și `==` (doar pentru prima formă).
Probabilități precum `P(pdf > valoare1 && pdf > valoare2)` și `P( pdf > valoare1 | pdf > valoare2)` nu au
fost luate în considerare, întrucât am considerat că implementarea lor este redundantă - ele sunt asociate
unor cazuri deja implementate.

Pentru găsirea probabilității asociate unei variabile aleatoare continue ne-am folosit de funcția de
distribuție cumulativă, stiind că $$P(X <= x) = F(x)$$ , unde `X` reprezintă o variabilă aleatoare continuă
și `F` funcția de distribuție cumulativă (notată în documentație și `cdf`).

De-a lungul implementării ne-am folosit de expresiile regulate pentru a identifica tipul de probabilitate
pe care trebuie să o calculam. Pentru un string care conține o expresie verificam prima dată dacă expresia
este una simplă (`pmf operator valoare`) (cu ajutorul funcției `isSimpleExpression`) sau compusă
(condiționată sau cu &&) (cu ajutorul funcției `isCompoundExpression`). Valorile numerice din expresii le
preluăm cu ajutorul funcției `getNumbers`, iar operatorii continuți cu `getOperators`. În funcției de
operatori și tipul probabilitătii (simplă sau compusă, condiționată sau &&), computăm probabilitatea cu
ajutorul `cdf`. În cazul în care am avut o probabilitate de tipul `pmf == valoare`, atunci se returnează 0.

Două funcții folosite preponderent sunt `regmatches` și `regexpr`. `regexpr` e folosită pentru a identifica
unde există un pattern într-un string și pentru a întoarce informație necesară pentru a extrage patternul.
În cazul în care patternul nu se găsește, se returnează -1. `regmatches` extrage patternul din string pe baza
informației primite de la `regexpr`. În proiect, un exemplu care să ilustreze aceste funcționalități
este următorul:
```R
getFunction <- function(r)
{
  get(regmatches(r, regexpr("^\\s*[A-Za-z]+[0-9]*", r)))
}
```
Am folosit funcția `str_extract_all` din pachetul `stringr` pentru a ușura lucrul cu expresii regulate.
`str_extract_all` extrage toate substringurile care respectă un pattern dat dintr-un string. În secvența
următoare de cod se preiau toate substringurile care conțin un operator urmat de oricâte spații și de un
număr (fie el întreg sau nu).
```R
getNumbers <- function(r)
{
   t1 <- str_extract_all(r,"(>|>=|<|<=)\\s*[0-9]+([.][0-9]+)*")
   #cod pentru getNumbers
}
```

Un alt pachet folosit este `sets`, ce conține structuri de date și funcții de bază pentru mulțimi. De aici am preluat funcția `tuple`, folosită pentru crearea și manipularea unui tuplu. Am folosit tuplurile pentru a returna rezultatele funcțiilor `getNumbers` și `getOperators`.

Am dat în codul sursă atât exemple noi, cât și exemple de probabilități calculate în cadrul laboratorului.

# Cerința 11
Aleasă de Stroie Mira. Pentru rezolvarea cerinței am implementat 4 funcții: `marginalX`, `marginalY` și `conditionalX`,
`conditionalY`. Primele 2 funcții au 2 parametrii: o valoare în care să fie evaluate și densitatea
comună a variabilelor aleatoare X si Y. Ultimele 2 funcții primesc ca parametrii densitatea comună
a variabilelor aleatoare X, Y și 2 valori x și y în care funcțiile să fie evaluate. Dat fiind o densitate
de probabilitate `fxy`, am calculat densitățiile marginale folosind următoarele formule:
$$fy(y)=\int_{-infty}^{\infty} fxy(x) dx$$

$$fx(x)=\int_{-infty}^{\infty} fxy(x) dy$$

unde fy(y) reprezintă densitatea marginală a v.a. `Y` si $fx(x)$, densitatea marginală a v.a. X.
Densitățiile condiționate le-am calculat după următoarele formule:
$$fx|y(x|y) = \frac{fxy(x,y)}{fy(y)}$$

$$fy|x(y|x) = \frac{fxy(x,y)}{fx(x)}$$

unde $fx|y$ reprezintă densitatea condiționată a variabilei aleatoare continue X, iar $fy|x$, densitatea
condiționată a lui Y.

Sursele care m-au ajutat la rezolvarea cerinței sunt:
[probability course](https://www.probabilitycourse.com/chapter5/5_2_3_conditioning_independence.php),
[probability course](https://www.probabilitycourse.com/chapter5/5_2_1_joint_pdf.php),
[stackoverflow](https://stackoverflow.com/questions/38123279/integrating-a-function-with-multiple-variables).

# Cerința 12
Aleasă de Stan Bianca-Mihaela.\
\
Știm că dacă 2 variabile aleatoare continue, X si Y, sunt independente, atunci:
$$f_U(u)=\int_{-\infty}^{\infty} f_X(v) f_Y(u-v) dv \textrm{\ \ unde $u=x+y$, $U=X+Y$ și $v=x$ și}$$
$$f_U(u)=\int_{-\infty}^{\infty} f_X(v) f_Y(v-u) dv \textrm{\ \ unde $u=x-y$, $U=X-Y$ și $v=x$}$$

Folosind aceste formule am construit histograme pentru distribuția normală, uniformă și exponențială.
