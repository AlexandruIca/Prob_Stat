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
