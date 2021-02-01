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
  cdf=function(x) { return(cdf_gamma_distribution(x, shape=2, scale=1/2)) },
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
