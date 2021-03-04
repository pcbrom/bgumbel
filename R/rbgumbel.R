#' Bimodal Gumbel: Pseudo-Random Numbers Generator
#'
#'
#' @param n Number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param mu First location parameter.
#' @param sigma Scale parameter.
#' @param delta Second location parameter.
#' @return A matrix nx1.
#' @import MCMCpack
#' @import quantreg
#' @import SparseM
#' @import coda
#' @examples
#' x <- rbgumbel(40000, mu = -2, sigma = 1, delta = -1)
#' hist(x, probability = TRUE)
#' curve(dbgumbel(x, mu = -2, sigma = 1, delta = -1), add = TRUE, col = 'blue')
#' lines(density(x), col = 'red')
#' @export

rbgumbel <- function(n, mu, sigma, delta) {

  suppressMessages(library(MCMCpack))

  mu <- mu
  sigma <- sigma
  delta <- delta

  fx <- function(x, ...) {
    gama = .5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495146314472498070824809605040144865428362241739976449235362535003337429373377376739427925952582470949160087352039481656708532331517766115286211995015079847937450857057400299213547861466940296043254215190587755352673313992540129674205137541395491116851028079842348775872050384310939973613725530608893312676001724795378367592713515772261027349291394079843010341777177808815495706610750101619166334015227893
    z_delta = 1 + (delta * sigma * pi)^2 / 6 +
      (delta * mu + delta * sigma * gama - 1)^2
    y <- -log(z_delta) +
      log((1 - delta * x)^2 + 1) -
      exp((mu - x) / sigma) +
      (mu - x) / sigma - log(sigma)
    return(y)
  }

  fx <- Vectorize(fx, 'x')

  samples <- MCMCmetrop1R(
    fun = fx,
    theta.init = abs(sigma),
    V = as.matrix(1),
    mcmc = n
  )
  samples <- as.vector(samples)

  return(samples)

}

rbgumbel <- Vectorize(rbgumbel, 'n')
