#' Bimodal Gumbel: Density Function
#'
#' @param x Domain.
#' @param mu First location parameter.
#' @param sigma Scale parameter.
#' @param delta Second location parameter.
#' @return Vector.
#' @examples
#' dbgumbel(x = 0, mu = -2, sigma = 1, delta = -1)
#' curve(dbgumbel(x, mu = -2, sigma = 1, delta = -1), xlim = c(-5, 10), ylim= c(0, .4))
#' integrate(dbgumbel, mu = -2, sigma = 1, delta = -1, lower = -5, upper = 0)
#' @export

dbgumbel <- function(x, mu, sigma, delta) {
  gama = .5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495146
  z_delta = 1 + (delta * sigma * pi)^2 / 6 + (delta * mu + delta * sigma * gama - 1)^2
  y = (z_delta^-1) * ( (1 - delta * x)^2 + 1 ) * (sigma^-1) * exp( -(x - mu) / sigma - exp( -(x - mu) / sigma ) )
  return(y)
}

dbgumbel <- Vectorize(dbgumbel, 'x')



