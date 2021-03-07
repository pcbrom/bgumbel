#' Bimodal Gumbel: Distribution Function
#'
#' @param q Quantile.
#' @param mu First location parameter.
#' @param sigma Scale parameter.
#' @param delta Second location parameter.
#' @param lower.tail 	Logical; if TRUE (default), probabilities are P(X <= x) otherwise, P(X > x).
#' @return Vector.
#' @examples
#' pbgumbel(0, mu = -2, sigma = 1, delta = -1)
#' integrate(dbgumbel, mu = -2, sigma = 1, delta = -1, lower = -Inf, upper = 0)
#' pbgumbel(0, mu = -2, sigma = 1, delta = -1, lower.tail = FALSE)
#' curve(pbgumbel(x, mu = -2, sigma = 1, delta = -1), xlim = c(-5, 10))
#' @export

pbgumbel <- function(q, mu, sigma, delta, lower.tail = TRUE) {

  dbgumbel <- function(x, mu, sigma, delta) {
    gama = .5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495146
    z_delta = 1 + (delta * sigma * pi)^2 / 6 + (delta * mu + delta * sigma * gama - 1)^2
    y = (z_delta^-1) * ( (1 - delta * x)^2 + 1 ) * (sigma^-1) * exp( -(x - mu) / sigma - exp( -(x - mu) / sigma ) )
    return(y)
  }

  y <- integrate(
    f = dbgumbel,
    mu = mu,
    sigma = sigma,
    delta = delta,
    lower = -Inf,
    upper = q
  )$value

  if (lower.tail) {
    if (q > 50 & y < .01) {
      return('Error! Numerical instability. Try a smaller value for q.')
    } else {
      return(y)
    }
  } else {
    if (q > 50 & y < .01) {
      return('Error! Numerical instability. Try a smaller value for q.')
    } else {
      return(1 - y)
    }
  }

}

pbgumbel <- Vectorize(pbgumbel, 'q')
