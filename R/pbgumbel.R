#' Bimodal Gumbel: Distribution Function
#'
#' @param q Quantile.
#' @param mu First location parameter.
#' @param sigma Scale parameter.
#' @param delta Second location parameter.
#' @param lower.tail 	Logical; if TRUE (default), probabilities are P(X <= x) otherwise, P(X > x).
#' @return Vector.
#' @examples
#' \donttest{
#' pbgumbel(0, mu = -2, sigma = 1, delta = -1)
#' integrate(dbgumbel, mu = -2, sigma = 1, delta = -1, lower = -Inf, upper = 0)
#' pbgumbel(0, mu = -2, sigma = 1, delta = -1, lower.tail = FALSE)
#' curve(pbgumbel(x, mu = -2, sigma = 1, delta = -1), xlim = c(-5, 10))
#' }
#' @export

pbgumbel <- function(q, mu, sigma, delta, lower.tail = TRUE) {

  dbgumbel <- function(x, mu, sigma, delta) {
    gama = .5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495146314472498070824809605040144865428362241739976449235362535003337429373377376739427925952582470949160087352039481656708532331517766115286211995015079847937450857057400299213547861466940296043254215190587755352673313992540129674205137541395491116851028079842348775872050384310939973613725530608893312676001724795378367592713515772261027349291394079843010341777177808815495706610750101619166334015227893
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
