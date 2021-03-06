#' Bimodal Gumbel: Quantile Function
#'
#' @param p Probability.
#' @param mu First location parameter.
#' @param sigma Scale parameter.
#' @param delta Second location parameter.
#' @param initial Starting point of range in desired quantile.
#' @param final Starting point of range in desired quantile.
#' @return Vector.
#' @examples
#' # It is recommended to set up a pbgumbel
#' # graph to see the starting and ending
#' # range of the desired quantile.
#' curve(pbgumbel(x, mu = -2, sigma = 1, delta = -1), xlim = c(-5, 5))
#' (value <- qbgumbel(.25, mu = -2, sigma = 1, delta = -1, initial = -4, final = -2))
#' pbgumbel(value, mu = -2, sigma = 1, delta = -1)
#' @export

qbgumbel <- function(p, mu, sigma, delta, initial = -10, final = 10) {

  pbgumbel <- function(q, mu, sigma, delta) {

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

    if (q > 50 & y < .01) {
      return('Error! Numerical instability. Try a smaller value for q.')
    } else {
      return(y)
    }

  }

  pbgumbel <- Vectorize(pbgumbel, 'q')

  cte <- p
  fx <- function(p) pbgumbel(p, mu = mu, sigma = sigma, delta = delta)

  xx <- seq(initial, final, length.out = 100)
  y <- (fx(xx) - cte)^2
  tmp <- spline(x = xx, y = y, n = 100)

  xx <- tmp$x[tmp$y < .005]
  xx <- sort(runif(100, min = min(xx), max = max(xx)))

  y <- (fx(xx) - cte)^2
  tmp <- spline(x = xx, y = y, n = 1000)
  yy <- tmp$x[tmp$y == min(tmp$y)]

  return(yy)

}

qbgumbel <- Vectorize(qbgumbel, 'p')
