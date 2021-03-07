#' Bimodal Gumbel: Theoretical E(X)
#'
#' @param mu First location parameter.
#' @param sigma Scale parameter.
#' @param delta Second location parameter.
#' @return Vector.
#' @examples
#' (EX <- m1bgumbel(mu = -2, sigma = 1, delta = -1))
#'
#' \donttest{
#' # Comparison: Theoretical E(X) and empirical mean
#'
#' x <- rbgumbel(100000, mu = -2, sigma = 1, delta = -1)
#' mean(x)
#' abs(EX - mean(x))/abs(EX) # relative error
#'
#' # grid 1
#'
#' mu <- seq(-5, 5, length.out = 100)
#' delta <- seq(-5, 5, length.out = 100)
#' z <- outer(
#'   X <- mu,
#'   Y <- delta,
#'   FUN = function(x, y) m1bgumbel(mu = x, sigma = 1, delta = y)
#' )
#'
#' persp(x = mu, y = delta, z = z, theta = -60, ticktype = 'detailed')
#'
#' # grid 2
#'
#' mu <- seq(-5, 5, length.out = 100)
#' delta <- seq(-5, 5, length.out = 100)
#' sigmas <- seq(.1, 10, length.out = 20)
#'
#' for (sigma in sigmas) {
#'  z <- outer(
#'    X <- mu,
#'    Y <- delta,
#'     FUN = function(x, y) m1bgumbel(mu = x, sigma = sigma, delta = y)
#'  )
#'  persp(x = mu, y = delta, z = z, theta = -60, zlab = 'E(X)')
#'  Sys.sleep(.5)
#' }
#' }

#' @export

m1bgumbel <- function(mu, sigma, delta) {

  gama = .5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495146

  z_delta = 1 + (delta * sigma * pi)^2 / 6 +
    (delta * mu + delta * sigma * gama - 1)^2

  msg = mu + sigma * gama
  spi6 = (sigma^2 * pi^2)/6
  zeta3 = 1.20205690315959428539973816151144999076498629234049888179227155534183820578631309018645587360933525814

  y = (
    delta^2 * (
      2 * sigma^3 * zeta3 +
        3 * msg * spi6 +
        msg^3
    ) -
      2 * delta * (spi6 + msg^2) +
      2 * msg
  ) / z_delta

  return(y)

}

m1bgumbel <- Vectorize(m1bgumbel, c('mu', 'sigma', 'delta'))
