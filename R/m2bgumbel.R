#' Bimodal Gumbel: Theoretical E(X^2)
#'
#' @param mu First location parameter.
#' @param sigma Scale parameter.
#' @param delta Second location parameter.
#' @return Vector.
#' @examples
#' (EX2 <- m2bgumbel(mu = -2, sigma = 1, delta = -1))
#'
#' \donttest{
#' # Comparison: Theoretical E(X^2) and empirical second moment
#'
#' x <- rbgumbel(100000, mu = -2, sigma = 1, delta = -1)
#' mean(x^2)
#' abs(EX2 - mean(x))/abs(EX2) # relative error
#'
#' # Variance
#' EX <- m1bgumbel(mu = -2, sigma = 1, delta = -1)
#' EX2 - EX^2
#' var(x)
#' abs(EX2 - EX^2 - var(x))/abs(EX2 - EX^2) # relative error
#'
#' # grid 1
#'
#' mu <- seq(-5, 5, length.out = 100)
#' delta <- seq(-5, 5, length.out = 100)
#' z <- outer(
#'   X <- mu,
#'   Y <- delta,
#'   FUN = function(x, y) m2bgumbel(mu = x, sigma = 1, delta = y)
#' )
#' persp(x = mu, y = delta, z = z, theta = -30, ticktype = 'detailed')
#'
#' # grid 2
#'
#' mu <- seq(-5, 5, length.out = 100)
#' delta <- seq(-5, 5, length.out = 100)
#' sigmas <- seq(.1, 10, length.out = 20)
#' for (sigma in sigmas) {
#'   z <- outer(
#'     X <- mu,
#'     Y <- delta,
#'     FUN = function(x, y) m2bgumbel(mu = x, sigma = sigma, delta = y)
#'   )
#'   persp(x = mu, y = delta, z = z, theta = -45, zlab = 'E(X^2)')
#'   Sys.sleep(.5)
#' }
#' }
#' @export

m2bgumbel <- function(mu, sigma, delta) {

  gama = .5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495146

  z_delta = 1 + (delta * sigma * pi)^2 / 6 +
    (delta * mu + delta * sigma * gama - 1)^2
  zeta3 = 1.20205690315959428539973816151144999076498629234049888179227155534183820578631309018645587360933525814

  y = (
    delta^2 * sigma^4 * (8 * gama * zeta3 + gama^4 + gama^2 * pi^2 + 3 * pi^4 / 20)
    - 2 * delta * sigma^3 * (1 - 2 * delta * mu) * (2 * zeta3 + gama^3 + gama * pi^2 / 2)
    + mu^2 * (2 - delta * mu * (2 - delta * mu))
    + 2 * sigma * mu * (2 - delta * mu * (3 - 2 * delta * mu)) * gama + 2 * sigma^2 * (1 - 3 * delta * mu * (1 - delta * mu)) * (gama^2 + pi^2 / 6)
  ) / z_delta

  return(y)

}

m2bgumbel <- Vectorize(m2bgumbel, c('mu', 'sigma', 'delta'))
