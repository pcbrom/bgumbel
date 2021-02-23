#' Bimodal Gumbel: Theoretical E(X^2)
#'
#' @param mu First location parameter.
#' @param sigma Scale parameter.
#' @param delta Second location parameter.
#' @examples
#' # Comparison: Theoretical E(X^2) and empirical second moment
#' (EX2 <- m2bgumbel(mu = -2, sigma = 1, delta = -1))
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
#' mu <- seq(-5, 5, length.out = 100)
#' delta <- seq(-5, 5, length.out = 100)
#' z <- outer(X <- mu, Y <- delta, FUN = function(x, y) m2bgumbel(mu = x, sigma = 1, delta = y))
#' persp(x = mu, y = delta, z = z, theta = -30, ticktype = 'detailed')
#'
#' # grid 2
#' mu <- seq(-5, 5, length.out = 100)
#' delta <- seq(-5, 5, length.out = 100)
#' sigmas <- seq(.1, 10, length.out = 20)
#' for (sigma in sigmas) {
#'   z <- outer(X <- mu, Y <- delta, FUN = function(x, y) m2bgumbel(mu = x, sigma = sigma, delta = y))
#'   persp(x = mu, y = delta, z = z, theta = -45, zlab = 'E(X^2)')
#'   Sys.sleep(.5)
#' }
#' @export

m2bgumbel <- function(mu, sigma, delta) {

  gama = .5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495146314472498070824809605040144865428362241739976449235362535003337429373377376739427925952582470949160087352039481656708532331517766115286211995015079847937450857057400299213547861466940296043254215190587755352673313992540129674205137541395491116851028079842348775872050384310939973613725530608893312676001724795378367592713515772261027349291394079843010341777177808815495706610750101619166334015227893

  z_delta = 1 + (delta * sigma * pi)^2 / 6 +
    (delta * mu + delta * sigma * gama - 1)^2
  zeta3 = 1.2020569031595942853997381615114499907649862923404988817922715553418382057863130901864558736093352581461991577952607194184919959986732832137763968372079001614539417829493600667191915755222424942439615639096641032911590957809655146512799184051057152559880154371097811020398275325667876035223369849416618110570147157786394997375237852779370309560257018531827900030765471075630488433208697115737423807934450316076253177145354444118311781822497185263570918244899879620350833575617202260339378587032813126780799005417734869

  y = (
    # bloco 1
    delta^2 * sigma^4 * (8 * gama * zeta3 + gama^4 + gama^2 * pi^2 + 3 * pi^4 / 20)
    # bloco 2
    - 2 * delta * sigma^3 * (1 - 2 * delta * mu) * (2 * zeta3 + gama^3 + gama * pi^2 / 2)
    # bloco 3
    + mu^2 * (2 - delta * mu * (2 - delta * mu))
    # bloco 4
    + 2 * sigma * mu * (2 - delta * mu * (3 - 2 * delta * mu)) * gama + 2 * sigma^2 * (1 - 3 * delta * mu * (1 - delta * mu)) * (gama^2 + pi^2 / 6)
  ) / z_delta

  return(y)

}

m2bgumbel <- Vectorize(m2bgumbel, c('mu', 'sigma', 'delta'))

#' @examples
#' # Comparison: Theoretical E(X^2) and empirical second moment
#' (EX2 <- m2bgumbel(mu = -2, sigma = 1, delta = -1))
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
#' mu <- seq(-5, 5, length.out = 100)
#' delta <- seq(-5, 5, length.out = 100)
#' z <- outer(X <- mu, Y <- delta, FUN = function(x, y) m2bgumbel(mu = x, sigma = 1, delta = y))
#' persp(x = mu, y = delta, z = z, theta = -30, ticktype = 'detailed')
#'
#' # grid 2
#' mu <- seq(-5, 5, length.out = 100)
#' delta <- seq(-5, 5, length.out = 100)
#' sigmas <- seq(.1, 10, length.out = 20)
#' for (sigma in sigmas) {
#'   z <- outer(X <- mu, Y <- delta, FUN = function(x, y) m2bgumbel(mu = x, sigma = sigma, delta = y))
#'   persp(x = mu, y = delta, z = z, theta = -45, zlab = 'E(X^2)')
#'   Sys.sleep(.5)
#' }
