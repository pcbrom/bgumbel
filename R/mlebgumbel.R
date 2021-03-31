#' Bimodal Gumbel: Maximum Likelihood Estimation
#'
#' @param data A numeric vector.
#' @param theta Vector. Starting parameter values for the minimization. Default: theta = c(1, 1, 1)
#' @param auto Logical. Automatic search for theta initial condition. Default: TRUE
#' @return List.
#' @import MASS
#' @importFrom stats nlm
#' @examples
#' # Let's generate some values
#'
#' set.seed(123)
#' x <- rbgumbel(1000, mu = -2, sigma = 1, delta = -1)
#'
#' # Look for these references in the figure:
#'
#' hist(x, probability = TRUE)
#' lines(density(x), col = 'blue')
#' abline(v = c(-2.5, -.5), col = 'red')
#' text(x = c(c(-2.5, -.5)), y = c(.05, .05), c('mu\nnear here', 'delta\nnear here'))
#'
#' # Time to fit!
#'
#' # If argument auto = FALSE
#' fit <- mlebgumbel(
#'    data = x,
#'    # try some values near the region. Format: theta = c(mu, sigma, delta)
#'    theta = c(-3, 2, -2),
#'    auto = FALSE
#' )
#' print(fit)
#'
#' # If argument auto = TRUE
#' fit <- mlebgumbel(
#'    data = x,
#'    auto = TRUE
#' )
#' print(fit)
#'
#' \donttest{
#' # Kolmogorov-Smirnov Tests
#'
#' mu.sigma.delta <- fit$estimate$estimate
#' ks.test(
#'   x,
#'   y = 'pbgumbel',
#'   mu = mu.sigma.delta[[1]],
#'   sigma = mu.sigma.delta[[2]],
#'   delta = mu.sigma.delta[[3]]
#' )
#' }
#' @export

mlebgumbel <- function(data, theta, auto = TRUE) {

  if (auto) {
    moments <- function(X) {
      X[2] <- ifelse(X[2] < 0, -Inf, X[2])
      y <- (m2bgumbel(X[1], X[2], X[3]) - mean(data^2))^2
      z <- (m1bgumbel(X[1], X[2], X[3]) - mean(data))^2
      w <- y + z
      return(w)
    }
    theta <- suppressWarnings({nlm(f = moments, p = c(1,1,1))$estimate})
  }

  data <- data[!is.na(data)]

  if (length(data) < 1) {
    return('Verify data vector. length(data) < 1.')
  }

  if (!is.vector(theta)) {
    return('Starting param eter values must be a vector. Change theta.')
  }

  dbgumbel <- function(x, mu, sigma, delta) {
    gama = .5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495146
    z_delta = 1 + (delta * sigma * pi)^2 / 6 + (delta * mu + delta * sigma * gama - 1)^2
    y = (z_delta^-1) * ( (1 - delta * x)^2 + 1 ) * (sigma^-1) * exp( -(x - mu) / sigma - exp( -(x - mu) / sigma ) )
    return(y)
  }
  dbgumbel <- Vectorize(dbgumbel, 'x')

  suppressWarnings({fit <- MASS::fitdistr(x = data, densfun = dbgumbel, start = list('mu' = theta[1], 'sigma' = theta[2], 'delta' = theta[3]))})

  y <- list(
    estimate = fit,
    vcov = fit$vcov,
    loglik = fit$loglik
  )

  return(y)

}
