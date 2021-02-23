<!-- # bgumbel <img src="man/figures/logo.png" align="right" /> -->

<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/bgumbel?color=brightgreen)](http://www.r-pkg.org/pkg/bgumbel) -->

<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/mRpostman?color=brightgreen)](http://www.r-pkg.org/pkg/mRpostman) -->

<!-- one space after links to display badges side by side -->

[![Travis-CI Build
Status](https://travis-ci.org/pcbrom/bgumbel.svg?branch=master)](https://travis-ci.org/pcbrom/bgumbel)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/bgumbel)](https://cran.r-project.org/package=bgumbel)
[![Downloads from the RStudio CRAN
mirror](https://cranlogs.r-pkg.org/badges/grand-total/bgumbel)](https://cran.r-project.org/package=bgumbel)


# bgumbel

<!-- badges: start -->
<!-- badges: end -->

We propose a new model with three parameters called bimodal Gumbel (BG) as a generalization of the Gumbel distribution. The advantage of our model in comparison to other generalizations of the Gumbel distribution is the number of parameters and the fact that it can be used to model extreme data with one or two modes.


## Installation

You can install the released version of bgumbel from [CRAN](https://CRAN.R-project.org) with:


``` r
install.packages("bgumbel")
```


## Load package

``` r
library(bgumbel)
```


## Density Function


``` r
dbgumbel(x = 0, mu = -2, sigma = 1, delta = -1)
curve(dbgumbel(x, mu = -2, sigma = 1, delta = -1), xlim = c(-5, 10))
integrate(dbgumbel, mu = -2, sigma = 1, delta = -1, lower = -5, upper = 0)
```


## Distribution Function

``` r
pbgumbel(0, mu = -2, sigma = 1, delta = -1)
integrate(dbgumbel, mu = -2, sigma = 1, delta = -1, lower = -Inf, upper = 0)
pbgumbel(0, mu = -2, sigma = 1, delta = -1, lower.tail = FALSE)
curve(pbgumbel(x, mu = -2, sigma = 1, delta = -1), xlim = c(-5, 10))
```


## Quantile Function

It is recommended to set up a pbgumbel graph to see the starting and ending range of the desired quantile.

``` r
curve(pbgumbel(x, mu = -2, sigma = 1, delta = -1), xlim = c(-5, 5))
(value <- qbgumbel(.25, mu = -2, sigma = 1, delta = -1, initial = -4, final = -2))
pbgumbel(value, mu = -2, sigma = 1, delta = -1)
```


## Pseudo-Random Numbers Generator

``` r
x <- rbgumbel(100000, mu = -2, sigma = 1, delta = -1)
hist(x, probability = T)
curve(dbgumbel(x, mu = -2, sigma = 1, delta = -1), add = TRUE, col = 'blue')
lines(density(x), col = 'red')
```


## Theoretical E(X) and empirical first moment

``` r
(EX <- m1bgumbel(mu = -2, sigma = 1, delta = -1))
x <- rbgumbel(100000, mu = -2, sigma = 1, delta = -1)
mean(x)
abs(EX - mean(x))/abs(EX) # relative error
# grid 1
mu <- seq(-5, 5, length.out = 100)
delta <- seq(-5, 5, length.out = 100)
z <- outer(X <- mu, Y <- delta, FUN = function(x, y) m1bgumbel(mu = x, sigma = 1, delta = y))
persp(x = mu, y = delta, z = z, theta = -60, ticktype = 'detailed')
# grid 2
mu <- seq(-5, 5, length.out = 100)
delta <- seq(-5, 5, length.out = 100)
sigmas <- seq(.1, 10, length.out = 20)
for (sigma in sigmas) {
  z <- outer(X <- mu, Y <- delta, FUN = function(x, y) m1bgumbel(mu = x, sigma = sigma, delta = y))
  persp(x = mu, y = delta, z = z, theta = -60, zlab = 'E(X)')
  Sys.sleep(.5)
}
```


## Theoretical E(X^2) and empirical second moment

``` r
(EX2 <- m2bgumbel(mu = -2, sigma = 1, delta = -1))
x <- rbgumbel(100000, mu = -2, sigma = 1, delta = -1)
mean(x^2)
abs(EX2 - mean(x))/abs(EX2) # relative error
# Variance
EX <- m1bgumbel(mu = -2, sigma = 1, delta = -1)
EX2 - EX^2
var(x)
abs(EX2 - EX^2 - var(x))/abs(EX2 - EX^2) # relative error
# grid 1
mu <- seq(-5, 5, length.out = 100)
delta <- seq(-5, 5, length.out = 100)
z <- outer(X <- mu, Y <- delta, FUN = function(x, y) m2bgumbel(mu = x, sigma = 1, delta = y))
persp(x = mu, y = delta, z = z, theta = -30, ticktype = 'detailed')
# grid 2
mu <- seq(-5, 5, length.out = 100)
delta <- seq(-5, 5, length.out = 100)
sigmas <- seq(.1, 10, length.out = 20)
for (sigma in sigmas) {
  z <- outer(X <- mu, Y <- delta, FUN = function(x, y) m2bgumbel(mu = x, sigma = sigma, delta = y))
  persp(x = mu, y = delta, z = z, theta = -45, zlab = 'E(X^2)')
  Sys.sleep(.5)
}
```



## Issues

Please, send to: https://github.com/pcbrom/bgumbel/issues


## To cite package ‘bgumbel’ in publications use:

```
Pedro C. Brom, Cira E. G. Otiniano, Roberto Vila and Marcelo B. Pereira (2021). bgumbel: Bimodal Gumbel distribution. R package version 0.0.1.0.

A BibTeX entry for LaTeX users is

@Manual{,
  title = {bgumbel: Bimodal Gumbel distribution},
  author = {Pedro {C. Brom} and Cira {E. G. Otiniano} and Roberto Vila and Marcelo {B. Pereira}},
  year = {2021},
  note = {R package version 0.0.1.1},
}
```
