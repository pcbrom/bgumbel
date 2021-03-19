.onAttach <- function(libname, pkgname) {
  if (!capabilities('long.double')) {options(matprod = 'internal')}
  # packageStartupMessage({
  #   "Dependencies for rbgumbel function: MCMCpack, coda, MASS."
  # })
}
