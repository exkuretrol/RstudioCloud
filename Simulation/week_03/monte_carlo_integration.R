monte_carlo_integration <- function(n, a, b) {
  # number of simulatation.
  num_sims <- n
  # where u is uniformly distributed between zero and one
  u <- runif(num_sims, min = 0, max = 1)
  # Xi
  x <- a+(b-a)*u
  # multiplied by (b-a) to get an approximation of the integral from a to b
  area <- (b-a)*mean(y(x))
  # area under a curve
  return(area)
}

y <- function(x) {
  # my function
  my_function <- sqrt(x*exp(x))
  return(my_function)
}

monte_carlo_integration(10000, 2, 5)
