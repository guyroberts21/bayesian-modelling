compound_dist <- function(N, alpha) {
  numerator <- stats::beta(N+1, alpha+1)
  denominator <- stats::beta(alpha, 1) * factorial(N + alpha)
  return(numerator/denominator)
}

compound_dist(2, 1)

# manual test
(factorial(N + alpha) / (factorial(N)*factorial(alpha))) * (N+1)/(alpha+N+1)
