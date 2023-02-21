compound_dist = function(N, alpha) {
  return(beta(alpha+1, N+1) / beta(alpha, 1))
}

compound_dist(2, 1)
