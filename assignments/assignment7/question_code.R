# Q1

# Understanding the pdf...
vals = seq(1, 100, 0.1)
n = length(vals)
out = rep(0, n)
for (i in 1:n) {
  x = vals[i]
  out[i] = x^(-2)
}
plot(vals, out, type='l') # create line plot of the distribution

# Stan model...