set.seed(38)
n = 1500
x = numeric(n)
phi = -0.6
for (i in 2:n) {
  x[i] = rnorm(1, mean=phi*x[i-1], sd=1.0), #x[i-1] takes x[i] values, x[(i=3)-1]
}
plot.ts(x)