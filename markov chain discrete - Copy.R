Q = matrix(c(0.0, 0.3, 0.3,
             0.7, 0.0, 0.5,
             0.7, 0.5, 0.0),
           nrow=3, byrow=TRUE)
n = 4
x = numeric(n)
x[1] = 1 # fix the state as 1 for time 1
for (i in 2:4) {
  x[i]=Q[x[i-1],]
}