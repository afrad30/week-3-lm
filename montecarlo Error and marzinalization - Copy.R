m=1e5
y=numeric(m)
y
phi=numeric(m)

for (i in 1:m) {
  phi[i]=rbeta(1,shape1=2, shape2=2) #2 head, 2 tail
  y[i]=rbinom(1,size = 10,prob = phi[i])
}
phi=rbeta(m,2,2)
y=rbinom(m,10,phi)
y
table(y)/m
