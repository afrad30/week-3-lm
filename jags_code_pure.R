library("rjags")
library("coda")

y=c(1.2, 1.4,-0.5,0.3,0.9,2.3,1.0,0.1,1.5,1.9)
n=length(y)
data_jags=list(y=y, n=n)

init=function(){
  init=list("mu"=0)
}

model_string= "model{
                     for(i in 1:n){
                y[i] ~ dnorm(mu, 1.0/sig2)}
                mu ~ dt(0,1/1,1) # inverse scale=1/1
                sig2= 1.0
                    }"

set.seed(123)
mod=jags.model(textConnection(model_string), data=data_jags, inits=init)
update(mod, 500)

param=c("mu")
mod_sim=coda.samples(model=mod, variable.names = param, n.iter = 1000)

plot(mod_sim)
summary(mod_sim)
