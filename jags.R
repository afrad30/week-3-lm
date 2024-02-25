#*****88specify the library

library("rjags")
library("coda")

#Specify the library

model_string= "model{

  for(i in 1:n){
    y[i] ~ dnorm(mu, 1.0/sig2)
               }
    mu ~ dt(0,1/1,1) # inverse scale=1/1
    sig2= 1.0
                    }"

#*******set up the model

y=c(1.2, 1.4,-0.5,0.3,0.9,2.3,1.0,0.1,1.5,1.9)
n=length(y)

#*******Before JAGS can run this model, 
#*******we need to tell it what the data are, 
#*******what the parameters are. 
#*******And provide initial values for those parameters.

data_jags=list(y=y, n=n)
param=c("mu")

#******There are actually multiple ways to specify initial values in JAGS.
 set.seed(123)
 
init=function(){
  init=list("mu"=0)
}

mod=jags.model(textConnection(model_string), data=data_jags, inits=init)
