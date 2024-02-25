#install.packages("rjags")
library("coda")
library("rjags")


# 1.specify the model
# distribution 1
#parameters of distribution 1 denoted blow
# distribution 2

modelinstring= "model {
  for (i in 1:n)  {
    y[i] ~ dnorm(mu, 1.0/sig2)
                  }
  mu ~ dt(0.0, 1.0/1.0, 1)
  sig2=1
}"


#2. set up the model
set.seed(50)

y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
n=length(y)



#before JAGS run this model we have to tell what the data are and what the parameter are
#& provide initial values

#data that will pass to the JAGS call,it has to a list
data_jags=list(y=y, n=n)

params=c("mu") #only parameter is mu, which is yet to define we can call sig2 here, but it is defined earlier

#to gives jags initial values we need to write a function



init = function(){
  init = list("mu"=0.0) #instead of using 0, we can generate random number here
}

mod=jags.model(textConnection(modelinstring), data = data_jags, inits = init)


#3 Run the MCMC sampler

update(mod,500)

mod_sim=coda.samples(mod,params,n.iter = 5000)

plot(mod_sim)
summary(mod_sim)
