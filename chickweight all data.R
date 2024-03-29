library(tidyverse)
library(purrr)
library("rjags")
library("coda")
library("datasets")

data("ChickWeight")
any(is.na(ChickWeight))
ckw = as_tibble(ChickWeight)
boxplot(Time ~ Chick, data=ckw)


diet1 = ckw[ckw$Diet==1,]
diet2 = ckw[ckw$Diet==2,]
diet3 = ckw[ckw$Diet==3,]
diet4 = ckw[ckw$Diet==4,]

mean(diet1$weight)
mean(diet2$weight)
mean(diet3$weight)
mean(diet4$weight)



mod_string_all = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(mu[Diet[i]], prec)
    }
    
    for (j in 1:4){
            mu[j] ~ dnorm(0.0, 1.0/1.0e6)
            }
    
    prec ~ dgamma(3/2.0, 3*1.0/2.0)
    sig = sqrt(1.0 / prec)
} "

data_jags_all = list(y=ckw$weight, Diet=ckw$Diet)
params_all = c("mu", "sig")


mod_all = jags.model(textConnection(mod_string_all), data=data_jags_all, n.chains=3)
update(mod_all, 1e3)


mod_sim_all = coda.samples(model=mod_all,
                        variable.names=params_all,
                        n.iter=5e3)
summary(mod_sim_all)



mod_csim_all = as.mcmc(do.call(rbind, mod_sim_all))

summary(mod_csim_all)


## convergence diagnostics
gelman.diag(mod_sim_all)
autocorr.diag(mod_sim_all)
effectiveSize(mod_sim_all)
raftery.diag(mod_sim_all)

dic1 = dic.samples(mod_all, n.iter=1e3)
dic1








