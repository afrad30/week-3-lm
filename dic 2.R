library("rjags")
library("car")
data("Anscombe")
mod4_string = " model {
    
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + 
                b[1]*income[i] + 
                b[2]*young[i]
                                    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data4_jags <- as.list(Anscombe)

# data1_jags <- list(
#   y = data_jags$education,
#   income = data_jags$income,
#   young = data_jags$young,
#   urban = data_jags$urban,
#   n = nrow(Anscombe)
# )

set.seed(72)

params4= c("b", "sig")

inits4=function(){
  inits=list("b"=rnorm(2, 0, 100), prec=rgamma(1,1.0,1.0))
}

mod4=jags.model(textConnection(mod4_string), data=data4_jags, inits = inits4, 
                n.chains = 3)

update(mod4, 1000)


mod4_sim = coda.samples(model = mod4, variable.names = params4, n.iter = 5e4)

mod4_csim = as.mcmc(do.call(rbind, mod4_sim))

#convergens

#plot(mod1_sim)

gelman.diag(mod1_sim)# understood, variance between chain and within chain

gelman.plot(mod1_sim)

autocorr.diag(mod1_sim)

effectiveSize(mod1_sim)

summary(mod1_sim)


# residuals from frequentist
lmod= lm(education ~ income+young+urban, data=Anscombe)
plot(lmod)

plot(resid(lmod))
plot(predict(lmod), resid(lmod))

qqnorm(resid(lmod))

#residuals from mcmc


dic.samples(mod4, n.iter = 100000)






