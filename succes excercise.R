library("rjags")

mod_string = " model {
    
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + 
                b[1]*income[i] + 
                b[2]*young[i] + 
                b[3]*urban[i]
                                    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags <- as.list(Anscombe)

# data1_jags <- list(
#   y = data_jags$education,
#   income = data_jags$income,
#   young = data_jags$young,
#   urban = data_jags$urban,
#   n = nrow(Anscombe)
# )

set.seed(72)

params1= c("b", "sig")

inits1=function(){
  inits=list("b"=rnorm(3, 0, 100), prec=rgamma(1,1.0,1.0))
}

mod1=jags.model(textConnection(mod_string), 
                data=data_jags, inits = inits1)

update(mod1, 1000)


mod1_sim = coda.samples(model = mod1, variable.names = params1, n.iter = 5e3)

mod1_csim = do.call(rbind, mod1_sim)

#convergens

plot(mod1_sim)

gelman.diag()
#gelman.diag(mod_sim)
autocorr.diag(mod_sim)

effectiveSize(mod_sim)

summary(mod_sim)


# residuals from frequentist
lmod= lm(education ~ income+young+urban, data=Anscombe)
plot(lmod)

plot(resid(lmod))
plot(predict(lmod), resid(lmod))

qqnorm(resid(lmod))

#residuals from mcmc









