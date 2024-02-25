library("rjags")

library("car")
data("Leinhardt")
dat=na.omit(Leinhardt)

#create a new variable to resuce the variability in data

Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

mod2_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt( 1.0 / prec )
} "


set.seed(73)
data2_jags = list(y=dat$loginfant,n=nrow(dat), 
                  log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))
data2_jags$is_oil

params2 = c("b", "sig")

inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
# 3: This specifies the number of random values to generate

mod2 = jags.model(textConnection(mod2_string), 
                  data=data2_jags, inits=inits2, n.chains=3)

update(mod2, 1e3) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)

mod2_csim = as.mcmc(do.call(rbind, mod2_sim)) # combine multiple chains

X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income, data1_jags$is_oil)
head(X)
(pm_params1 = colMeans(mod1_csim)) # posterior mean
