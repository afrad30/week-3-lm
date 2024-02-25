install.packages("car")

library("car")
data("Leinhardt")
?Leinhardt
head(Leinhardt)
str(Leinhardt)
pairs(Leinhardt)

plot(income ~ infant, data = Leinhardt)
hist(Leinhardt$income)

#create a new variable to resuce the variability in data

Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

plot(loginfant ~ logincome, data = Leinhardt)

lmod=lm(loginfant ~ logincome, data=Leinhardt)
summary(lmod)


# jags, add oil covariate
dat=na.omit(Leinhardt)
library(rjags)


mod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$loginfant, 
                  n=nrow(dat), 
                  log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), 
                  data=data1_jags, inits=inits1, n.chains=3)

update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim)) # combine multiple chains

summary(mod1_csim)

X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income, data1_jags$is_oil)

head(X)

(pm_params1 = colMeans(mod1_csim)) # posterior mean

yhat1 = drop(X %*% pm_params1[1:3])
resid1 = data1_jags$y - yhat1
plot(resid1) # against data index





