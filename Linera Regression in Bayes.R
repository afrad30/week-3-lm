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

#plot(loginfant ~ logincome, data = Leinhardt)

#lmod=lm(loginfant ~ logincome, data=Leinhardt)
#summary(lmod)


# jags, add oil covariate
dat=na.omit(Leinhardt)
library(rjags)


mod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i]
    }
    
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$loginfant, 
                  n=nrow(dat), 
                  log_income=dat$logincome)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), 
                  data=data1_jags, inits=inits1, n.chains=3)

update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)

formcmc=do.call(rbind, mod1_sim)

mod1_csim = as.mcmc(formcmc) # combine multiple chains

autocorr.diag(mod1_csim)

summary(mod1_csim)

#excercise part

X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income)

#head(X)

(pm_params1 = colMeans(mod1_csim)) # posterior mean

yhat1 = drop(X %*% pm_params1[1:2])
resid1 = data1_jags$y - yhat1
plot(resid1) # against data index

plot(yhat1, resid1)

qqnorm(resid1)



