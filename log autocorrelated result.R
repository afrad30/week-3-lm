library("car")
data("Leinhardt")


#create a new variable to reduce the variability in data

# a data set about mortality rate of different countries. scale reduction of income by log, to reduce inconsistency  

Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

lmod=lm(loginfant ~ logincome, data=Leinhardt)

plot(lmod)

lmod=lm(infant ~ income, data=Leinhardt)

plot(lmod)

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

inits1 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)

update(mod1, 1000) # burn-in

mod1_sim = coda.samples(mod1,c("b", "sig"), n.iter=5000)

formcmc=do.call(rbind, mod1_sim)

mod1_csim = as.mcmc(formcmc) # combine multiple chains

# convergence
gelman.diag(mod1_csim)
autocorr.diag(mod1_csim)
effectiveSize(mod1_csim)
summary_output=summary(mod1_csim)
summary_output$statistics[1:4]


#residuals

lmod=lm(infant ~ income, data=Leinhardt)

plot(lmod) # plot 4 diag
plot(resid(lmod))

logmod=lm(loginfant ~ logincome, data=Leinhardt)
plot(resid(logmod))

plot(predict(lmod), resid(lmod))

plot(predict(logmod), resid(logmod))

qqnorm(resid(lmod))

#for posterir mean,  create a data table of logincome, or the predictor 
# & residuals
X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income, data1_jags$is_oil)

head(X) #creat a matrix of (bnot, xi, zi) 

pm_params1 = colMeans(mod1_csim) # posterior mean of every column, in table form

yhat = drop(X %*% pm_params1[1:3])# y value is equal bnot+b1xi+b2zi# matrix mutiplication, bnot+ b1*INCOME+SIG
yhat
yhat1 = drop(X %*% summary_output$statistics[1:3])
yhat1
resid1 = data1_jags$y - yhat
resid2 = data1_jags$y - yhat1
plot(resid1) # against data index
plot(resid2)
plot(yhat, resid1)
qqnorm(resid2)


