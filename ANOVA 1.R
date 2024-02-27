data("PlantGrowth")
?PlantGrowth
#30 experiment, 2 vairable weight of the plant & group of plant
# group levels are ctrl, trt1, trt2 

boxplot(weight ~ group, data = PlantGrowth)

lmod = lm(weight ~ group, data=PlantGrowth)
summary(lmod)

anova(lmod)

library("rjags")

data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))


mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec) ### > data_jags$grp[29]:[1] 3; > data_jags$grp[1:10]: [1] 1 1 1 1 1 1 1 1 1 1
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6) # mu follow 3 normal distribution,& get mu[1],m[2],m[3]
    }
    
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt( 1.0 / prec )
} "

set.seed(82)

str(PlantGrowth)


params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains

summary(mod_sim)

dic1=dic.samples(mod, n.iter = 100000)

# Convergence

#plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

#posterior mean

pm_params = colMeans(mod_csim)

yhat = pm_params[1:3][data_jags$grp]
resid = data_jags$y - yhat
plot(resid)
plot(yhat, resid)

#find each group residual variance

yhat0 = pm_params[1][data_jags$grp[1:10]]
resid0 = data_jags$y[1:10] - yhat0
plot(resid0)
plot(yhat0, resid0)

yhat1 = pm_params[2][data_jags$grp[1:10]]
resid1 = data_jags$y[11:20] - yhat1
plot(resid1)
plot(yhat1, resid1)

yhat2 = pm_params[3][data_jags$grp[1:10]]
resid2 = data_jags$y[21:30] - yhat2
plot(resid2)
plot(yhat2, resid2)

coefficients(lmod)



hpd=HPDinterval(mod_csim)

mean(mod_csim[,3] > mod_csim[,1])

mean(mod_csim[,3] > 1.1*mod_csim[,1])

mu3_samples = mod_csim[, "mu[3]"]
mu1_samples = mod_csim[, "mu[1]"]

mu_diff = mu3_samples - mu1_samples

# Calculating the 95% HPD interval for the differences
hpd_diff = HPDinterval(as.mcmc(mu_diff))

# Displaying the HPD interval
print(hpd_diff)

mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)
