data("PlantGrowth")
?PlantGrowth
#30 experiment, 2 vairable weight of the plant & group of plant
# group levels are ctrl, trt1, trt2 

boxplot(weight ~ group, data = PlantGrowth)

lmod = lm(weight ~ group, data=PlantGrowth)
summary(lmod)

anova(lmod)


library("rjags")

mod_string1 = "
model {
    for (i in 1:N) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)  # Prior for group means
        prec[j] ~ dgamma(0.01, 0.01)    # Prior for group precisions
    }
    
    sig[1] = sqrt(1.0 / prec[1])
    sig[2] = sqrt(1.0 / prec[2])
    sig[3] = sqrt(1.0 / prec[3])
} "

set.seed(82)

str(PlantGrowth)

PlantGrowth$groupNum = as.numeric(factor(PlantGrowth$group))
data_jags1 = list(y = PlantGrowth$weight, grp = PlantGrowth$groupNum, N = nrow(PlantGrowth))


params1 = c("mu", "sig")

inits1 = function() {
  list(mu = rnorm(3, 0, 100), prec = rep(1, 3))
}

mod1 = jags.model(textConnection(mod_string1), data = data_jags1, inits = inits1, n.chains = 3)

update(mod1, 1000)  # Burn-in

mod_sim1 = coda.samples(model = mod1, variable.names = params1, n.iter = 5000)

mod_csim = as.mcmc(do.call(rbind, mod_sim1)) # combined chains

summary(mod_sim1)

dic2=dic.samples(mod1, n.iter = 100000)
# Convergence

plot(mod_sim1)

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

summary(mod_sim)

HPDinterval(mod_csim)

mean(mod_csim[,3] > mod_csim[,1])

mean(mod_csim[,3] > 1.1*mod_csim[,1])
