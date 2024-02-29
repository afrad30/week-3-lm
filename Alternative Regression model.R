

mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
        logit(p[i]) = int + b[1]*gravity[i] + b[2]*cond[i] + b[3]*calc[i]
    }
        int ~ dnorm(0.0, 1.0/25.0)
    for (j in 1:3) {
        b[j] ~ dnorm(0.0, 1.0/25.0) # noninformative for logistic regression
    }
} "

mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)

update(mod2, 1e3)

mod2_sim = coda.samples(model=mod2,
                        variable.names=params,
                        n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

#plot(mod2_sim, ask=TRUE)

gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)
#autocorr.plot(mod2_sim)
effectiveSize(mod2_sim)

dic2 = dic.samples(mod2, n.iter=1e3)
#dic2

#summary(mod2_sim)

pm_coef = colMeans(mod2_csim)

pm_Xb = pm_coef["int"] + X[,c(1,4,6)] %*% pm_coef[1:3]

phat = 1.0 / (1.0 + exp(-pm_Xb))

#plot(phat, jitter(dat$r))
tab0.5 = table(phat > 0.5, data_jags$y)
sum(diag(tab0.5)) / sum(tab0.5)
tab0.3 = table(phat > 0.3, data_jags$y)
sum(diag(tab0.3)) / sum(tab0.3)
