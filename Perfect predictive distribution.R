library(utils)


url <- "https://d3c33hcgiwev3.cloudfront.net/_25ad8f0154906d5d83efc640647beb4b_cookies.dat?Expires=1710028800&Signature=HfKqQSYa4gxWZWOEwyDtA-wf~WkqiscD2hry5gA0ElrCzSwtX~4f6R6tBLWDa7wF~xVsP8MI2xMiiT~sVc47G-e9XLMmeeDy9kwMuDkYwYtU4F5CnENwdB2-yf8Ta3OAQT1KcQt4Sbkj34Az7~ibdYjp6dKxQPB9vH5RRCHx~~0_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"

dat <- read.table(url, header = TRUE, sep = " ")

library("rjags")

mod_string = " model {
for (i in 1:length(chips)) {
chips[i] ~ dpois(lam[location[i]])
}
for (j in 1:max(location)) {
lam[j] ~ dgamma(alpha, beta)
}
alpha = mu^2 / sig^2
beta = mu / sig^2
mu ~ dgamma(2.0, 1.0/5.0)
sig ~ dexp(1.0)
} "
set.seed(113)
data_jags = as.list(dat)
params = c("lam", "mu", "sig")
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)

pm_params = colMeans(mod_csim)

yhat = rep(pm_params[1:5], each=30)
resid = dat$chips - yhat
plot(resid)

plot(jitter(yhat), resid)

var(resid[yhat<7])

var(resid[yhat>11])

## location level residuals
lam_resid = pm_params[1:5] - pm_params["mu"]
plot(lam_resid)
abline(h=0, lty=2)

summary(mod_sim)

#posterior Predictive
n_sim = nrow(mod_csim)

alpha = mod_csim[,"mu"]^2/mod_csim[,"sig"]^2
beta = mod_csim[,"mu"]/mod_csim[,"sig"]^2

lam_pred = rgamma(n=n_sim, shape=alpha, rate=beta)

hist(lam_pred)

mean(lam_pred > 11)

y_pred = rpois(n=n_sim, lambda=lam_pred)

hist(y_pred)

mean(y_pred < 7)

hist(dat$chips)

y_pred1 = rpois(n=n_sim, lambda=mod_csim[,"lam[1]"])
hist(y_pred1)

mean(y_pred1 < 7)
