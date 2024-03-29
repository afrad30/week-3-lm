library("MASS")
data("OME")

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
dat

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )

mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
plot(mod_glm)
summary(mod_glm)

plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)

X = model.matrix(mod_glm)[,-1]# -1 removes the column of 1s for the intercept
X
library("rjags")
library("coda")

mod_string3 = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags3 = as.list(as.data.frame(X))
data_jags3$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags3$n = dat$Trials
str(data_jags3)# make sure that all variables have the same number of observations (712).

params3 = c("b0","b")

mod3 = jags.model(textConnection(mod_string3), data=data_jags3, n.chains=3)

update(mod3, 1e3)

mod1_sim3 = coda.samples(model=mod3,
                         variable.names = params3,
                         n.iter=5e3)
summary(mod1_sim3)

mod1_csim3 = as.mcmc(do.call(rbind, mod1_sim3))

summary(mod1_csim3)

r= raftery.diag(mod1_csim3)

## convergence diagnostics
plot(mod1_sim3, ask=TRUE)

gelman.diag(mod1_sim3)
autocorr.diag(mod1_sim3)
autocorr.plot(mod1_sim3)
effectiveSize(mod1_sim3)

## calculate DIC
dic1 = dic.samples(mod3, n.iter=1e3)
dic1

summary(mod1_csim3)

par(mfrow=c(3,2))
densplot(mod1_csim[,1:6], xlim=c(-3.0, 3.0))

colnames(X) # variable names

(pm_coef = colMeans(mod2_csim))

pm_Xb = pm_coef["int"] + X[,c(1,4,6)] %*% pm_coef[1:3]

phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)
