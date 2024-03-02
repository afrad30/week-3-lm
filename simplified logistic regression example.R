library("MASS")
data("OME")

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)

mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")

X = model.matrix(mod_glm)# -1 removes the column of 1s for the intercept
X
library("rjags")
library("coda")

mod_string3 = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b[1] + b[2]*Age[i] + b[3]*OMElow[i] + b[4]*Loud[i] + b[5]*Noiseincoherent[i]
	}
	
	for (j in 1:5) {
		b[j] ~ dnorm(0.0, 1.0/5.0^2)
	}
	
} "

data_jags3 = as.list(as.data.frame(X))
data_jags3$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags3$n = dat$Trials
str(data_jags3)# make sure that all variables have the same number of observations (712).

params3 = c("b")

mod3 = jags.model(textConnection(mod_string3), data=data_jags3, n.chains=4)

update(mod3, 1e3)

mod1_sim3 = coda.samples(model=mod3,
                         variable.names = params3,
                         n.iter=5e3)
summary(mod1_sim3)

mod1_csim3 = as.mcmc(do.call(rbind, mod1_sim3))


#r= raftery.diag(mod1_csim3)

# convergence diagnostics
#plot(mod1_sim3, ask=TRUE)

#gelman.diag(mod1_sim3)
#autocorr.diag(mod1_sim3)
#autocorr.plot(mod1_sim3)
#effectiveSize(mod1_sim3)

## calculate DIC
#dic1 = dic.samples(mod3, n.iter=1e3)
#dic1


#par(mfrow=c(3,2))
#densplot(mod1_csim3[,1:6], xlim=c(-3.0, 3.0))

colnames(X) # variable names

pm_coef = colMeans(mod1_csim3)
pm_coef

pm_Xb = X %*% pm_coef

phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)

tab0.7 = table(phat > 0.7, (dat$Correct / dat$Trials) > 0.7)
sum(diag(tab0.7)) / sum(tab0.7)

X
pm_coef
pm_Xb
