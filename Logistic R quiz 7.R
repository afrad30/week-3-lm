X = model.matrix(mod_glm)[,-1]# -1 removes the column of 1s for the intercept

mod_string3 = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + 0.01892*Age[i] -0.23848*OMElow[i] + 0.17102*Loud[i] + 1.57788*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags3 = as.list(as.data.frame(X))
data_jags3$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags3$n = dat$Trials
str(data_jags3)

params3 = c("b")

mod3 = jags.model(textConnection(mod_string3), data=data_jags3, n.chains=3)

update(mod3, 1e3)

mod1_sim3 = coda.samples(model=mod3,
                         variable.names = params3,
                         n.iter=5e3)

mod1_csim3 = as.mcmc(do.call(rbind, mod1_sim3))
